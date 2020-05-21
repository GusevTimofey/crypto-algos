package e2e

import java.security.SecureRandom

import algos.cipher.Cipher
import algos.cipher.Cipher.BlowFishCipher
import algos.prng.RNG
import cats.effect.{ Concurrent, Sync }
import cats.syntax.flatMap._
import cats.syntax.functor._
import e2e.services.ClientService
import fs2.Stream
import fs2.concurrent.Queue
import io.chrisdavenport.log4cats.Logger
import algos.common.utils._

trait DHProgram[F[_], S[_[_], _]] {
  def aliceScenario: S[F, Unit]
  def bobScenario: S[F, Unit]
}

object DHProgram {
  def apply[F[_]: Logger: Concurrent](
    cs: ClientService[F],
    inputQueue: Queue[F, HttpDSL],
    messages: Queue[F, e2e.Message]
  )(implicit F: Sync[F]): DHProgram[F, Stream] = new DHProgram[F, Stream] {

    val rng: RNG = RNG(RNG.seed)

    val random: SecureRandom = new SecureRandom()

    override def aliceScenario: Stream[F, Unit] =
      Stream
        .eval(
          for {
            _         <- Logger[F].info(s"Init Alice scenario")
            p: BigInt = pPrime
            g: BigInt = gGen(p)
            _         <- cs.postHttpDSL(PGValues(p, g))
            a         = BigInt(rng.next())
            aBig      = g.modPow(a, p)
            _         <- cs.postHttpDSL(AValue(aBig))
            bBig      <- inputQueue.dequeue1.map(_.asInstanceOf[BValue].B)
            k         = bBig.modPow(a, p)
          } yield k
        )
        .flatMap(k => chat("bob", Cipher.blowFish(k.toString().take(8))))

    override def bobScenario: Stream[F, Unit] =
      Stream
        .eval(
          for {
            _    <- Logger[F].info(s"Init Bob scenario")
            pg   <- inputQueue.dequeue1.map(_.asInstanceOf[PGValues])
            b    = BigInt(rng.next())
            bBig = pg.G.modPow(b, pg.P)
            _    <- cs.postHttpDSL(BValue(bBig))
            aBig <- inputQueue.dequeue1.map(_.asInstanceOf[AValue].A)
            k    = aBig.modPow(b, pg.P)
          } yield k
        )
        .flatMap(k => chat("alice", Cipher.blowFish(k.toString().take(8))))

    private def chat(side: String, blowfish: BlowFishCipher): Stream[F, Unit] = {
      def received: Stream[F, Unit] =
        messages.dequeue
          .evalMap(msg => Logger[F].info(s"$side: ${new String(blowfish.ecbDecipher(msg.bytes))}"))
          .handleErrorWith { err =>
            Stream.eval(Logger[F].info(s"Error has occurred in receive function: ${err.getMessage}")) >> received
          }

      def send: Stream[F, Unit] =
        Stream
          .emit(())
          .covary[F]
          .repeat
          .evalMap(_ => F.delay(scala.io.StdIn.readLine()).flatTap(_ => Logger[F].info("$>")))
          .evalMap(msg => cs.postMsg(e2e.Message(blowfish.ecbEncipher(msg.getBytes))))
          .handleErrorWith { err =>
            Stream.eval(Logger[F].info(s"Error has occurred in send function: ${err.getMessage}")) >> send
          }

      Stream.eval(Logger[F].info(s"Start communication as $side")) >> (received concurrently send)
    }

    @scala.annotation.tailrec
    private def pPrime: BigInt = {
      val probablyP = generatePrime(512, random)
      if (((probablyP - 1) / 2).isProbablePrime(99)) probablyP
      else pPrime
    }

    private def gGen(p: BigInt): BigInt =
      generateCoprimeWith(generatePrime(generateNextRandomSize(512), random), p, 512, random)

  }
}
