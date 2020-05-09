package algos.prng

import java.io.File

import algos.common.BitsLike.instances._
import algos.common.BitsLike.ops._
import algos.common.FileReader
import cats.effect.{ ExitCode, IO, IOApp }
import fs2.Stream
import cats.syntax.functor._

object Application extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    (for {
      _       <- IO(println(s"Start rng."))
      _       <- IO(println(s"Enter destination file path:"))
      pathStr <- IO(scala.io.StdIn.readLine())
      seed    = System.nanoTime() * 8682522807148012L * 181783497276652981L
      rng     = RNG(seed)
      file    <- IO(new File(pathStr))
      _       <- IO(file.createNewFile())
      stream  = Stream.emits(0 to 1000000).evalMap(_ => IO(rng.next().asBits))
      reader  = FileReader[IO]
      _       <- reader.writeTo(file.toPath, stream)
      _       <- IO(println(s"Finish rng."))
    } yield ()).as(ExitCode.Success)
}
