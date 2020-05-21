package authentication

import algos.hash.HashFunction
import cats.Applicative
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger

trait Auth[F[_]] {
  def enter: F[Unit]
}

object Auth {
  def apply[F[_]: Logger](db: DB[F])(implicit F: Sync[F]): Auth[F] = new Auth[F] {

    private val hash: HashFunction = HashFunction.sha1

    override def enter: F[Unit] =
      (for {
        _          <- Logger[F].info(s"Enter your login:")
        inputLogin <- F.delay(scala.io.StdIn.readLine())
        contains   <- db.contains(inputLogin.getBytes)
        res <- if (contains) checkPass(inputLogin).map(_ => true)
               else Logger[F].info(s"Unknown user. Please, do registration") >> reg(inputLogin).map(_ => false)
      } yield res).ifM(Applicative[F].unit, enter)

    private def reg(l: String): F[Unit] =
      for {
        _       <- Logger[F].info(s"Init registration operation.")
        _       <- Logger[F].info(s"Enter new password:")
        newPass <- F.delay(scala.io.StdIn.readLine())
        _       <- db.put(l.getBytes(), hash.make(newPass).getBytes)
        _       <- Logger[F].info(s"Registration finished!")
      } yield ()

    private def checkPass(login: String): F[Unit] =
      for {
        _             <- Logger[F].info(s"Enter your password:")
        inputPassword <- F.delay(scala.io.StdIn.readLine())
        realPassword  <- db.get(login.getBytes)
        res <- if (checkPass(inputPassword, realPassword)) Logger[F].info(s"Password is correct.")
              else Logger[F].info(s"Password is incorrect.") >> checkPass(login)
      } yield res

    private def checkPass(input: String, dbHash: Array[Byte]): Boolean =
      hash.make(input).getBytes.sameElements(dbHash)
  }
}
