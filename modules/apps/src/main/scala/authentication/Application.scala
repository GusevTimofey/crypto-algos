package authentication

import algos.hash.HashFunction
import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.functor._
import cats.syntax.flatMap._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

object Application extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    Slf4jLogger
      .create[IO]
      .flatMap(implicit log => DB.apply[IO]("/Users/timofeygusev/Desktop/qwe").use(work))
      .as(ExitCode.Success)

  private val hash: HashFunction = HashFunction.sha1

  private def work(db: DB[IO]): IO[Unit] =
    for {
      _        <- IO(println(s"Enter login:"))
      login    <- IO(scala.io.StdIn.readLine())
      contains <- db.contains(login.getBytes)
      _        <- if (contains) log(db, login) else reg(db, login) >> work(db)
    } yield ()

  private def reg(db: DB[IO], log: String): IO[Unit] =
    for {
      _    <- IO(println(s"Enter new password:"))
      pass <- IO(scala.io.StdIn.readLine())
      _    <- db.put(log.getBytes(), hash.make(pass).getBytes)
      _    <- IO(println(s"Registration finished!"))
    } yield ()

  private def log(db: DB[IO], logging: String): IO[Unit] =
    for {
      _        <- IO(println(s"Enter your password:"))
      pass     <- IO(scala.io.StdIn.readLine())
      hashPass <- db.get(logging.getBytes)
      _ <- if (hash.make(pass).getBytes().sameElements(hashPass))
            IO(println(s"You successfully login!")) >> IO(println(s"Now everything is available."))
          else IO(println(s"Incorrect login =(. Try again")) >> log(db, logging)
    } yield ()
}
