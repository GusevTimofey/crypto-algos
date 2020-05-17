package authentication

import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.functor._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

object Application extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    Slf4jLogger
      .create[IO]
      .flatMap(implicit log => DB[IO]("/Users/timofeygusev/Desktop/qwe").use(db => Auth[IO](db).enter))
      .as(ExitCode.Success)
}
