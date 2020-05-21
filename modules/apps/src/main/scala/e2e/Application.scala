package e2e

import authentication.{ Auth, DB }
import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.functor._
import e2e.services.{ ClientService, ServerService }
import fs2.concurrent.Queue
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import utils.HttpClientResource

object ApplicationBob extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    (for {
      client <- HttpClientResource.create[IO]
      redis  <- DB.redis[IO]
    } yield client -> redis).use {
      case (client, db) =>
        Slf4jLogger
          .create[IO]
          .flatMap { implicit log =>
            for {
              _          <- Auth(db).enter
              inputQueue <- Queue.bounded[IO, HttpDSL](10)
              messages   <- Queue.bounded[IO, e2e.Message](500)
              ss         = ServerService[IO](inputQueue, messages, 8081)
              cs         = ClientService(client, "http://localhost:8080")
              dh         = DHProgram[IO](cs, inputQueue, messages)
              _          <- (ss.run concurrently dh.bobScenario).compile.drain
            } yield ()
          }
    }.as(ExitCode.Success)
}

object ApplicationAlice extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    (for {
      client <- HttpClientResource.create[IO]
      redis  <- DB.redis[IO]
    } yield client -> redis).use {
      case (client, db) =>
        Slf4jLogger
          .create[IO]
          .flatMap { implicit log =>
            for {
              _          <- Auth(db).enter
              inputQueue <- Queue.bounded[IO, HttpDSL](10)
              messages   <- Queue.bounded[IO, e2e.Message](500)
              ss         = ServerService[IO](inputQueue, messages, 8080)
              cs         = ClientService(client, "http://localhost:8081")
              dh         = DHProgram[IO](cs, inputQueue, messages)
              _          <- (ss.run concurrently dh.aliceScenario).compile.drain
            } yield ()
          }
      }
      .as(ExitCode.Success)
}
