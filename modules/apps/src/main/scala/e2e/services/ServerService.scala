package e2e.services

import cats.data.Kleisli
import cats.effect.{ ConcurrentEffect, Timer }
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ Defer, MonadError }
import e2e._
import fs2.Stream
import fs2.concurrent.Queue
import io.chrisdavenport.log4cats.Logger
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.{ HttpRoutes, _ }

trait ServerService[F[_], S[_[_], _]] {
  def run: S[F, Unit]
}

object ServerService {
  def apply[F[_]: Defer: MonadError[*[_], Throwable]: ConcurrentEffect: Timer: Logger](
    inputQueue: Queue[F, HttpDSL],
    messages: Queue[F, e2e.Message],
    port: Int
  ): ServerService[F, Stream] = new ServerService[F, Stream] {
    override def run: Stream[F, Unit] =
      BlazeServerBuilder[F]
        .bindHttp(port, "localhost")
        .withHttpApp(httpApp)
        .serve
        .void

    private val routes: HttpRoutes[F] = HttpRoutes.of[F] {
      case req @ POST -> Root / "DH" =>
        implicit val decoderHttp: EntityDecoder[F, HttpDSL] = jsonOf[F, HttpDSL]
        for {
          value <- req.as[HttpDSL]
          _     <- Logger[F].info(s"server $port received $value")
          _     <- inputQueue.enqueue1(value)
        } yield Response[F]()

      case msg @ POST -> Root / "msg" =>
        implicit val decoderMessage: EntityDecoder[F, e2e.Message] = jsonOf[F, e2e.Message]
        for {
          value <- msg.as[e2e.Message]
          _     <- messages.enqueue1(value)
        } yield Response[F]()
    }

    private val httpApp: Kleisli[F, Request[F], Response[F]] = Router("" -> routes).orNotFound
  }
}
