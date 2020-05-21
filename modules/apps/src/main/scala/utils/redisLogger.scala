package utils

import dev.profunktor.redis4cats.effect.Log
import io.chrisdavenport.log4cats.Logger

object redisLogger {
  implicit def rLogger[F[_]](implicit log: Logger[F]): Log[F] =
    new Log[F] {
      override def info(msg: => String): F[Unit] = log.info(msg)

      override def error(msg: => String): F[Unit] = log.error(msg)
    }
}
