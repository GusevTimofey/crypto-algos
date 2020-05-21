package utils

import cats.effect.{ Concurrent, ContextShift, Resource }
import cats.syntax.functor._
import dev.profunktor.redis4cats.algebra.RedisCommands
import dev.profunktor.redis4cats.connection.{ RedisClient, RedisURI }
import dev.profunktor.redis4cats.domain.RedisCodec
import dev.profunktor.redis4cats.effect.Log
import dev.profunktor.redis4cats.interpreter.{ Redis => RedisCmd }
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

object RedisResource {
  def create[F[_]: Concurrent: ContextShift]: Resource[F, RedisCommands[F, String, String]] =
    for {
      implicit0(log: Log[F]) <- Resource.liftF(Slf4jLogger.create[F].map(redisLogger.rLogger(_)))
      url                    <- Resource.liftF(RedisURI.make[F]("redis://localhost:6379"))
      client                 <- RedisClient[F](url)
      cmd                    <- RedisCmd[F, String, String](client, RedisCodec.Utf8)
    } yield cmd

}
