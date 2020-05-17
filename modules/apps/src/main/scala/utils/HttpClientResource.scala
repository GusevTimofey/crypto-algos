package utils

import java.util.concurrent.{ Executors, ThreadFactory }

import cats.effect.{ ConcurrentEffect, Resource }
import com.google.common.util.concurrent.ThreadFactoryBuilder
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.{ ExecutionContext, ExecutionContextExecutor }

object HttpClientResource {
  def create[F[_]: ConcurrentEffect]: Resource[F, Client[F]] = {
    val tf: ThreadFactory = new ThreadFactoryBuilder()
      .setNameFormat("http-api-client-thread-pool-%d")
      .setDaemon(false)
      .setPriority(Thread.NORM_PRIORITY)
      .build()
    val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(
      Executors.newFixedThreadPool(3, tf)
    )
    BlazeClientBuilder[F](ec).resource
  }
}
