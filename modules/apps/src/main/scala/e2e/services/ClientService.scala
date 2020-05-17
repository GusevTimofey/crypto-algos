package e2e.services

import cats.effect.Sync
import e2e._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.{ Method, Request, Uri }

trait ClientService[F[_]] {
  def postHttpDSL(input: HttpDSL): F[Unit]
  def postMsg(msg: Message): F[Unit]
}

object ClientService {
  def apply[F[_]: Sync](client: Client[F], url: String): ClientService[F] = new ClientService[F] {
    override def postHttpDSL(input: HttpDSL): F[Unit] =
      client.expect(Request[F](Method.POST, Uri.unsafeFromString(url + "/DH")).withEntity(input.asJson))

    override def postMsg(msg: Message): F[Unit] =
      client.expect(Request[F](Method.POST, Uri.unsafeFromString(url + "/msg")).withEntity(msg.asJson))
  }
}
