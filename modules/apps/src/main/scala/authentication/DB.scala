package authentication

import cats.effect.{ Concurrent, ContextShift, Resource, Sync }
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import org.rocksdb.{ Options, ReadOptions, RocksDB, WriteBatch, WriteOptions }
import utils.RedisResource

trait DB[F[_]] {
  def get(k: Array[Byte]): F[Array[Byte]]
  def put(k: Array[Byte], v: Array[Byte]): F[Unit]
  def contains(k: Array[Byte]): F[Boolean]
}

object DB {

  def rocksDb[F[_]: Logger: Sync](path: String): Resource[F, DB[F]] =
    Resource
      .fromAutoCloseable(Sync[F].delay(RocksDB.open(new Options().setCreateIfMissing(true).prepareForBulkLoad(), path)))
      .map { db =>
        new DB[F] {
          override def get(k: Array[Byte]): F[Array[Byte]] = doReq(k).handleError(_ => Array.emptyByteArray)

          override def put(k: Array[Byte], v: Array[Byte]): F[Unit] =
            (for {
              wb <- Resource.fromAutoCloseable(new WriteBatch().pure[F])
              wo <- Resource.fromAutoCloseable(new WriteOptions().pure[F])
            } yield (wb, wo)).use {
              case (batch, options) =>
                batch.put(k, v)
                db.write(options, batch).pure[F]
            }

          override def contains(k: Array[Byte]): F[Boolean] = doReq(k).map(_ => true).handleError(_ => false)

          private def doReq(k: Array[Byte]): F[Array[Byte]] =
            Resource
              .fromAutoCloseable(new ReadOptions().setSnapshot(db.getSnapshot).pure[F])
              .use(db.get(_, k).pure[F])
              .flatMap {
                case v if v.isEmpty => new Exception("null value").raiseError
                case v              => v.pure[F]
              }
        }
      }

  def redis[F[_]: Concurrent: ContextShift]: Resource[F, DB[F]] =
    RedisResource.create[F].map { cmd =>
      new DB[F] {
        override def get(k: Array[Byte]): F[Array[Byte]] =
          cmd.get(new String(k)).map(_.getOrElse("")).map(_.getBytes)

        override def put(k: Array[Byte], v: Array[Byte]): F[Unit] =
          cmd.set(new String(k), new String(v))

        override def contains(k: Array[Byte]): F[Boolean] =
          cmd.exists(new String(k))
      }
    }
}
