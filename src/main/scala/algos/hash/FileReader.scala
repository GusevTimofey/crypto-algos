package algos.hash

import java.nio.file.Path
import java.util.concurrent.{ Executors, ThreadFactory }

import cats.effect.{ Blocker, ContextShift, Sync }
import com.google.common.util.concurrent.ThreadFactoryBuilder
import fs2.io.file
import fs2.{ text, Stream }

import scala.concurrent.{ ExecutionContext, ExecutionContextExecutor }

trait FileReader[F[_]] {
  def readFrom(path: Path): F[String]
  def writeTo(path: Path, data: String): F[Unit]
}

object FileReader {
  def apply[F[_]: Sync: ContextShift]: FileReader[F] = {
    val readerTF: ThreadFactory = new ThreadFactoryBuilder()
      .setNameFormat("file-reader-$d")
      .setDaemon(false)
      .setPriority(Thread.NORM_PRIORITY)
      .build()
    val readerEC: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(readerTF))
    val blocker: Blocker                   = Blocker.liftExecutionContext(readerEC)
    new FileReader[F] {
      override def readFrom(path: Path): F[String] =
        file
          .readAll(path, blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .filter(s => !s.trim.isEmpty && !s.startsWith("//"))
          .compile
          .fold("") { case (acc, str) => acc + str }

      override def writeTo(path: Path, data: String): F[Unit] =
        Stream
          .emit(data)
          .covary[F]
          .through(text.utf8Encode)
          .through(file.writeAll(path, blocker))
          .compile
          .drain

    }
  }
}
