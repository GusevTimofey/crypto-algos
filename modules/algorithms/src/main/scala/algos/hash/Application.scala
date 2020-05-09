package algos.hash

import java.io.File

import algos.common.FileReader
import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.functor._

object Application extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    (for {
      _         <- IO(println(s"Enter source file path:"))
      pathStr   <- IO(scala.io.StdIn.readLine())
      path      <- IO(new File(pathStr).toPath)
      reader    = FileReader[IO]
      inputData <- reader.readFrom(path)
      _         <- IO(println(s"File data was read"))
      sha1      = HashFunction.sha1
      hash      = sha1.make(inputData)
      _         <- IO(println(s"Resulted hash is: $hash"))
    } yield ()).as(ExitCode.Success)

}
