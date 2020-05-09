package algos.digital.signature

import java.io.File

import algos.common.FileReader
import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.functor._

object Application extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    (for {
      _                <- IO(println(s"Enter source file path:"))
      pathStr          <- IO(scala.io.StdIn.readLine())
      path             <- IO(new File(pathStr).toPath)
      reader           = FileReader[IO]
      inputData        <- reader.readFrom(path)
      _                <- IO(println(s"File data was read"))
      digitalSignature = DS.apply
      pairs            = digitalSignature.formKeysPair(1024)
      ciphered         = digitalSignature.cipher(inputData, pairs._2)
      deciphered       = digitalSignature.decipher(ciphered, pairs._1)
      _                <- IO(println(s"Algorithm finished. Result is: $deciphered"))
    } yield ()).as(ExitCode.Success)
}
