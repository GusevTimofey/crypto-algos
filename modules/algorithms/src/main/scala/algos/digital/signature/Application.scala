package algos.digital.signature

import java.io.File

import algos.common.FileReader
import algos.hash.HashFunction
import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.functor._
import algos.common.BitsLike.instances._
import algos.common.FromBits.instances._
import algos.common.BitsLike.ops._
import algos.common.FromBits.ops._
import algos.common.utils._
import algos.common.types._

object Application extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    (for {
      _                <- IO(println(s"Enter source file path:"))
      pathStr          <- IO(scala.io.StdIn.readLine())
      path             <- IO(new File(pathStr).toPath)
      reader           = FileReader[IO]
      inputData        <- reader.readFrom(path)
      _                <- IO(println(s"File data was read"))
      sha1             = HashFunction.apply
      hash             = sha1.make(inputData)
      digitalSignature = DS.apply
      hashBytes        = hash.flatMap(_.asBits.grouped(8).toList.map(_.liftToByte)).toArray
      pairs            = digitalSignature.formKeysPair(1024)
      ciphered         = digitalSignature.cipher(BigInt(hashBytes), pairs._2)
      deciphered       = digitalSignature.decipher(ciphered, pairs._1)
      _                <- IO(println(s"Algorithm finished. Result is: $deciphered"))
      _                <- IO(println(s"Resulted hash is: $hash"))
    } yield ()).as(ExitCode.Success)
}
