package algos.common

import java.math.BigInteger

import cats.instances.bigInt._
import cats.syntax.eq._
import java.util.Random
import scala.util.{ Random => R }

object utils {

  val ZeroByteBits: String = "00000000"

  def asUInt(l: Long): Long = l & 0xFFFFFFFFL
  def asUInt(i: Int): Long  = i & 0xFFFFFFFFL

  def generatePrime(bitSize: Int, random: Random): BigInt = BigInt.apply(BigInteger.probablePrime(bitSize, random))

  @scala.annotation.tailrec
  final def generateCoprimeWith(e: BigInt, fi: BigInt, bitSize: Int, random: Random): BigInt =
    if (e < fi && fi.gcd(e) === BigInt(1)) e
    else generateCoprimeWith(generatePrime(generateNextRandomSize(bitSize), random), fi, bitSize, random)

  def generateNextRandomSize(max: Int): Int = {
    @scala.annotation.tailrec
    def loop(next: Int): Int =
      if (next > 2 && next < max) next
      else loop(R.nextInt(max))
    loop(R.nextInt(max))
  }

}
