package algos.digital.signature

import java.math.BigInteger
import java.security.SecureRandom

import algos.digital.signature.DS.{ CipherResult, PrivateKey, PublicKey }
import cats.instances.bigInt._
import cats.syntax.eq._

import scala.util.Random

trait DS {
  def cipher(m: BigInt, privateKey: PrivateKey): CipherResult
  def decipher(cipherResult: CipherResult, publicKey: PublicKey): Boolean
  def formKeysPair(bitSize: Int): (PublicKey, PrivateKey)
}

object DS {
  def apply: DS = new RSA

  private class RSA extends DS {
    final val random: SecureRandom = new SecureRandom()
    override def cipher(m: BigInt, privateKey: PrivateKey): CipherResult = {
      val s: BigInt = m.modPow(privateKey.d, privateKey.n)
      val res       = CipherResult(s, m)
      res
    }
    override def decipher(cipherResult: CipherResult, publicKey: PublicKey): Boolean = {
      val mDeciphered = cipherResult.s.modPow(publicKey.e, publicKey.n)
      val res         = mDeciphered === cipherResult.m
      println(s"Decipher result: $res")
      res
    }

    def formKeysPair(bitSize: Int): (PublicKey, PrivateKey) = {
      val (p: BigInt, q: BigInt) = generateCoprimeNumbers(bitSize)
      val n: BigInt              = p * q
      val fi: BigInt             = (p - 1) * (q - 1)
      val e: BigInt              = generateCoprimeWith(generatePrime(bitSize / 2), fi, bitSize)
      val d: BigInt              = e.modInverse(fi)
      PublicKey(e, n) -> PrivateKey(d, n)
    }

    def generateCoprimeNumbers(bitSize: Int): (BigInt, BigInt) =
      generateCoprime(generatePrime(bitSize), generatePrime(bitSize), bitSize)

    @scala.annotation.tailrec
    final def generateCoprimeWith(e: BigInt, fi: BigInt, bitSize: Int): BigInt =
      if (e < fi && fi.gcd(e) === BigInt(1)) e
      else generateCoprimeWith(generatePrime(generateNextRandomSize(bitSize)), fi, bitSize)

    @scala.annotation.tailrec
    final def generateCoprime(p: BigInt, q: BigInt, bitSize: Int): (BigInt, BigInt) =
      if (p =!= q && p.gcd(q) === BigInt(1)) p -> q
      else generateCoprime(generatePrime(bitSize), generatePrime(bitSize), bitSize)

    def generateNextRandomSize(max: Int): Int = {
      @scala.annotation.tailrec
      def loop(next: Int): Int =
        if (next > 2 && next < max) next
        else loop(Random.nextInt(max))
      loop(Random.nextInt(max))
    }

    def generatePrime(bitSize: Int): BigInt = BigInt.apply(BigInteger.probablePrime(bitSize, random))
  }

  final case class PublicKey(e: BigInt, n: BigInt)
  final case class PrivateKey(d: BigInt, n: BigInt)
  final case class CipherResult(s: BigInt, m: BigInt)
}
