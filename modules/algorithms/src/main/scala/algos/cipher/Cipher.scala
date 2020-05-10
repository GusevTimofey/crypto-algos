package algos.cipher

import algos.common.utils._
import com.google.common.primitives.Ints
import mouse.all._

trait Cipher {
  def encrypt(input: Array[Byte]): Array[Byte]
  def decrypt(output: Array[Byte]): Array[Byte]
}

object Cipher {

  def blowFish(key: String): BlowFishCipher = {
    val cipher = new BlowFishCipher
    cipher.initKeys(key)
    cipher
  }

  final class BlowFishCipher extends Cipher {

    val mod32: Long = Math.pow(2, 32).toLong

    val pMutable: Array[Long] = bfP

    val sMutable: Array[Array[Long]] = bfS

    override def encrypt(input: Array[Byte]): Array[Byte] = Array.emptyByteArray

    override def decrypt(output: Array[Byte]): Array[Byte] = Array.emptyByteArray

    def initKeys(key: String): Unit = {
      val keyBytes: Array[Byte] = key.getBytes
      if (keyBytes.length < 56 && keyBytes.length > 4) {
        val requiredCycles: Int = 72 - keyBytes.length
        val pKeys =
          (0 until requiredCycles)
            .foldLeft(keyBytes) { case (acc, i) => acc.appended(keyBytes(i % keyBytes.length)) }
            .grouped(4)
            .toArray
            .view
            .map(bytes => Ints.fromByteArray(bytes) |> asUInt)
            .zip(pMutable)
            .map { case (i, l) => (i ^ l) |> asUInt }
            .toArray

        var xr: Long = 0L
        var xl: Long = 0L

        for (i <- (0 until 18) by 2) {
          val (xli, xri) = cipher(xl, xr, pKeys, bfS)
          xr = xri
          xl = xli
          pMutable(i) = xl
          pMutable(i + 1) = xr
        }

        for (i <- 0 until 4)
          for (k <- (0 until 256) by 2) {
            val (xli, xri) = cipher(xl, xr, pKeys, bfS)
            xr = xri
            xl = xli
            sMutable(i)(k) = xl
            sMutable(i)(k + 1) = xr
          }

      } else throw new RuntimeException(s"Unsupported user key length! Got: ${keyBytes.length} key bits.")
    }

    def cipher(left: Long, right: Long, P: Array[Long], S: Array[Array[Long]]): (Long, Long) = {
      var xl: Long = left
      var xr: Long = right
      xl = (xl ^ P(0)) |> asUInt
      for (i <- (0 until 16) by 2) {
        xr = (xr ^ ((f(xl, S) ^ P(i)) |> asUInt)) |> asUInt
        xl = (xl ^ ((f(xr, S) ^ P(i + 1)) |> asUInt)) |> asUInt
      }
      xr = (xr ^ P(17)) |> asUInt
      xr -> xl
    }

    def decipher(left: Long, right: Long, P: Array[Int], S: Array[Array[Long]]): (Long, Long) = {
      var xl: Long = left
      var xr: Long = right
      xl = (xl ^ P(17)) |> asUInt
      for (i <- (16 until 0) by -2) {
        xr = (xr ^ ((f(xl, S) ^ P(i)) |> asUInt)) |> asUInt
        xl = (xl ^ ((f(xr, S) ^ P(i - 1)) |> asUInt)) |> asUInt
      }
      xr = (xr ^ P(0)) |> asUInt
      xr -> xl
    }

    def f: (Long, Array[Array[Long]]) => Long = (block: Long, S: Array[Array[Long]]) => {
      val a: Int = ((block & 0xff000000) >> 24).toInt
      val b: Int = ((block & 0x00ff0000) >> 16).toInt
      val c: Int = ((block & 0x0000ff00) >> 8).toInt
      val d: Int = (block & 0x000000ff).toInt
      (((((S(0)(a) + S(1)(b)) % mod32) ^ S(2)(c)) |> asUInt) + S(3)(d)) % mod32
    }

  }

}
