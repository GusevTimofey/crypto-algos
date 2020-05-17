package algos.cipher

import algos.common.utils._
import com.google.common.primitives.{ Ints, Longs }
import mouse.all._

sealed trait Cipher {
  def ecbEncipher(input: Array[Byte]): Array[Byte]
  def ecbDecipher(input: Array[Byte]): Array[Byte]

  def cbcEncipher(input: Array[Byte], iv: Array[Byte]): Array[Byte]
  def cbcDecipher(input: Array[Byte], iv: Array[Byte]): Array[Byte]

  def pcbcEncipher(input: Array[Byte], iv: Array[Byte]): Array[Byte]
  def pcbcDecipher(input: Array[Byte], iv: Array[Byte]): Array[Byte]

  def cfbEncipher(input: Array[Byte], iv: Array[Byte]): Array[Byte]
  def cfbDecipher(input: Array[Byte], iv: Array[Byte]): Array[Byte]
}

object Cipher {

  def blowFish(key: String): BlowFishCipher =
    new BlowFishCipher().initKeys(key)

  final class BlowFishCipher extends Cipher {

    val mod32: Long = Math.pow(2, 32).toLong

    var pMutable: Array[Long] = Array.ofDim(18)

    for (i <- 0 until 18) pMutable(i) = bfP(i)

    val sMutable: Array[Array[Long]] = Array.ofDim(4, 256)

    for (i <- 0 until 4)
      for (k <- (0 until 255) by 1)
        sMutable(i)(k) = bfS(i)(k)

    override def ecbEncipher(input: Array[Byte]): Array[Byte] = {
      val blocks               = input.grouped(8).toArray
      val (lastBlock, zeroNum) = expandLastBlock(blocks.lastOption.getOrElse(Array.emptyByteArray), 0)
      val resBlocks            = blocks.dropRight(1).appended(lastBlock)
      val mutableEncrypted     = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      for (b <- resBlocks) mutableEncrypted.append(blockEncrypt(b))
      mutableEncrypted.toArray.flatten.appendedAll(Ints.toByteArray(zeroNum))
    }

    override def ecbDecipher(input: Array[Byte]): Array[Byte] = {
      val zeroNum          = Ints.fromByteArray(input.takeRight(4))
      val blocks           = input.dropRight(4).grouped(8).toArray
      val mutableEncrypted = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      for (b <- blocks) mutableEncrypted.append(blockDecrypt(b))
      val result = mutableEncrypted.toArray.flatten
      result.dropRight(zeroNum)
    }

    override def cbcEncipher(input: Array[Byte], iv: Array[Byte]): Array[Byte] = {
      val blocks                  = input.grouped(8).toArray
      val (lastBlock, _)          = expandLastBlock(blocks.lastOption.getOrElse(Array.emptyByteArray), 0)
      val resBlocks               = blocks.dropRight(1).appended(lastBlock)
      val firstBlock: Array[Byte] = resBlocks.headOption.getOrElse(Array.emptyByteArray)
      val xorIv: Array[Byte]      = firstBlock.zip(iv).map { case (b, b1) => (b ^ b1).toByte }
      val mutableEncrypted        = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      val encrypted               = blockEncrypt(xorIv)
      mutableEncrypted.append(encrypted)
      var lastBlockBytes = encrypted
      for (b <- resBlocks.drop(1)) {
        val bXor      = b.zip(lastBlockBytes).map { case (b, b1) => (b ^ b1).toByte }
        val encrypted = blockEncrypt(bXor)
        mutableEncrypted.append(encrypted)
        lastBlockBytes = encrypted
      }
      val r = mutableEncrypted.toArray.flatten
      r
    }

    override def cbcDecipher(input: Array[Byte], iv: Array[Byte]): Array[Byte] = {
      val blocks                  = input.grouped(8).toArray
      val firstBlock: Array[Byte] = blocks.headOption.getOrElse(Array.emptyByteArray)
      val mutableDecrypted        = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      val encrypted               = blockDecrypt(firstBlock)
      val fb                      = encrypted.zip(iv).map { case (b, b1) => (b ^ b1).toByte }
      mutableDecrypted.append(fb)
      var lastBlockBytes = firstBlock
      for (b <- blocks.drop(1)) {
        val encrypted = blockDecrypt(b)
        val bXor      = encrypted.zip(lastBlockBytes).map { case (b, b1) => (b ^ b1).toByte }
        mutableDecrypted.append(bXor)
        lastBlockBytes = b
      }
      mutableDecrypted.toArray.flatten
    }

    override def pcbcEncipher(input: Array[Byte], iv: Array[Byte]): Array[Byte] = {
      val blocks                  = input.grouped(8).toArray
      val (lastBlock, _)          = expandLastBlock(blocks.lastOption.getOrElse(Array.emptyByteArray), 0)
      val resBlocks               = blocks.dropRight(1).appended(lastBlock)
      val firstBlock: Array[Byte] = resBlocks.headOption.getOrElse(Array.emptyByteArray)
      val xorIv: Array[Byte]      = firstBlock.zip(iv).map { case (b, b1) => (b ^ b1).toByte }
      val mutableEncrypted        = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      val encrypted               = blockEncrypt(xorIv)
      mutableEncrypted.append(encrypted)
      var lastBlockBytes = encrypted
      var openBytes      = firstBlock
      for (b <- resBlocks.drop(1)) {
        val bXor = b
          .zip(lastBlockBytes)
          .map { case (b, b1) => (b ^ b1).toByte }
          .zip(openBytes)
          .map { case (b, b1) => (b ^ b1).toByte }
        val encrypted = blockEncrypt(bXor)
        mutableEncrypted.append(encrypted)
        openBytes = b
        lastBlockBytes = encrypted
      }
      val r = mutableEncrypted.toArray.flatten
      r
    }

    override def pcbcDecipher(input: Array[Byte], iv: Array[Byte]): Array[Byte] = {
      val blocks                  = input.grouped(8).toArray
      val firstBlock: Array[Byte] = blocks.headOption.getOrElse(Array.emptyByteArray)
      val mutableDecrypted        = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      val encrypted               = blockDecrypt(firstBlock)
      val fb                      = encrypted.zip(iv).map { case (b, b1) => (b ^ b1).toByte }
      mutableDecrypted.append(fb)
      var lastBlockBytes = firstBlock
      var lastOpen       = fb
      for (b <- blocks.drop(1)) {
        val encrypted = blockDecrypt(b)
        val bXor = encrypted
          .zip(lastBlockBytes)
          .map { case (b, b1) => (b ^ b1).toByte }
          .zip(lastOpen)
          .map { case (b, b1) => (b ^ b1).toByte }
        mutableDecrypted.append(bXor)
        lastOpen = bXor
        lastBlockBytes = b
      }
      mutableDecrypted.toArray.flatten
    }

    override def cfbEncipher(input: Array[Byte], iv: Array[Byte]): Array[Byte] = {
      val blocks                  = input.grouped(8).toArray
      val firstBlock: Array[Byte] = blocks.headOption.getOrElse(Array.emptyByteArray)
      val mutableDecrypted        = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      val encrypted               = blockEncrypt(iv)
      val fb                      = encrypted.zip(firstBlock).map { case (b, b1) => (b ^ b1).toByte }
      mutableDecrypted.append(fb)
      var lastBlockBytes = fb
      for (b <- blocks.drop(1)) {
        val encrypted = blockEncrypt(lastBlockBytes)
        val bXor = encrypted
          .zip(b)
          .map { case (b, b1) => (b ^ b1).toByte }
        lastBlockBytes = bXor
        mutableDecrypted.append(bXor)
      }
      mutableDecrypted.toArray.flatten
    }

    override def cfbDecipher(input: Array[Byte], iv: Array[Byte]): Array[Byte] = {
      val blocks                  = input.grouped(8).toArray
      val firstBlock: Array[Byte] = blocks.headOption.getOrElse(Array.emptyByteArray)
      val mutableDecrypted        = new scala.collection.mutable.ArrayBuffer[Array[Byte]](blocks.length)
      val encrypted               = blockEncrypt(iv)
      val fb                      = encrypted.zip(firstBlock).map { case (b, b1) => (b ^ b1).toByte }
      mutableDecrypted.append(fb)
      var lastBlockBytes = firstBlock
      for (b <- blocks.drop(1)) {
        val encrypted = blockEncrypt(lastBlockBytes)
        val bXor = encrypted
          .zip(b)
          .map { case (b, b1) => (b ^ b1).toByte }
        lastBlockBytes = b
        mutableDecrypted.append(bXor)
      }
      mutableDecrypted.toArray.flatten
    }

    def blockEncrypt(input: Array[Byte]): Array[Byte] = {
      val (l: Long, r: Long) =
        cipher(Ints.fromByteArray(input.take(4)), Ints.fromByteArray(input.takeRight(4)), pMutable, sMutable)
      Ints.toByteArray(l.toInt).appendedAll(Ints.toByteArray(r.toInt))
    }

    def blockDecrypt(output: Array[Byte]): Array[Byte] = {
      val (l: Long, r: Long) =
        decipher(Ints.fromByteArray(output.take(4)), Ints.fromByteArray(output.takeRight(4)), pMutable, sMutable)
      Ints.toByteArray(l.toInt).appendedAll(Ints.toByteArray(r.toInt))
    }

    def initKeys(key: String): BlowFishCipher = {
      val keyBytes: Array[Byte] = key.getBytes
      if (keyBytes.length < 56 && keyBytes.length > 4) {
        val pKeys: Array[Long] =
          (0 until (72 - keyBytes.length))
            .foldLeft(keyBytes) { case (acc, i) => acc.appended(keyBytes(i % keyBytes.length)) }
            .grouped(4)
            .toArray
            .view
            .map(bytes => Ints.fromByteArray(bytes) |> asUInt)
            .zip(pMutable)
            .map { case (i, l) => (i ^ l) |> asUInt }
            .toArray

        pMutable = pKeys

        var xr: Long = 0L
        var xl: Long = 0L

        for (i <- (0 until 17) by 2) {
          val (xli, xri) = cipher(xl, xr, pMutable, bfS)
          xr = xri
          xl = xli
          pMutable(i) = xl
          pMutable(i + 1) = xr
        }

        for (i <- 0 until 4)
          for (k <- (0 until 255) by 2) {
            val (xli, xri) = cipher(xl, xr, pMutable, bfS)
            xr = xri
            xl = xli
            sMutable(i)(k) = xl
            sMutable(i)(k + 1) = xr
          }

        this
      } else throw new RuntimeException(s"Unsupported user key length! Got: ${keyBytes.length} key bytes.")
    }

    def cipher(left: Long, right: Long, P: Array[Long], S: Array[Array[Long]]): (Long, Long) = {
      var xl: Long = left
      var xr: Long = right
      for (i <- 0 until 16) {
        xl = (xl ^ P(i)) |> asUInt
        xr = (xr ^ f(xl, S)) |> asUInt
        val (xli, xri) = (xl, xr).swap
        xl = xli
        xr = xri
      }
      val (xll, xrl) = (xl, xr).swap
      xl = xll
      xr = xrl
      xr = (xr ^ P(16)) |> asUInt
      xl = (xl ^ P(17)) |> asUInt
      xl -> xr
    }

    def decipher(left: Long, right: Long, P: Array[Long], S: Array[Array[Long]]): (Long, Long) = {
      var xl: Long = left
      var xr: Long = right
      for (i <- (17 until 1) by -1) {
        xl = (xl ^ P(i)) |> asUInt
        xr = (xr ^ f(xl, S)) |> asUInt
        val (xli, xri) = (xl, xr).swap
        xl = xli
        xr = xri
      }
      val (xll, xrl) = (xl, xr).swap
      xl = xll
      xr = xrl
      xr = (xr ^ P(1)) |> asUInt
      xl = (xl ^ P(0)) |> asUInt
      xl -> xr
    }

    def f: (Long, Array[Array[Long]]) => Long = (block: Long, S: Array[Array[Long]]) => {
      val a: Int = ((block & 0xff000000) >> 24).toInt
      val b: Int = ((block & 0x00ff0000) >> 16).toInt
      val c: Int = ((block & 0x0000ff00) >> 8).toInt
      val d: Int = (block & 0x000000ff).toInt
      (((((S(0)(a) + S(1)(b)) % mod32) ^ S(2)(c)) |> asUInt) + S(3)(d)) % mod32
    }

  }

  @scala.annotation.tailrec
  def expandLastBlock(block: Array[Byte], acc: Int): (Array[Byte], Int) =
    if (block.length < 8)
      expandLastBlock(block.appended(0: Byte), acc + 1)
    else block -> acc

}
