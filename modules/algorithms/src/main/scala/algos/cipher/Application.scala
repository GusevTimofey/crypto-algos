package algos.cipher

import algos.cipher.Cipher.BlowFishCipher

object Application extends App {

  def ecbR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 500000).getBytes()
    val encrypted: Array[Byte] = blowfish.ecbEncrypt(input)
    val decrypted: Array[Byte] = blowfish.ecbDecrypt(encrypted)
    println(decrypted.sameElements(input))
  }

  def cbcR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 500000).getBytes()
    val iv = "11111111".getBytes
    val encrypted: Array[Byte] = blowfish.cbcEncrypt(input, iv)
    val decrypted: Array[Byte] = blowfish.cbcDecrypt(encrypted, iv)
    println(decrypted.sameElements(input))
  }

  def pcbcR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 500000).getBytes()
    val iv = "11111111".getBytes
    val encrypted: Array[Byte] = blowfish.pcbcEncrypt(input, iv)
    val decrypted: Array[Byte] = blowfish.pcbcDecrypt(encrypted, iv)
    println(decrypted.sameElements(input))
  }

  def cfbR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 500000).getBytes()
    val iv = "11111111".getBytes
    val encrypted: Array[Byte] = blowfish.cfbEncrypt(input, iv)
    val decrypted: Array[Byte] = blowfish.cfbDecrypt(encrypted, iv)
    println(decrypted.sameElements(input))
  }
  ecbR()
  cbcR()
  pcbcR()
  cfbR()
}
