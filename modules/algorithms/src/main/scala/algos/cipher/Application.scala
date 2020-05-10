package algos.cipher

import algos.cipher.Cipher.BlowFishCipher

object Application extends App {

  def ecbR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("blowfish" * 8).getBytes()
    println(s"input bytes ${input.toList}")
    val encrypted: Array[Byte] = blowfish.ecbEncrypt(input)
    val decrypted: Array[Byte] = blowfish.ecbDecrypt(encrypted)
    println(s"decrypted bytes: ${decrypted.toList}")
    println(decrypted.sameElements(input))
  }
  def cbcR: Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 100).getBytes()
    val iv = "11111111".getBytes
    println(s"input bytes ${input.length}")
    val encrypted: Array[Byte] = blowfish.cbcEncrypt(input, iv)
    println(s"encrypted bytes ${encrypted.length}")
    val decrypted: Array[Byte] = blowfish.cbcDecrypt(encrypted, iv)
    println(s"decrypted bytes: ${decrypted.length}")
    println(decrypted.sameElements(input))
    println(decrypted.toList)
    println(input.toList)
  }


}
