package algos.cipher

import algos.cipher.Cipher.BlowFishCipher

object Application extends App {

  def ecbR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 500000).getBytes()
    val encrypted: Array[Byte] = blowfish.ecbEncipher(input)
    val decrypted: Array[Byte] = blowfish.ecbDecipher(encrypted)
    println(decrypted.sameElements(input))
  }

  def cbcR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 500000).getBytes()
    val iv = "11111111".getBytes
    val encrypted: Array[Byte] = blowfish.cbcEncipher(input, iv)
    val decrypted: Array[Byte] = blowfish.cbcDecipher(encrypted, iv)
    println(decrypted.sameElements(input))
  }

  def pcbcR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("blowfish")
    val input: Array[Byte] = ("b1l1o1w1f1i1s1h1" * 500000).getBytes()
    val iv = "11111111".getBytes
    val encrypted: Array[Byte] = blowfish.pcbcEncipher(input, iv)
    val decrypted: Array[Byte] = blowfish.pcbcDecipher(encrypted, iv)
    println(decrypted.sameElements(input))
  }

  def cfbR(): Unit = {
    val blowfish: BlowFishCipher = Cipher.blowFish("qwertyqwerty123")
    val input: Array[Byte] = ("Blowfishssh").getBytes()
    val iv = "11111111".getBytes
    val encrypted: Array[Byte] = blowfish.cfbEncipher(input, iv)
    val decrypted: Array[Byte] = blowfish.cfbDecipher(encrypted, iv)
    println(decrypted.sameElements(input))
  }
  val t = System.currentTimeMillis()
  ecbR()
  println(s"Ready: ${(System.currentTimeMillis() - t)/1000}s")
//  cbcR()
//  pcbcR()
  //cfbR()
}
