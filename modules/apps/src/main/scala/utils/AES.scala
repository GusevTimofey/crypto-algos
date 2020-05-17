package utils

import algos.hash.HashFunction
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object AES {

  val algorithm = "AES"

  val sha1: HashFunction = HashFunction.sha1

  private def cipher(mode: Int, secret: String): Cipher = {
    val encipher: Cipher = Cipher.getInstance(algorithm + "/ECB/PKCS5Padding")
    encipher.init(mode, new SecretKeySpec(sha1.make(secret).getBytes.take(16), algorithm))
    encipher
  }

  def encrypt(data: Array[Byte], secret: String): Array[Byte] = cipher(Cipher.ENCRYPT_MODE, secret).doFinal(data)

  def decrypt(data: Array[Byte], secret: String): Array[Byte] = cipher(Cipher.DECRYPT_MODE, secret).doFinal(data)
}
