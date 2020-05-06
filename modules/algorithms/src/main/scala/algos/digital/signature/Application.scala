package algos.digital.signature

object Application extends App {

  val digitalSignature = DS.apply
  val pairs            = digitalSignature.formKeysPair(16)
  val ciphered         = digitalSignature.cipher(BigInt(40000), pairs._2)
  val deciphered       = digitalSignature.decipher(ciphered, pairs._1)
  println("Algorithm finished")
}
