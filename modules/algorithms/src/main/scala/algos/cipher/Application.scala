package algos.cipher

object Application extends App {

  val a  = Cipher.blowFish("abcdefc")
  val in = Array.fill(8)(1: Byte)
  val e  = a.encrypt(in)
  val d  = a.decrypt(e)
  println(d.sameElements(in))
}
