package algos.hash

object Application extends App {

  val sha1 = SHA1.apply
  val res  = sha1.make("sha")
  println(res.length)
  println(new String(res))

}
