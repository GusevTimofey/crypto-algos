package algos.hash

import com.google.common.primitives.Ints

object Application extends App {

  val sha1 = SHA1()

  println(Int.MaxValue)
  //2147483648
  //2147483647

  val res = sha1.make("sha")
  /*2fd4e1c67a2d28fced849ee1bb76e7391b93eb12*/
  /*
  1110-0111 = 231
  1010-0011 = 163
  0000-1100 = 12

  0111-0011
  0110-1000
  0110-0001
  1
   */

  println(s"Main app")
  println(res.length)
  println(res.toList)
  println(new String(res))

}
