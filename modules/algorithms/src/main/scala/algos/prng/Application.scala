package algos.prng

import algos.common.BitsLike.instances._
import algos.common.BitsLike.ops._
import algos.common.FileReader
import algos.common.FromBits.instances._
import algos.common.FromBits.ops._

object Application extends App {

  val rng: RNG = RNG(1000L)
  val a = rng.next()
  println(a)
  println(a.asBits)
  println(a.asBits.length)

  println((0 to 100).map(_ => rng.next().asBits).foldLeft("")(_ + _))

}
