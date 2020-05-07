package algos.prng

object Application extends App {

  val rng: RNG = RNG(System.currentTimeMillis())
  while(true) println(rng.next())

}
