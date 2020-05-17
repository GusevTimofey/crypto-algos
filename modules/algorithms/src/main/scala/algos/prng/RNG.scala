package algos.prng

trait RNG { def next(bits: Int = 32): Int }

object RNG {
  def apply(seed: Long): RNG = new LCG(seed)

  private class LCG(var seed: Long) extends RNG {
    val mask: Long       = (1L << 48) - 1
    val multiplier: Long = 0x5DEECE66DL
    val addend: Long     = 0xBL

    override def next(bits: Int = 32): Int = {
      val nextSeed = (seed * multiplier + addend) & mask
      seed = nextSeed
      (nextSeed >>> (48 - bits)).toInt
    }
  }

  def seed: Long = System.nanoTime() * 8682522807148012L * 181783497276652981L
}
