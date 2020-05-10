package algos.common

object utils {

  val ZeroByteBits: String = "00000000"

  def asUInt(l: Long): Long = l & 0xFFFFFFFFL
  def asUInt(i: Int): Long = i & 0xFFFFFFFFL

}
