package algos.common

import simulacrum.typeclass

/*
  type class rules are:
    - input should be less than 8 bits
    - fromBits(asBits(value)) == value
 */
@typeclass trait FromBits[T] {
  def fromBits(bits: String): T
}

object FromBits {
  object instances {
    implicit object ByteFromBitsInstance extends FromBits[Byte] {
      override def fromBits(s: String): Byte = java.lang.Byte.parseByte(s, 2)
    }
    implicit object LongBitsLikeInstance extends FromBits[Long] {
      override def fromBits(s: String): Long = java.lang.Long.parseLong(s, 2)
    }
  }
}
