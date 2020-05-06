package algos.common

import mouse.all._
import simulacrum.typeclass

/*
  type class rules are:
    - output size is 8 bits
    - asBits should be bijective function e.g. fromBits(asBits(value)) == value
 */
@typeclass trait BitsLike[T] {
  def asBits(t: T): String
}

object BitsLike {
  object instances {
    implicit object ByteBitsLikeInstance extends BitsLike[Byte] {
      override def asBits(b: Byte): String = b.toBinaryString |> expandedAsBits
    }
    implicit object CharBitsLikeInstance extends BitsLike[Char] {
      override def asBits(c: Char): String = c.toBinaryString |> expandedAsBits
    }
    implicit object IntBitsLikeInstance extends BitsLike[Int] {
      override def asBits(i: Int): String = i.toBinaryString |> expandedAsBits
    }
    implicit object LongBitsLikeInstance extends BitsLike[Long] {
      override def asBits(l: Long): String = l.toBinaryString |> expandedAsBits
    }
  }

  private def expandedAsBits: String => String =
    (bits: String) => (0 until (8 - bits.length)).map(_ => '0').foldLeft("")(_ + _) + bits
}
