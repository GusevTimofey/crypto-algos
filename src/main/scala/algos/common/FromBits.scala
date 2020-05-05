package algos.common

//Not a type class :(
trait FromBits {
  def fromBitsByte(bits: String): Byte
  def fromBitsLong(bits: String): Long
}

object FromBits {
  object instances {
    implicit object FromBitsInstance extends FromBits {
      override def fromBitsByte(s: String): Byte = java.lang.Byte.parseByte(s, 2)
      override def fromBitsLong(s: String): Long = java.lang.Long.parseLong(s, 2)
    }
  }
  object ops {
    implicit class FromBitsOps(val string: String) extends AnyVal {
      def liftToByte(implicit env: FromBits): Byte = env.fromBitsByte(string)
      def liftToLong(implicit env: FromBits): Long = env.fromBitsLong(string)
    }
  }
}
