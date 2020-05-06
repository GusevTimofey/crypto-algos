package algos.common

object types {
  import io.estatico.newtype.macros.newtype
  @newtype final case class Block512Bits(value: String)
  @newtype final case class Block64BitsLong(v: Long)
  @newtype final case class Block64BitsString(str: String)
  @newtype final case class Block32BitsString(str: String)
  @newtype final case class Block32BitsInt(i: Int)
}
