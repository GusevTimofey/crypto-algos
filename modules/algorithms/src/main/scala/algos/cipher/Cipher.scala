package algos.cipher

import algos.common.BitsLike.instances._
import algos.common.FromBits.instances._
import algos.common.BitsLike.ops._
import algos.common.FromBits.ops._
import algos.common.types._
import algos.common.utils._
import mouse.all._

trait Cipher {
  def encrypt(input: Array[Byte], userKey: String): Array[Byte]
  def decrypt(output: Array[Byte], userKey: String): Array[Byte]
}

object Cipher {
  final class BlowFishCipher extends Cipher {

    val mod32: Int = Math.pow(2, 32).toInt

    override def encrypt(input: Array[Byte], userKey: String): Array[Byte] = {
      val inputBits: String = input.map(_.asBits).foldLeft("")(_ + _)
      val roundPKeys: List[Int] = userKey |> initOps
      val simulateRegime: List[Block64BitsString] = inputBits.grouped(64).map { block =>
        (block |> Block64BitsString.apply) |> cipher(roundPKeys)
      }.toList
      simulateRegime.map(_.str).toArray
      ???
    }

    override def decrypt(output: Array[Byte], userKey: String): Array[Byte] = ???

    def initOps(key: String): List[Int] = {
      val keyBits: String = key.map(_.asBits).foldLeft("")(_ + _)
      if (keyBits.length >= 32 && keyBits.length <= 448) {
        val key32Grouped: List[String] = keyBits.grouped(32).toList
        val resultedKey: List[String]  = key32Grouped appendedAll (key32Grouped take 18 - key32Grouped.size)
        val pKeys: List[Int]           = resultedKey.zip(bfP).map { case (str, i) => str.liftToInt ^ i }
        (0 to 8).foldLeft(List.empty[Int]) {
          case (acc, _) =>
            val p1p2: List[Int] =
              cipher(pKeys)((ZeroByteBits * 8) |> Block64BitsString.apply).str
                .grouped(32)
                .map(_.liftToInt)
                .toList
            acc.appendedAll(p1p2)
        }
      } else throw new RuntimeException(s"Unsupported user key length! Got: ${keyBits.length} key bits.")
    }

    def cipher(pKeys: List[Int])(block: Block64BitsString): Block64BitsString = {
      val (l16, r16) = (0 to 15).foldLeft(block.str.take(32).liftToInt -> block.str.drop(32).liftToInt) {
        case ((l, r), i) =>
          (l ^ pKeys(i)) -> (r ^ F(l.asBits |> Block32BitsString.apply).i)
      }
      val (lr, rr) = (r16 ^ pKeys(17)) -> (l16 ^ pKeys(16))
      (lr.asBits + rr.asBits) |> Block64BitsString.apply
    }

    def F: Block32BitsString => Block32BitsInt = (block: Block32BitsString) => {
      val a :: b :: c :: d :: Nil = block.str.grouped(8).map(_.liftToByte).toList
      (((((bfS(0)(a) + bfS(1)(b)) % mod32) ^ bfS(2)(c)) + bfS(3)(d)) % mod32) |> Block32BitsInt.apply
    }

  }

}
