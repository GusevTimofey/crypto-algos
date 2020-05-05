package algos.hash

import algos.common.BitsLike.instances._
import algos.common.FromBits.instances._
import algos.common.BitsLike.ops._
import algos.common.FromBits.ops._
import algos.common.utils._
import mouse.all._

import scala.annotation._

trait SHA1 {

  def make(input: String): String

}

object SHA1 {

  def apply: SHA1 = new SHA1 {

    private val A: Long = 0x67452301L
    private val B: Long = 0xEFCDAB89L
    private val C: Long = 0x98BADCFEL
    private val D: Long = 0x10325476L
    private val E: Long = 0xC3D2E1F0L

    override def make(input: String): String = {
      val ar :: br :: cr :: dr :: er :: _ = formLastBlock(input).foldLeft(List(A, B, C, D, E)) {
        case (a :: b :: c :: d :: e :: _, wInput: String) =>
          val wMain: List[Long] = (16 to 79).foldLeft(wInput.grouped(32).toList.map(_.liftToLong)) {
            case (acc: List[Long], i: Int) =>
              acc appended leftRotate(acc(i - 3) ^ acc(i - 8) ^ acc(i - 14) ^ acc(i - 16), 1)
          }

          val (aN, bN, cN, dN, eN) = (0 to 79).foldLeft(a, b, c, d, e) {
            case ((al, bl, cl, dl, el), i) =>
              val newA = (leftRotate(al, 5) + Ft(bl, cl, dl, i) + el + Kt(i) + wMain(i)) & 0xFFFFFFFFL
              (newA, al, leftRotate(bl, 30), cl, dl)
          }
          List(a + aN, b + bN, c + cN, d + dN, e + eN).map(_ & 0xFFFFFFFFL)
        case (l, _) => l
      }
      "%08x%08x%08x%08x%08x".format(ar, br, cr, dr, er)
    }

    private def Kt(round: Int): Long =
      if (round <= 19) 0x5A827999L
      else if (round <= 39) 0x6ED9EBA1L
      else if (round <= 59) 0x8F1BBCDCL
      else 0xCA62C1D6L

    private def Ft(b: Long, c: Long, d: Long, round: Int): Long =
      if (round <= 19) (b & c) | (~b & d)
      else if (round <= 39) b ^ c ^ d
      else if (round <= 59) (b & c) | (b & d) | (c & d)
      else b ^ c ^ d

    private def leftRotate(block: Long, count: Int): Long = ((block << count) | (block >> (32 - count))) & 0xFFFFFFFFL

    private def formLastBlock(input: String): List[String] = {
      val inputBits: String              = input.map(_.asBits).foldLeft("")(_ + _)
      val groupedInputBits: List[String] = inputBits.grouped(512).toList
      def expandLastBlock(tailBlockBits: String): String = {
        @tailrec def loop(acc: String): String =
          if ((acc.length + 64) % 512 == 0) {
            val tailBits: String = inputBits.length.toLong.asBits
            acc ++ (0 until (8 - tailBits.grouped(8).size)).foldLeft(tailBits) {
              case (acc, _) => acc prependedAll ZeroByteBits
            }
          } else loop(acc + '0')
        loop(tailBlockBits + '1')
      }
      val lastExpandedBlock: String = groupedInputBits.lastOption.getOrElse("") |> expandLastBlock
      groupedInputBits dropRight 1 appendedAll lastExpandedBlock.grouped(512)
    }
  }
}
