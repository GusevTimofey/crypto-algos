package algos.hash

import algos.common.BitsLike.instances._
import algos.common.FromBits.instances._
import algos.common.BitsLike.ops._
import algos.common.FromBits.ops._
import algos.common.utils._
import mouse.all._

import scala.annotation._

trait SHA1 { def make(input: String): String }

object SHA1 {
  def apply: SHA1 = new SHA1 {
    override def make(input: String): String = {
      val ar :: br :: cr :: dr :: er :: _ = formLastBlock(input).foldLeft(List(A, B, C, D, E)) {
        case (a :: b :: c :: d :: e :: _, wIn) =>
          val w: List[Block64Bits] = wIn.value.grouped(32).toList.map(_.liftToLong |> Block64Bits.apply) |> computeWi
          val (aN, bN, cN, dN, eN) = runBlockCycle(a, b, c, d, e, w)
          List(a + aN, b + bN, c + cN, d + dN, e + eN).map(_ & 0xFFFFFFFFL)
        case (l, _) => l
      }
      "%08x%08x%08x%08x%08x".format(ar, br, cr, dr, er)
    }

    private def computeWi: List[Block64Bits] => List[Block64Bits] =
      (wi: List[Block64Bits]) =>
        (16 to 79).foldLeft(wi) {
          case (acc, i) =>
            val wi: Long = leftRotate(acc(i - 3).v ^ acc(i - 8).v ^ acc(i - 14).v ^ acc(i - 16).v, 1)
            acc.appended(wi |> Block64Bits.apply)
        }

    private def runBlockCycle: (Long, Long, Long, Long, Long, List[Block64Bits]) => (Long, Long, Long, Long, Long) =
      (a: Long, b: Long, c: Long, d: Long, e: Long, wRes: List[Block64Bits]) =>
        (0 to 79).foldLeft(a, b, c, d, e) {
          case ((al, bl, cl, dl, el), i) =>
            val newA = (leftRotate(al, 5) + Ft(bl, cl, dl, i) + el + Kt(i) + wRes(i).v) & 0xFFFFFFFFL
            (newA, al, leftRotate(bl, 30), cl, dl)
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

    private def formLastBlock(input: String): List[Block512Bits] = {
      val inputBits: String                    = input.map(_.asBits).foldLeft("")(_ + _)
      val groupedInputBits: List[Block512Bits] = inputBits.grouped(512).map(Block512Bits.apply).toList
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
      val lastExpandedBlock: String = groupedInputBits.lastOption.map(_.value).getOrElse("") |> expandLastBlock
      groupedInputBits dropRight 1 appendedAll lastExpandedBlock.grouped(512).map(Block512Bits.apply)
    }
  }

  private val A: Long = 0x67452301L
  private val B: Long = 0xEFCDAB89L
  private val C: Long = 0x98BADCFEL
  private val D: Long = 0x10325476L
  private val E: Long = 0xC3D2E1F0L

  import io.estatico.newtype.macros.newtype
  @newtype final case class Block512Bits(value: String)
  @newtype final case class Block64Bits(v: Long)
}
