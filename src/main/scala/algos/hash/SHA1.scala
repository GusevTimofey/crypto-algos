package algos.hash

import com.google.common.primitives.{ Ints, Longs }

trait SHA1 {

  def make(input: String): Array[Byte]

}

object SHA1 {

  def apply(): SHA1 = new SHA1 {

    private val A = 0x67452301
    private val B = 0xEFCDAB89
    private val C = 0x98BADCFE
    private val D = 0x10325476
    private val E = 0xC3D2E1F0

    override def make(input: String): Array[Byte] = {
      val validatedData: List[List[Int]] = formLastBlock(input)
      val (ar, br, cr, dr, er) = validatedData.foldLeft(A, B, C, D, E) {
        case ((a, b, c, d, e), wInit: List[Int]) =>

          val wMain: List[Int] = (16 to 79).foldLeft(wInit) {
            case (acc, i) =>
              acc prepended ((acc(i - 3) ^ acc(i - 8) ^ acc(i - 14) ^ acc(i - 16)) << 1)
          }

          val (aN, bN, cN, dN, eN) = (0 to 79).foldLeft(a, b, c, d, e) {
            case ((a, b, c, d, e), i) =>
              ((a << 5) + Ft(b, c, d, i) + e + Kt(i) + wMain(i), a, b << 30, c, d)
          }

          (a + aN, b + bN, c + cN, d + dN, e + eN)
      }
      println(s"List(ar, br, cr, dr, er): ${List(ar, br, cr, dr, er)}")
      (ar :: br :: cr :: dr :: er :: Nil flatMap Ints.toByteArray) toArray
    }

    private def Kt(round: Int): Int =
      if (round <= 19) 0x5A827999
      else if (round <= 39) 0x6ED9EBA1
      else if (round <= 59) 0x8F1BBCDC
      else 0xCA62C1D6

    private def Ft(x: Int, y: Int, z: Int, round: Int): Int =
      if (round <= 19) (x & y) | (~x & z)
      else if (round <= 39) x ^ y ^ z
      else if (round <= 59) (x & y) | (x & z) | (y & z)
      else x ^ y ^ z

    private def formLastBlock(input: String): List[List[Int]] = {
      val inputDataRaw: List[String] = input.grouped(32).toList
      def expandBlock(tail: String): List[Int] = {
        @scala.annotation.tailrec
        def loop(acc: String): String =
          if ((acc.length + 64) % 512 == 0) {
            val size: Long             = input.flatMap(expandString).length.toLong
            val sizeBytes: Array[Byte] = Longs.toByteArray(size)
            val sizeBytesStr: String   = new String(sizeBytes)
            val sizeBits: String       = sizeBytesStr.flatMap(expandString)
            acc ++ sizeBits
          } else loop(acc + '0')
        def expandString: Char => String = (i: Char) => {
          val t = i.toBinaryString
          if (t.length < 8) (0 until (8 - t.length)).map(_ => '0').foldLeft("")(_ + _) + t
          else t
        }
        val tailNew: String = loop(tail.flatMap(expandString) + "1")
        println(s"Expanded block: $tailNew.")
        val result: List[Int] = tailNew.grouped(32).toList.map(intStr => Integer.parseInt(intStr, 2))
        println(s"Result in ints is: $result")
        result
      }
      val lastBlock: List[Int] = expandBlock(inputDataRaw.lastOption.getOrElse(""))
      val init: List[List[Int]] =
        inputDataRaw
          .dropRight(1)
          .map(_.grouped(2).toList.map(u => Ints.fromByteArray(u.getBytes)))
      val result: List[List[Int]] = init :+ lastBlock
      println(s"former result is: $result.")
      result
    }
  }
}
