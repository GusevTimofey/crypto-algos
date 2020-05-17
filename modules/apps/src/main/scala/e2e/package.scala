import io.circe.{Decoder, Encoder}
import io.circe.generic.JsonCodec
import io.circe.syntax._
import cats.syntax.functor._

package object e2e {

  sealed trait HttpDSL

  @JsonCodec final case class AValue(A: BigInt) extends HttpDSL

  @JsonCodec final case class BValue(B: BigInt) extends HttpDSL

  @JsonCodec final case class PGValues(P: BigInt, G: BigInt) extends HttpDSL

  implicit val encodeEvent: Encoder[HttpDSL] = Encoder.instance {
    case av: AValue    => av.asJson
    case bv: BValue    => bv.asJson
    case pgv: PGValues => pgv.asJson
  }

  implicit val decodeEvent: Decoder[HttpDSL] =
    List[Decoder[HttpDSL]](
      Decoder[AValue].widen,
      Decoder[BValue].widen,
      Decoder[PGValues].widen
    ).reduceLeft(_ or _)

  @JsonCodec final case class Message(bytes: Array[Byte])
}
