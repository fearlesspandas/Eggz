package network

import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

case class AuthRequest(id: String, pubkey: String)

object AuthRequest {
  implicit val encoder: JsonEncoder[AuthRequest] =
    DeriveJsonEncoder.gen[AuthRequest]
  implicit val decoder: JsonDecoder[AuthRequest] =
    DeriveJsonDecoder.gen[AuthRequest]
}
