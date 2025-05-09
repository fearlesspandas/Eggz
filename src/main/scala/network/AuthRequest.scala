package network

import zio.json.*
case class AuthRequest(id: String, pubkey: String)

object AuthRequest {
  implicit val encoder: JsonEncoder[AuthRequest] =
    DeriveJsonEncoder.gen[AuthRequest]
  implicit val decoder: JsonDecoder[AuthRequest] =
    DeriveJsonDecoder.gen[AuthRequest]
}
