package network

import network.Session.Secret
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait Session {
  val secret: Secret
  val id: String
}
object Session {
  type Secret = String
  implicit val encoder: JsonEncoder[Session] = DeriveJsonEncoder.gen[Session]
  implicit val decoder: JsonDecoder[Session] = DeriveJsonDecoder.gen[Session]
}

case class BasicSession(id: String, secret: Secret) extends Session

object BasicSession {
  type Secret = String
  implicit val encoder: JsonEncoder[BasicSession] =
    DeriveJsonEncoder.gen[BasicSession]
  implicit val decoder: JsonDecoder[BasicSession] =
    DeriveJsonDecoder.gen[BasicSession]
}

case class PublicSession(
  id: String,
  secret: Secret,
  existing_pub_keys: Set[String]
) extends Session

object PublicSession {
  type Secret = String
  implicit val encoder: JsonEncoder[PublicSession] =
    DeriveJsonEncoder
      .gen[PublicSession]
      .contramap(k => PublicSession(k.id, "", Set.empty[String]))
  implicit val decoder: JsonDecoder[PublicSession] =
    DeriveJsonDecoder.gen[PublicSession]
}
