package src.com.db

import src.com.db.DBServer.{PubKey, SessionToken}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class VerifySession (publickey:PubKey, token:SessionToken)
object VerifySession{
  implicit def decoder:JsonDecoder[VerifySession] = DeriveJsonDecoder.gen[VerifySession]
  implicit def encoder:JsonEncoder[VerifySession] = DeriveJsonEncoder.gen[VerifySession]
}

case class ValidSession (publickey:PubKey, valid:Boolean)
object ValidSession{
  implicit def decoder:JsonDecoder[ValidSession] = DeriveJsonDecoder.gen[ValidSession]
  implicit def encoder:JsonEncoder[ValidSession] = DeriveJsonEncoder.gen[ValidSession]
}
