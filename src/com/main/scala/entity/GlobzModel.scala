package entity

import controller.Stats
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.EggzOps.ID
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait GlobzModel {}
object GlobzModel {
  implicit val encoder: JsonEncoder[GlobzModel] = DeriveJsonEncoder.gen[GlobzModel]
  implicit val decoder: JsonDecoder[GlobzModel] = DeriveJsonDecoder.gen[GlobzModel]
}
case class PlayerGlob(id: String, stats: Stats) extends GlobzModel
object PlayerGlob {
  implicit val encoder: JsonEncoder[PlayerGlob] = DeriveJsonEncoder.gen[PlayerGlob]
  implicit val decoder: JsonDecoder[PlayerGlob] = DeriveJsonDecoder.gen[PlayerGlob]
}
case class GlobInMemory(id: String, eggs: Set[EggzModel], relations: Set[(ID, ID)])
    extends GlobzModel
object GlobInMemory {
  implicit val encoder: JsonEncoder[GlobInMemory] = DeriveJsonEncoder.gen[GlobInMemory]
  implicit val decoder: JsonDecoder[GlobInMemory] = DeriveJsonDecoder.gen[GlobInMemory]
}
