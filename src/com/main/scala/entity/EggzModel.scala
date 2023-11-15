package entity

import controller.Location
import controller.Stats
import src.com.main.scala.entity.EggzOps.ID
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait EggzModel {}
object EggzModel {
  implicit val encoder: JsonEncoder[EggzModel] = DeriveJsonEncoder.gen[EggzModel]
  implicit val decoder: JsonDecoder[EggzModel] = DeriveJsonDecoder.gen[EggzModel]
}
case class REPAIR_EGG(id: ID, stats: Stats, cost: Double, repairValue: Double) extends EggzModel
case class PLAYER_EGG(ID: ID, stats: Stats, location: (Double, Double, Double)) extends EggzModel
