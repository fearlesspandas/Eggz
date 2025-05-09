package entity

import zio.json.*
import controller.Stats
import entity.EggzOps.ID

sealed trait EggzModel {}
object EggzModel {
  implicit val encoder: JsonEncoder[EggzModel] =
    DeriveJsonEncoder.gen[EggzModel]
  implicit val decoder: JsonDecoder[EggzModel] =
    DeriveJsonDecoder.gen[EggzModel]
}
case class REPAIR_EGG(id: ID, stats: Stats, cost: Double, repairValue: Double)
    extends EggzModel
case class PLAYER_EGG(ID: ID, stats: Stats, location: (Double, Double, Double))
    extends EggzModel

case class PROWLER_EGG(ID: ID, stats: Stats, location: (Double, Double, Double))
    extends EggzModel
