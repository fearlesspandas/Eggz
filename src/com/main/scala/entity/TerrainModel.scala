package entity

import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait TerrainModel

object TerrainModel {
  implicit val decoder: JsonDecoder[TerrainModel] =
    DeriveJsonDecoder.gen[TerrainModel]
  implicit val encoder: JsonEncoder[TerrainModel] =
    DeriveJsonEncoder.gen[TerrainModel]
}

case class TerrainUnitM() extends TerrainModel

object TerrainUnitM {
  implicit val decoder: JsonDecoder[TerrainUnitM] =
    DeriveJsonDecoder.gen[TerrainUnitM]
  implicit val encoder: JsonEncoder[TerrainUnitM] =
    DeriveJsonEncoder.gen[TerrainUnitM]
}
