package entity

import entity.Terrain.TerrainId
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

case class TerrainUnitM(location: Vector[Double], entities: Map[TerrainId, Int])
    extends TerrainModel

object TerrainUnitM {
  implicit val decoder: JsonDecoder[TerrainUnitM] =
    DeriveJsonDecoder.gen[TerrainUnitM]
  implicit val encoder: JsonEncoder[TerrainUnitM] =
    DeriveJsonEncoder.gen[TerrainUnitM]
}

case class TerrainRegionM(terrain: Set[TerrainModel]) extends TerrainModel
object TerrainRegionM {
  implicit val decoder: JsonDecoder[TerrainRegionM] =
    DeriveJsonDecoder.gen[TerrainRegionM]
  implicit val encoder: JsonEncoder[TerrainRegionM] =
    DeriveJsonEncoder.gen[TerrainRegionM]
}
