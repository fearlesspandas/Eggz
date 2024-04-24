package controller

import entity.EggzModel
import entity.GlobzModel
import entity.TerrainModel
import entity.TerrainUnitM
import physics.DESTINATION_TYPE
import physics.DestinationModel
import physics.destination
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait QueryResponse {}

object QueryResponse {
  implicit val encoder: JsonEncoder[QueryResponse] =
    DeriveJsonEncoder.gen[QueryResponse]
  implicit val decoder: JsonDecoder[QueryResponse] =
    DeriveJsonDecoder.gen[QueryResponse]
}

//MSG contains an id that can be used to route the response back to the original sender entity
case class MSG(route: String, message: QueryResponse) extends QueryResponse
object MSG {
  implicit val encoder: JsonEncoder[MSG] = DeriveJsonEncoder.gen[MSG]
  implicit val decoder: JsonDecoder[MSG] = DeriveJsonDecoder.gen[MSG]
}

case class GlobSet(globs: Set[GlobzModel]) extends QueryResponse
object GlobSet {
  implicit val encoder: JsonEncoder[GlobSet] = DeriveJsonEncoder.gen[GlobSet]
  implicit val decoder: JsonDecoder[GlobSet] = DeriveJsonDecoder.gen[GlobSet]
}
case class EggSet(eggs: Set[EggzModel]) extends QueryResponse
object EggSet {
  implicit val encoder: JsonEncoder[EggSet] = DeriveJsonEncoder.gen[EggSet]
  implicit val decoder: JsonDecoder[EggSet] = DeriveJsonDecoder.gen[EggSet]
}
case class EntityIDSet(ids: Set[GLOBZ_ID]) extends QueryResponse
object EntityIDSet {
  implicit val encoder: JsonEncoder[EntityIDSet] =
    DeriveJsonEncoder.gen[EntityIDSet]
  implicit val decoder: JsonDecoder[EntityIDSet] =
    DeriveJsonDecoder.gen[EntityIDSet]
}
case class Stats(id: ID, health: Double, energy: Double) extends QueryResponse
object Stats {
  implicit val encoder: JsonEncoder[Stats] = DeriveJsonEncoder.gen[Stats]
  implicit val decoder: JsonDecoder[Stats] = DeriveJsonDecoder.gen[Stats]
}
case class AllStats(stats: Set[Stats]) extends QueryResponse
object AllStats {
  implicit val encoder: JsonEncoder[AllStats] = DeriveJsonEncoder.gen[AllStats]
  implicit val decoder: JsonDecoder[AllStats] = DeriveJsonDecoder.gen[AllStats]
}

case class NextDestination(id: ID, destination: destination)
    extends QueryResponse
object NextDestination {
  implicit val encoder: JsonEncoder[NextDestination] =
    DeriveJsonEncoder.gen[NextDestination]
  implicit val decoder: JsonDecoder[NextDestination] =
    DeriveJsonDecoder.gen[NextDestination]
}
case class AllDestinations(id: ID, destinations: Seq[destination])
    extends QueryResponse
object AllDestinations {
  implicit val encoder: JsonEncoder[AllDestinations] =
    DeriveJsonEncoder.gen[AllDestinations]
  implicit val decoder: JsonDecoder[AllDestinations] =
    DeriveJsonDecoder.gen[AllDestinations]
}
case class Location(id: GLOBZ_ID, location: (Double, Double, Double))
    extends QueryResponse
object Location {
  implicit val encoder: JsonEncoder[Location] = DeriveJsonEncoder.gen[Location]
  implicit val decoder: JsonDecoder[Location] = DeriveJsonDecoder.gen[Location]
}
case class NoLocation(id: GLOBZ_ID) extends QueryResponse
object NoLocation {
  implicit val encoder: JsonEncoder[NoLocation] =
    DeriveJsonEncoder.gen[NoLocation]
  implicit val decoder: JsonDecoder[NoLocation] =
    DeriveJsonDecoder.gen[NoLocation]
}

case class Input(id: GLOBZ_ID, vec: (Double, Double, Double))
    extends QueryResponse
object Input {
  implicit val encoder: JsonEncoder[Input] = DeriveJsonEncoder.gen[Input]
  implicit val decoder: JsonDecoder[Input] = DeriveJsonDecoder.gen[Input]
}
case class NoInput(id: GLOBZ_ID) extends QueryResponse
object NoInput {
  implicit val encoder: JsonEncoder[NoInput] = DeriveJsonEncoder.gen[NoInput]
  implicit val decoder: JsonDecoder[NoInput] = DeriveJsonDecoder.gen[NoInput]
}
case class Blob(blob: Option[GlobzModel]) extends QueryResponse
object Blob {
  implicit val encoder: JsonEncoder[Blob] = DeriveJsonEncoder.gen[Blob]
  implicit val decoder: JsonDecoder[Blob] = DeriveJsonDecoder.gen[Blob]
}
case class LV(id: GLOBZ_ID, lv: (Double, Double, Double)) extends QueryResponse
object LV {
  implicit val encoder: JsonEncoder[LV] = DeriveJsonEncoder.gen[LV]
  implicit val decoder: JsonDecoder[LV] = DeriveJsonDecoder.gen[LV]
}
case class PhysStat(id: GLOBZ_ID, max_speed: Double) extends QueryResponse
object PhysStat {
  implicit val encoder: JsonEncoder[PhysStat] =
    DeriveJsonEncoder.gen[PhysStat]
  implicit val decoder: JsonDecoder[PhysStat] = DeriveJsonDecoder.gen[PhysStat]
}

case class TerrainSet(terrain: Set[TerrainModel]) extends QueryResponse
object Terrainset {
  implicit val encoder: JsonEncoder[TerrainSet] =
    DeriveJsonEncoder.gen[TerrainSet]
  implicit val decoder: JsonDecoder[TerrainSet] =
    DeriveJsonDecoder.gen[TerrainSet]
}
case class ConsoleResponse(val msg: String) extends QueryResponse
object ConsoleResponse {
  implicit val encoder: JsonEncoder[ConsoleResponse] =
    DeriveJsonEncoder.gen[ConsoleResponse]
  implicit val decoder: JsonDecoder[ConsoleResponse] =
    DeriveJsonDecoder.gen[ConsoleResponse]
}
