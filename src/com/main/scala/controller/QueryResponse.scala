package controller

import entity.EggzModel
import entity.GlobzModel
import entity.TerrainModel
import entity.TerrainRegionM
import entity.TerrainUnitM
import entity.Terrain.TerrainId
import physics.DEST
import physics.DESTINATION_TYPE
import physics.DestinationModel
import physics.Mode
import physics.PhysicsCommand
import physics.destination
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz
import zio.Chunk
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

import java.util.UUID

sealed trait QueryResponse {}

object QueryResponse {
  case object Empty extends QueryResponse
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
case class HealthSet(id: GLOBZ_ID, value: Double) extends QueryResponse
object HealthSet {
  implicit val encoder: JsonEncoder[HealthSet] =
    DeriveJsonEncoder.gen[HealthSet]
  implicit val decoder: JsonDecoder[HealthSet] =
    DeriveJsonDecoder.gen[HealthSet]
}
case class NextDestination(id: ID, destination: DEST) extends QueryResponse
object NextDestination {
  implicit val encoder: JsonEncoder[NextDestination] =
    DeriveJsonEncoder.gen[NextDestination]
  implicit val decoder: JsonDecoder[NextDestination] =
    DeriveJsonDecoder.gen[NextDestination]
}

case class ActiveDestination(id: ID, destination: UUID) extends QueryResponse
object ActiveDestination {
  implicit val encoder: JsonEncoder[ActiveDestination] =
    DeriveJsonEncoder.gen[ActiveDestination]
  implicit val decoder: JsonDecoder[ActiveDestination] =
    DeriveJsonDecoder.gen[ActiveDestination]
}
case class NextIndex(id: ID, index: Int) extends QueryResponse
object NextIndex {
  implicit val encoder: JsonEncoder[NextIndex] =
    DeriveJsonEncoder.gen[NextIndex]
  implicit val decoder: JsonDecoder[NextIndex] =
    DeriveJsonDecoder.gen[NextIndex]
}
case class TeleportToNext(id: ID, location: (Double, Double, Double))
    extends QueryResponse
object TeleportToNext {
  implicit val encoder: JsonEncoder[TeleportToNext] =
    DeriveJsonEncoder.gen[TeleportToNext]
  implicit val decoder: JsonDecoder[TeleportToNext] =
    DeriveJsonDecoder.gen[TeleportToNext]
}
case class AllDestinations(id: ID, destinations: Seq[DEST])
    extends QueryResponse
object AllDestinations {
  implicit val encoder: JsonEncoder[AllDestinations] =
    DeriveJsonEncoder.gen[AllDestinations]
  implicit val decoder: JsonDecoder[AllDestinations] =
    DeriveJsonDecoder.gen[AllDestinations]
}
case class NewDestination(id: ID, destination: DEST) extends QueryResponse
object NewDestination {
  implicit val encoder: JsonEncoder[NewDestination] =
    DeriveJsonEncoder.gen[NewDestination]
  implicit val decoder: JsonDecoder[NewDestination] =
    DeriveJsonDecoder.gen[NewDestination]
}

case class DeleteDestination(id: ID, uuid: UUID) extends QueryResponse
object DeleteDestination {
  implicit val encoder: JsonEncoder[DeleteDestination] =
    DeriveJsonEncoder.gen[DeleteDestination]
  implicit val decoder: JsonDecoder[DeleteDestination] =
    DeriveJsonDecoder.gen[DeleteDestination]
}
case class ModeSet(mode: Mode) extends QueryResponse
object ModeSet {
  implicit val encoder: JsonEncoder[ModeSet] =
    DeriveJsonEncoder.gen[ModeSet]
  implicit val decoder: JsonDecoder[ModeSet] =
    DeriveJsonDecoder.gen[ModeSet]
}
case class ClearDestinations() extends QueryResponse
object ClearDestinations {
  implicit val encoder: JsonEncoder[ClearDestinations] =
    DeriveJsonEncoder.gen[ClearDestinations]
  implicit val decoder: JsonDecoder[ClearDestinations] =
    DeriveJsonDecoder.gen[ClearDestinations]
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
case class GravityActive(id: GLOBZ_ID, is_active: Boolean) extends QueryResponse
object GravityActive {
  implicit val encoder: JsonEncoder[GravityActive] =
    DeriveJsonEncoder.gen[GravityActive]
  implicit val decoder: JsonDecoder[GravityActive] =
    DeriveJsonDecoder.gen[GravityActive]
}
case class DestinationsActive(id: GLOBZ_ID, is_active: Boolean)
    extends QueryResponse
object DestinationsActive {
  implicit val encoder: JsonEncoder[DestinationsActive] =
    DeriveJsonEncoder.gen[DestinationsActive]
  implicit val decoder: JsonDecoder[DestinationsActive] =
    DeriveJsonDecoder.gen[DestinationsActive]
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
case class PhysStat(id: GLOBZ_ID, max_speed: Double, speed: Double)
    extends QueryResponse
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
case class TerrainRegionSet(
  terrain: TerrainRegionM
) extends QueryResponse
object TerrainRegionSet {
  implicit val encoder: JsonEncoder[TerrainRegionSet] =
    DeriveJsonEncoder.gen[TerrainRegionSet]
  implicit val decoder: JsonDecoder[TerrainRegionSet] =
    DeriveJsonDecoder.gen[TerrainRegionSet]
}
case class PaginatedResponse(responses: Chunk[QueryResponse])
    extends QueryResponse
object PaginatedResponse {
  implicit val encoder: JsonEncoder[PaginatedResponse] =
    DeriveJsonEncoder.gen[PaginatedResponse]
  implicit val decoder: JsonDecoder[PaginatedResponse] =
    DeriveJsonDecoder.gen[PaginatedResponse]
}
case class MultiResponse(responses: Chunk[QueryResponse]) extends QueryResponse
object MultiResponse { // todo should maybe remove this serialization logic as we should never actually use it
  implicit val encoder: JsonEncoder[MultiResponse] =
    DeriveJsonEncoder.gen[MultiResponse]
  implicit val decoder: JsonDecoder[MultiResponse] =
    DeriveJsonDecoder.gen[MultiResponse]
}
case class StartPagination(tpe: String, count: Int) extends QueryResponse
object StartPagination {
  implicit val encoder: JsonEncoder[StartPagination] =
    DeriveJsonEncoder.gen[StartPagination]
  implicit val decoder: JsonDecoder[StartPagination] =
    DeriveJsonDecoder.gen[StartPagination]
}
case class EndPagination(tpe: String) extends QueryResponse
object EndPagination {
  implicit val encoder: JsonEncoder[EndPagination] =
    DeriveJsonEncoder.gen[EndPagination]
  implicit val decoder: JsonDecoder[EndPagination] =
    DeriveJsonDecoder.gen[EndPagination]
}
case class ConsoleResponse(val msg: String) extends QueryResponse
object ConsoleResponse {
  implicit val encoder: JsonEncoder[ConsoleResponse] =
    DeriveJsonEncoder.gen[ConsoleResponse]
  implicit val decoder: JsonDecoder[ConsoleResponse] =
    DeriveJsonDecoder.gen[ConsoleResponse]
}
case class Completed() extends QueryResponse
case class QueuedServerMessage(responses: Chunk[QueryResponse])
    extends QueryResponse
case class QueuedClientMessage(id: ID, responses: Chunk[QueryResponse])
    extends QueryResponse
case class QueuedPhysicsMessage(messages: Chunk[PhysicsCommand])
    extends QueryResponse

case class TerrainUnitm(
  uuid: UUID,
  location: (Double, Double, Double),
  entities: Map[TerrainId, Int]
) extends QueryResponse
object TerrainUnitm {
  implicit val encoder: JsonEncoder[TerrainUnitm] =
    DeriveJsonEncoder.gen[TerrainUnitm]
  implicit val decoder: JsonDecoder[TerrainUnitm] =
    DeriveJsonDecoder.gen[TerrainUnitm]
}
case class TerrainChunkm(
  uuid: UUID,
  location: (Double, Double, Double),
  radius: Double
) extends QueryResponse
object TerrainChunkm {
  implicit val encoder: JsonEncoder[TerrainChunkm] =
    DeriveJsonEncoder.gen[TerrainChunkm]
  implicit val decoder: JsonDecoder[TerrainChunkm] =
    DeriveJsonDecoder.gen[TerrainChunkm]
}
case class EmptyChunk(
  uuid: UUID,
  location: (Double, Double, Double),
  radius: Double
) extends QueryResponse
object EmptyChunk {
  implicit val encoder: JsonEncoder[EmptyChunk] =
    DeriveJsonEncoder.gen[EmptyChunk]
  implicit val decoder: JsonDecoder[EmptyChunk] =
    DeriveJsonDecoder.gen[EmptyChunk]
}
case class TerrainRegionm(
  terrain: Set[(Vector[Double], Map[TerrainId, Int], UUID)]
) extends QueryResponse
object TerrainRegionm {
  implicit val decoder: JsonDecoder[TerrainRegionm] =
    DeriveJsonDecoder.gen[TerrainRegionm]
  implicit val encoder: JsonEncoder[TerrainRegionm] =
    DeriveJsonEncoder.gen[TerrainRegionm]
}
