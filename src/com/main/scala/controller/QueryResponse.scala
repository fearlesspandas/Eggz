package controller

import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait QueryResponse {}

object QueryResponse {
  implicit val encoder: JsonEncoder[QueryResponse] = DeriveJsonEncoder.gen[QueryResponse]
  implicit val decoder: JsonDecoder[QueryResponse] = DeriveJsonDecoder.gen[QueryResponse]
}

case class GlobSet(globs: Set[Globz.Glob]) extends QueryResponse
object GlobSet {
  implicit val encoder: JsonEncoder[GlobSet] = DeriveJsonEncoder.gen[Set[Globz.Glob]]
  implicit val decoder: JsonDecoder[GlobSet] = DeriveJsonDecoder.gen[GlobSet]
}
case class EggSet(eggs: Set[Eggz.Service]) extends QueryResponse
object EggSet {
  implicit val encoder: JsonEncoder[EggSet] = DeriveJsonEncoder.gen[EggSet]
  implicit val decoder: JsonDecoder[EggSet] = DeriveJsonDecoder.gen[EggSet]
}
case class EntityIDSet(ids: Set[GLOBZ_ID]) extends QueryResponse
object EntityIDSet {
  implicit val encoder: JsonEncoder[EntityIDSet] = DeriveJsonEncoder.gen[EntityIDSet]
  implicit val decoder: JsonDecoder[EntityIDSet] = DeriveJsonDecoder.gen[EntityIDSet]
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

case class NextDestination(id: ID, location: (Double, Double, Double)) extends QueryResponse
object NextDestination {
  implicit val encoder: JsonEncoder[NextDestination] = DeriveJsonEncoder.gen[NextDestination]
  implicit val decoder: JsonDecoder[NextDestination] = DeriveJsonDecoder.gen[NextDestination]
}
case class Location(id: GLOBZ_ID, location: (Double, Double, Double)) extends QueryResponse
object Location {
  implicit val encoder: JsonEncoder[Location] = DeriveJsonEncoder.gen[Location]
  implicit val decoder: JsonDecoder[Location] = DeriveJsonDecoder.gen[Location]
}
case class Blob(blob: Option[Globz.Glob]) extends QueryResponse
object Blob {
  implicit val encoder: JsonEncoder[Blob] = DeriveJsonEncoder.gen[Blob]
  implicit val decoder: JsonDecoder[Blob] = DeriveJsonDecoder.gen[Blob]
}
