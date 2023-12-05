package physics

import controller.NextDestination
import controller.QueryResponse
import entity.Player
import physics.DESTINATION_TYPE.WAYPOINT
import zio.IO
import zio.ZIO
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.EncoderOps
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait DESTINATION_TYPE

object DESTINATION_TYPE {

  case object WAYPOINT extends DESTINATION_TYPE

  implicit val encoder: JsonEncoder[DESTINATION_TYPE] = DeriveJsonEncoder.gen[DESTINATION_TYPE]
  implicit val decoder: JsonDecoder[DESTINATION_TYPE] = DeriveJsonDecoder.gen[DESTINATION_TYPE]
}

trait Destination {
  val dest_type: DESTINATION_TYPE
  val location: Vector[Double]
  def serialize: IO[DestinationError, destination] =
    (for {
      loc <- ZIO
        .succeed(location(0))
        .zip(ZIO.succeed(location(1)))
        .zip(ZIO.succeed(location(2)))
//      conf <- DestinationModel.config
    } yield destination(dest_type, loc)) //.orElseFail(DestinationDecodingError(""))
}
case class WaypointDestination(location: Vector[Double]) extends Destination {
  override val dest_type: DESTINATION_TYPE = WAYPOINT
}

sealed trait DestinationModel {
  val dest_type: DESTINATION_TYPE
  val location: (Double, Double, Double)
  def deserialize: IO[DestinationError, Destination] =
    (for {
      conf <- DestinationModel.config
      f <- ZIO.fromOption(conf.get(dest_type))
    } yield f._1(Vector(location._1, location._2, location._3)))
      .orElseFail(DestinationDecodingError(s"failed to decode destination $dest_type , $location"))
}
object DestinationModel {
  type DestinationMapper =
    (Vector[Double] => Destination, ((Double, Double, Double)) => DestinationModel)
  implicit val encoder: JsonEncoder[DestinationModel] = DeriveJsonEncoder.gen[DestinationModel]
  implicit val decoder: JsonDecoder[DestinationModel] = DeriveJsonDecoder.gen[DestinationModel]

  def config: IO[Nothing, Map[DESTINATION_TYPE, DestinationMapper]] =
    ZIO.succeed(
      Map(
        (
          WAYPOINT,
          (x => WaypointDestination(x), loc => Waypoint(loc))
        )
      )
    )

}
case class destination(dest_type: DESTINATION_TYPE, location: (Double, Double, Double))
    extends DestinationModel
object destination {
  implicit val encoder: JsonEncoder[destination] = DeriveJsonEncoder.gen[destination]
  implicit val decoder: JsonDecoder[destination] = DeriveJsonDecoder.gen[destination]
}
case class Waypoint(location: (Double, Double, Double)) extends DestinationModel {
  override val dest_type: DESTINATION_TYPE = WAYPOINT
}
object Waypoint {
  implicit val encoder: JsonEncoder[Waypoint] =
    DeriveJsonEncoder
      .gen[Waypoint]
  implicit val decoder: JsonDecoder[Waypoint] = DeriveJsonDecoder
    .gen[Waypoint]
}

object test {
  def main(args: Array[String]): Unit = {
    println((DESTINATION_TYPE.WAYPOINT: DESTINATION_TYPE).toJson)
    println((Waypoint((0, 0, 0)): DestinationModel).toJson)
    println(
      (NextDestination("1", destination(WAYPOINT, (0, 0, 0))): QueryResponse).toJson
    )
  }
}
trait DestinationError
case class DestinationDecodingError(msg: String) extends DestinationError
