package physics

import physics.DESTINATION_TYPE.GRAVITY
import physics.DESTINATION_TYPE.TELEPORT
import physics.DESTINATION_TYPE.WAYPOINT
import zio.IO
import zio.ZIO
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

import java.util.UUID

sealed trait DESTINATION_TYPE

object DESTINATION_TYPE {
  case object WAYPOINT extends DESTINATION_TYPE
  case object TELEPORT extends DESTINATION_TYPE
  case object GRAVITY extends DESTINATION_TYPE
  implicit val encoder: JsonEncoder[DESTINATION_TYPE] =
    DeriveJsonEncoder.gen[DESTINATION_TYPE]
  implicit val decoder: JsonDecoder[DESTINATION_TYPE] =
    DeriveJsonDecoder.gen[DESTINATION_TYPE]
}

trait Destination {
  val uuid: UUID = UUID.randomUUID()
  val dest_type: DESTINATION_TYPE
  val location: Vector[Double]
  val radius: Double
  def serialize: IO[DestinationError, destination] =
    for {
      loc <- ZIO
        .succeed(location(0))
        .zip(ZIO.succeed(location(1)))
        .zip(ZIO.succeed(location(2)))
    } yield destination(dest_type, loc, radius)
}
case class WaypointDestination(location: Vector[Double], radius: Double)
    extends Destination {
  override val dest_type: DESTINATION_TYPE = WAYPOINT
}
case class TeleportDestination(location: Vector[Double], radius: Double)
    extends Destination {
  override val dest_type: DESTINATION_TYPE = TELEPORT
}
case class GravityDestination(location: Vector[Double], radius: Double)
    extends Destination {
  override val dest_type: DESTINATION_TYPE = GRAVITY
}

sealed trait DestinationModel {
  val dest_type: DESTINATION_TYPE
  val location: (Double, Double, Double)
  val radius: Double
  def deserialize: IO[DestinationError, Destination] =
    (for {
      conf <- DestinationModel.config
      f <- ZIO.fromOption(conf.get(dest_type))
    } yield f._1(Vector(location._1, location._2, location._3), radius))
      .orElseFail(
        DestinationDecodingError(
          s"failed to decode destination $dest_type , $location"
        )
      )
}
object DestinationModel {
  type DestinationMapper =
    (
      ((Vector[Double], Double)) => Destination,
      ((Double, Double, Double)) => DestinationModel
    )
  implicit val encoder: JsonEncoder[DestinationModel] =
    DeriveJsonEncoder.gen[DestinationModel]
  implicit val decoder: JsonDecoder[DestinationModel] =
    DeriveJsonDecoder.gen[DestinationModel]

  def config: IO[Nothing, Map[DESTINATION_TYPE, DestinationMapper]] =
    ZIO.succeed(
      Map(
        (
          WAYPOINT,
          (x => WaypointDestination(x._1, x._2), loc => Waypoint(loc))
        ),
        (
          TELEPORT,
          (x => TeleportDestination(x._1, x._2), loc => Teleport(loc))
        ),
        (
          GRAVITY,
          (x => GravityDestination(x._1, x._2), loc => Gravity(loc))
        )
      )
    )
}
case class destination(
  dest_type: DESTINATION_TYPE,
  location: (Double, Double, Double),
  radius: Double
) extends DestinationModel
object destination {
  implicit val encoder: JsonEncoder[destination] =
    DeriveJsonEncoder.gen[destination]
  implicit val decoder: JsonDecoder[destination] =
    DeriveJsonDecoder.gen[destination]
}
case class Waypoint(location: (Double, Double, Double))
    extends DestinationModel {
  override val dest_type: DESTINATION_TYPE = WAYPOINT
  val radius = ???
}
object Waypoint {
  implicit val encoder: JsonEncoder[Waypoint] =
    DeriveJsonEncoder
      .gen[Waypoint]
  implicit val decoder: JsonDecoder[Waypoint] = DeriveJsonDecoder
    .gen[Waypoint]
}

case class Teleport(location: (Double, Double, Double))
    extends DestinationModel {
  override val dest_type: DESTINATION_TYPE = TELEPORT
  override val radius: Double = ???
}

object Teleport {
  implicit val encoder: JsonEncoder[Teleport] =
    DeriveJsonEncoder
      .gen[Teleport]
  implicit val decoder: JsonDecoder[Teleport] = DeriveJsonDecoder
    .gen[Teleport]
}
case class Gravity(location: (Double, Double, Double))
    extends DestinationModel {
  override val dest_type: DESTINATION_TYPE = GRAVITY
  override val radius: Double = ???
}

object Gravity {
  implicit val encoder: JsonEncoder[Gravity] =
    DeriveJsonEncoder
      .gen[Gravity]
  implicit val decoder: JsonDecoder[Gravity] = DeriveJsonDecoder
    .gen[Gravity]
}
trait DestinationError
case class DestinationDecodingError(msg: String) extends DestinationError
