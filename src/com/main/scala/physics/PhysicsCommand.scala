package physics
import zio.json._
sealed trait PhysicsCommand {}
object PhysicsCommand {
  implicit val encoder: JsonEncoder[PhysicsCommand] =
    DeriveJsonEncoder.gen[PhysicsCommand]

  implicit val decoder: JsonDecoder[PhysicsCommand] =
    DeriveJsonDecoder.gen[PhysicsCommand]
}
case class SendLocation(id: String, loc: Vector[Double]) extends PhysicsCommand
object SendLocation {
  implicit val encoder: JsonEncoder[SendLocation] =
    DeriveJsonEncoder.gen[SendLocation]

  implicit val decoder: JsonDecoder[SendLocation] =
    DeriveJsonDecoder.gen[SendLocation]
}
