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
case class SetInputLock(id: String, value: Boolean) extends PhysicsCommand
object serializationTest {

  import zio.json._

  def main(args: Array[String]): Unit = {
    val str = SendLocation("test", Vector(0, 0, 0))
    println(str.toJson)
    val cmd =
      """{
        |"SendLocation":{
        | "id":"test",
        | "loc":[0.0,0.0,0.0]
        |   }
        |}""".stripMargin
    val res = cmd.fromJson[PhysicsCommand]
    if (res.isLeft) { println(res.left) }
    assert(res.isRight)
  }
}
