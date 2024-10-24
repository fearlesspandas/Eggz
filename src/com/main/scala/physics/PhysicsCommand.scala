package physics
import zio.json._
sealed trait PhysicsCommand {}
object PhysicsCommand {
  implicit val encoder: JsonEncoder[PhysicsCommand] =
    DeriveJsonEncoder.gen[PhysicsCommand]

  implicit val decoder: JsonDecoder[PhysicsCommand] =
    DeriveJsonDecoder.gen[PhysicsCommand]
}
case class Loc(id: String, loc: Vector[Double]) extends PhysicsCommand
object Loc {
  implicit val encoder: JsonEncoder[Loc] =
    DeriveJsonEncoder.gen[Loc]

  implicit val decoder: JsonDecoder[Loc] =
    DeriveJsonDecoder.gen[Loc]
}
case class SetInputLock(id: String, value: Boolean) extends PhysicsCommand
case class PhysicsTeleport(id: String, location: (Double, Double, Double))
    extends PhysicsCommand
object serializationTest {

  import zio.json._

  def main(args: Array[String]): Unit = {
    val str = Loc("test", Vector(0, 0, 0))
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
