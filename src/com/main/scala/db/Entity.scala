//package src.com.db
//
//import zio.json._
//
//case class Basis(x: Vector3, y: Vector3, z: Vector3)
//
//object Basis {
//  implicit val decoder: JsonDecoder[Basis] = DeriveJsonDecoder.gen[Basis]
//  implicit val encoder: JsonEncoder[Basis] = DeriveJsonEncoder.gen[Basis]
//}
//
//sealed trait Entity {
//  self =>
//  val id: String
//  val position: Vector3
//  val rotation_angles: Vector3
//  val basis: Basis
//  val speed: Float
//  val selections: Selections
//
//  def copy(
//    id: String = id,
//    position: Vector3 = position,
//    rotation_angles: Vector3 = rotation_angles,
//    basis: Basis = basis,
//    speed: Float = speed,
//    selections: Selections = selections
//  ): Entity
//
//  def takeAction(action: Action): Entity
//
//  def move_to(loc: Vector3): Entity =
//    this.copy(position = loc)
//
//  def rotate_to(rot: Vector3): Entity = this.copy(rotation_angles = rot)
//}
//
//object Entity {
//  implicit val decoder: JsonDecoder[Entity] = DeriveJsonDecoder.gen[Entity]
//  implicit val encoder: JsonEncoder[Entity] = DeriveJsonEncoder.gen[Entity]
//}
//
//case class Player(
//  id: String,
//  position: Vector3 = Vector3(0, 0, 0),
//  rotation_angles: Vector3 = Vector3(0, 0, 0),
//  basis: Basis = Basis(RIGHT.normal, UP.normal, FORWARD.normal),
//  speed: Float = 1,
//  selections: Selections = Selections.empty
//) extends Entity {
//  def takeAction(action: Action): Entity = action match {
//    case UpdateUserDirection(key, value) if key == id =>
//      val (a, b) = value
//      if (a == b)
//        copy(position = (basis.z * a.normal.z + basis.x * a.normal.x).normalize * speed)
//      else
//        copy(position = (basis.z * scala.math.max(a.normal.z, b.normal.z) +
//          basis.x * scala.math.max(a.normal.x, b.normal.x)).normalize * speed
//        )
//    case _ => this
//  }
//
//  def copy(
//    id: String = id,
//    position: Vector3 = position,
//    rotation_angles: Vector3 = rotation_angles,
//    basis: Basis = basis,
//    speed: Float = speed,
//    selections: Selections
//  ): Entity = Player(id, position, rotation_angles, basis, speed, selections)
//}
//
//object Player {
//  implicit val decoder: JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]
//  implicit val encoder: JsonEncoder[Player] = DeriveJsonEncoder.gen[Player]
//}
