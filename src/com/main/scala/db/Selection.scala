package src.com.db

import zio.json._

sealed trait Selection {
  val id: Long
  val position: Vector3
  def within_epsilon(entity: Entity): Entity
}

object Selection {
  val PATH_FINDING_EPSILON = 10
  implicit val decoder: JsonDecoder[Selection] = DeriveJsonDecoder.gen[Selection]
  implicit val encoder: JsonEncoder[Selection] = DeriveJsonEncoder.gen[Selection]
}

case class Waypoint(id: Long, position: Vector3) extends Selection {
  override def within_epsilon(entity: Entity): Entity =
    entity.copy(selections = entity.selections.remove_waypoint(id))
}
object Waypoint {
  implicit val decoder: JsonDecoder[Waypoint] = DeriveJsonDecoder.gen[Waypoint]
  implicit val encoder: JsonEncoder[Waypoint] = DeriveJsonEncoder.gen[Waypoint]
}

case class Teleport(id: Long, position1: Vector3, position2: Vector3) extends Selection {
  override val position: Vector3 = position1
  override def within_epsilon(entity: Entity): Entity =
    entity.move_to(position2).copy(selections = entity.selections.remove_waypoint(id))
}
object Teleport {
  implicit val decoder: JsonDecoder[Teleport] = DeriveJsonDecoder.gen[Teleport]
  implicit val encoder: JsonEncoder[Teleport] = DeriveJsonEncoder.gen[Teleport]
}

sealed trait Selections {
  val selections: Seq[Selection]
  val selection_map: Map[Long, Selection]

  def add_waypoint(sel: Selection) = {
    val curr = this
    new Selections {
      override val selections: Seq[Selection] = sel +: curr.selections
      override val selection_map: Map[Long, Selection] = curr.selection_map.updated(sel.id, sel)
    }
  }
  def remove_waypoint(id: Long): Selections = {
    val curr = this
    new Selections {
      override val selections = curr.selections.filterNot(_.id == id)
      override val selection_map: Map[Long, Selection] = curr.selection_map.filterNot({
        case (k, v) => k == id
      })
    }
  }
}
case class SelectionsInstance(selections: Seq[Selection], selection_map: Map[Long, Selection])
    extends Selections
object SelectionsInstance {
  implicit val decoder: JsonDecoder[SelectionsInstance] = DeriveJsonDecoder.gen[SelectionsInstance]
  implicit val encoder: JsonEncoder[SelectionsInstance] = DeriveJsonEncoder.gen[SelectionsInstance]
}

object Selections {
  implicit val decoder: JsonDecoder[Selections] = JsonDecoder[Seq[Selection]].map(s => SelectionsInstance(s,s.map(s_ => s_.id -> s_).toMap))
  implicit val encoder: JsonEncoder[Selections] = JsonEncoder[Seq[Selection]].contramap(_.selections)
  def empty: Selections = new Selections {
    override val selections = Seq()
    override val selection_map: Map[Long, Selection] = Map()
  }
}
