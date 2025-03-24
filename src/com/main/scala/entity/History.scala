package entity

import entity.Ability.ABILITY_ID
import entity.EventType.ABILITY_OBTAINED
import entity.EventType.NPC_KILLED
import src.com.main.scala.entity.Globz.GLOBZ_ID

trait EventQuery
case object ALL extends EventQuery
case class MATCHING() extends EventQuery
enum EventType {
  case NPC_KILLED
  case ABILITY_OBTAINED
}
trait Event {
  val event_type: EventType
}
case class NPCKilled(id: GLOBZ_ID) extends Event {
  override val event_type: EventType = NPC_KILLED
}
case class AbilityObtained(id: ABILITY_ID) extends Event {
  override val event_type: EventType = ABILITY_OBTAINED
}
trait History {}
