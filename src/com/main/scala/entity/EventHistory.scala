package entity
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.*

enum EventType {
  case DamageDealt
}
trait Event {
  val event_type: EventType

}
case class DamageDealt(
  to: GLOBZ_ID,
  amount: Double,
  time: Option[String] = None
) {

  val event_type = EventType.DamageDealt
}
trait EventHistory {
  val ref: Ref[Map[EventType, Chunk[Event]]]
  def addEvent(event: Event) = ref
}
