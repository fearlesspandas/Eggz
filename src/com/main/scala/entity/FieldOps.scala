package entity
import entity.Ability.ABILITY_ID
import entity.FieldOps.Location
import zio.*

trait FieldOps {
  val field_state: Ref[Map[ABILITY_ID, Location]]
  def canPlace(
    id: ABILITY_ID,
    location: Location
  ): IO[FieldOpsError, Boolean] = for {
    requirements <- Ability
      .zoneRequirement(id)
      .mapError(err => PlacementError(s"Error while placing id $id : $err"))
    current_state <- field_state.get
    occupied_conflicts <- ZIO.filterPar(current_state.values)(occupied_space =>
      ZIO.succeed(
        (requirements
          .map((x, y) => (location._1 + x, location._2 + y)) :+ location)
          .contains(occupied_space)
      )
    )
  } yield occupied_conflicts.isEmpty

  def addAbility(
    id: ABILITY_ID,
    location: Location
  ): IO[FieldOpsError, Unit] =
    field_state
      .update(_.updated(id, location))
      .whenZIO(canPlace(id, location))
      .someOrElse(CannotPlaceError)
      .unit
}
object FieldOps {
  type Location = (Int, Int)
  def make(): UIO[FieldOps] = BasicFieldOps.make()
}
trait FieldOpsError
case class PlacementError(msg: String) extends FieldOpsError
case object CannotPlaceError extends FieldOpsError

case class BasicFieldOps(field_state: Ref[Map[ABILITY_ID, Location]])
    extends FieldOps
case object BasicFieldOps {
  def make(): UIO[BasicFieldOps] = for {
    ref_state <- Ref.make(Map.empty[ABILITY_ID, Location])
  } yield BasicFieldOps(ref_state)
}
