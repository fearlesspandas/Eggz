package entity
import entity.Ability.ABILITY_ID
import entity.FieldOps.Location
import zio.*

import scala.:+

trait FieldOps {
  val field_state: Ref[Map[ABILITY_ID, Chunk[Location]]]
  def canPlace(
    id: ABILITY_ID,
    location: Location
  ): IO[FieldOpsError, Boolean] = for {
    requirements <- Ability
      .zoneRequirement(id)
      .mapError(err => PlacementError(s"Error while placing id $id : $err"))
    current_state <- field_state.get
    occupied_conflicts <- ZIO.filterPar(current_state.values.flatten)(
      occupied_space =>
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
      .update(state =>
        state.updated(id, state.getOrElse(id, Chunk()) ++ Chunk(location))
      )
      .whenZIO(canPlace(id, location))
      .someOrElse(CannotPlaceError)
      .unit

  def removeAbility(
    id: ABILITY_ID
  ): IO[FieldOpsError, Chunk[Location]] =
    for {
      res <- field_state.get.map(_.getOrElse(id, Chunk()))
      _ <-
        field_state
          .update(state =>
            state.updated(
              id,
              Chunk.empty[Location]
            )
          )
    } yield res

  def getField(): UIO[Map[ABILITY_ID, Chunk[Location]]] = field_state.get

  def getFieldCount(ability_id: ABILITY_ID): UIO[Int] =
    field_state.get.map(_.getOrElse(ability_id, Chunk()).size)
}
object FieldOps {
  type Location = (Int, Int)
  def make(): UIO[FieldOps] = BasicFieldOps.make()
}
trait FieldOpsError
case class PlacementError(msg: String) extends FieldOpsError
case object CannotPlaceError extends FieldOpsError
case object NoOpsRemovedError extends FieldOpsError

case class BasicFieldOps(field_state: Ref[Map[ABILITY_ID, Chunk[Location]]])
    extends FieldOps
case object BasicFieldOps {
  def make(): UIO[BasicFieldOps] = for {
    ref_state <- Ref.make(Map.empty[ABILITY_ID, Chunk[Location]])
  } yield BasicFieldOps(ref_state)
}
