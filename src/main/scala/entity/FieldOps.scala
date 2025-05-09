package entity
import controller.FieldCleared
import controller.QueryResponse
import entity.Ability.ABILITY_ID
import entity.FieldOps.Location
import entity.Globz.GLOBZ_ID
import zio.*

trait FieldOps {
  val field_state: Ref[Map[ABILITY_ID, Chunk[Location]]]
  val occupied_spaces: Ref[Map[Location, Chunk[Location]]]
  def canPlace(
    id: ABILITY_ID,
    location: Location
  ): IO[FieldOpsError, Boolean] = occupiedConflicts(id, location)
    .map(_.isEmpty)
    .flatMap(res =>
      occupied_spaces.get
        .map(res && !_.values.flatten.toSet.contains(location))
    )

  def occupiedConflicts(
    id: ABILITY_ID,
    location: Location
  ): IO[FieldOpsError, Chunk[Location]] = for {
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
  } yield Chunk.from(occupied_conflicts)

  def addAbility(
    id: ABILITY_ID,
    location: Location,
    pocket_count: Int,
    field_count: Int
  ): IO[FieldOpsError, Unit] =
    for {
      zone_req <- Ability
        .zoneRequirement(id)
        .mapBoth(
          _ => CannotRetreiveRequirementsError,
          chunk =>
            chunk.map(offset =>
              (offset._1 + location._1, offset._2 + location._2)
            )
        )
      _ <- (field_state
        .update(state =>
          state.updated(
            id,
            state.getOrElse(id, Chunk()) ++ Chunk(location)
          )
        ) *> occupied_spaces.update(state =>
        state.updated(
          location,
          state.getOrElse(location, Chunk()) ++ zone_req
        )
      ))
        .whenZIO(canPlace(id, location))
        .flatMap(ZIO.fromOption(_))
        .orElseFail(CannotPlaceError)
        .unit
    } yield ()

  def removeAbility(
    id: ABILITY_ID
  ): IO[FieldOpsError, Chunk[Location]] =
    for {
      original_locations <- field_state.get.map(_.getOrElse(id, Chunk()))
      freed_spaces <- ZIO
        .foreachPar(original_locations)(loc =>
          occupied_spaces.get.map(_.getOrElse(loc, Chunk()))
        )
        .map(_.flatten)
      _ <-
        field_state
          .update(state =>
            state.updated(
              id,
              Chunk.empty[Location]
            )
          )

      _ <- ZIO.foreachParDiscard(original_locations)(loc =>
        occupied_spaces.update(state => state.updated(loc, Chunk()))
      )
    } yield freed_spaces

  def clearField(entity_id: GLOBZ_ID): UIO[QueryResponse] =
    (field_state
      .update(_ => Map.empty[ABILITY_ID, Chunk[Location]]) *> occupied_spaces
      .update(_ => Map.empty[Location, Chunk[Location]]))
      .as(FieldCleared(entity_id))

  def getField: UIO[Map[ABILITY_ID, Chunk[Location]]] = field_state.get

  def getFieldCount(ability_id: ABILITY_ID): UIO[Int] =
    field_state.get.map(_.get(ability_id).map(_.size).getOrElse(0))

  def getOccupiedAt(location: Location): UIO[Chunk[Location]] =
    occupied_spaces.get.map(_.getOrElse(location, Chunk()))

  def getOccupied: UIO[Chunk[Location]] =
    occupied_spaces.get.map(x => Chunk.from(x.values.flatten))

}
object FieldOps {
  type Location = (Int, Int)
  def make(): UIO[FieldOps] = BasicFieldOps.make()
}
trait FieldOpsError
case class PlacementError(msg: String) extends FieldOpsError
case object CannotPlaceError extends FieldOpsError
case object CannotRetreiveRequirementsError extends FieldOpsError
case object NoOpsRemovedError extends FieldOpsError

case class BasicFieldOps(
  field_state: Ref[Map[ABILITY_ID, Chunk[Location]]],
  occupied_spaces: Ref[Map[Location, Chunk[Location]]]
) extends FieldOps
case object BasicFieldOps {
  def make(): UIO[BasicFieldOps] = for {
    ref_state <- Ref.make(Map.empty[ABILITY_ID, Chunk[Location]])
    ref_occupied <- Ref.make(Map.empty[Location, Chunk[Location]])
  } yield BasicFieldOps(ref_state, ref_occupied)
}
