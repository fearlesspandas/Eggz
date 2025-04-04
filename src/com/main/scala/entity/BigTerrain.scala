package entity

import entity.Terrain.TerrainId
import entity.Terrain.is_within_disance
import entity.Terrain.is_within_range
import zio.*

import java.util.UUID
trait BigTerrain extends Terrain {}
case class BigTerrainUnit(
  location: Vector[Double],
  radius: Double,
  entitiesRef: Ref[Map[TerrainId, Int]]
) extends BigTerrain {

  final override def get_terrain(): IO[TerrainError, Seq[Terrain]] =
    ZIO.succeed(Seq(this))

  final override def get_terrain_within_distance(
    loc: Vector[Double],
    distance: Double
  ): UIO[Seq[Terrain]] =
    if (is_within_range(location, loc, distance + 2 * this.radius)) {
      ZIO.logError("Found BigTerrain") *>
        ZIO.succeed(Seq(this))
    } else ZIO.succeed(Seq())
  override def serialize_relative(
    location: Vector[Double],
    radius: Double
  ): IO[TerrainError, Set[TerrainModel]] = ???

  final override def serialize(): IO[TerrainError, Set[TerrainModel]] =
    for {
      entities <- entitiesRef.get
    } yield Set(TerrainUnitM(location, entities, uuid))
}
case class BigTerrainRegion(
  ref: Ref[Chunk[BigTerrainUnit]]
) extends BigTerrain {

  def add_terrain(
    id: TerrainId,
    location: Vector[Double],
    radius: Double
  ): UIO[Unit] = for {
    entitiesRef <- Ref.make(Map((id, 1)))
    _ <- ref.update(
      _.appended(BigTerrainUnit(location, radius, entitiesRef))
    )
  } yield ()

  final override def get_terrain(): IO[TerrainError, Seq[Terrain]] = for {
    res <- ref.get.flatMap(t => ZIO.foreachPar(t)(_.get_terrain()))
  } yield res.flatten

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): UIO[Seq[Terrain]] = ref.get
    .flatMap(
      ZIO
        .foreachPar(_)(terrainUnit =>
          terrainUnit.get_terrain_within_distance(
            location,
            distance
          )
        )
    )
    .map(_.flatten)

  final override def serialize(): IO[TerrainError, Set[TerrainModel]] = for {
    r1 <-
      get_terrain()
    r2 <- ZIO.foreachPar(r1)(
      _.serialize()
    )
  } yield r2.flatten.toSet

  override def serialize_relative(
    location: Vector[Double],
    radius: Double
  ): IO[TerrainError, Set[TerrainModel]] = ???
}
object BigTerrainRegion {
  def make: UIO[BigTerrainRegion] = for {
    units <- Ref.make(Chunk.empty[BigTerrainUnit])
  } yield BigTerrainRegion(units)

}
