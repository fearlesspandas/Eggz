package entity

import entity.SimpleTerrain.TerrainId
import entity.Terrain.TerrainId
import entity.Terrain.distance
import entity.Terrain.is_within_radius
import zio.IO
import zio.Ref
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault

import scala.math.abs

trait SimpleTerrain {
  def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit]
  def remove_terrain(id: TerrainId): IO[TerrainError, Unit]
  def get_terrain(): IO[TerrainError, Seq[SimpleTerrain]]
  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[SimpleTerrain]]
}
object SimpleTerrain {
  type TerrainId = String
}
trait TerrainError
case class TerrainAddError(msg: String) extends TerrainError
case class SimpleTerrainBlock(
  center: Vector[Double],
  radius: Ref[Double],
  terrain: Ref[Seq[Terrain]]
) extends TerrainManager
    with Terrain {
  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] = for {
    _ <- terrain.update(_ :+ SimpleTerrainUnit(id, location))
    rad <- radius.get
    _ <- ZIO.when(distance(location, center) > rad)(
      radius.update(_ => distance(location, center))
    )
  } yield ()

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def get_terrain(): IO[TerrainError, Seq[Terrain]] = ???

  override def get_terrain_within_distance(
    location: Vector[Double],
    dist: Double
  ): IO[TerrainError, Seq[Terrain]] = for {
    rad <- radius.get
    res <- ZIO
      .when(distance(center, location) < dist + rad)(
        terrain.get.flatMap(t =>
          ZIO
            .collectAllPar(t.map(_.get_terrain_within_distance(location, dist)))
            .map(_.flatten)
        )
      )
      .map(_.getOrElse(Seq()))
  } yield res

  override def serialize: TerrainModel = ???
}
case class SimpleTerrainUnit(id: TerrainId, center: Vector[Double])
    extends Terrain {
  override def get_terrain(): IO[TerrainError, Seq[Terrain]] =
    ZIO.succeed(Seq(this))

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): zio.IO[TerrainError, Seq[Terrain]] = ZIO
    .when(is_within_radius(location, center, distance))(ZIO.succeed(Seq(this)))
    .map(_.getOrElse(Seq()))

  override def serialize: TerrainModel = ???
}
object SimpleTerrainUnit {}
