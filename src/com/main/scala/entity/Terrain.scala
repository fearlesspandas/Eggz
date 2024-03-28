package entity

import entity.Terrain.TerrainId
import zio.IO
import zio.Ref
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault

import scala.math.abs

trait Terrain {
  def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit]
  def remove_terrain(id: TerrainId): IO[TerrainError, Unit]
  def get_terrain(): IO[TerrainError, Seq[Terrain]]
  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]]
}
object Terrain {
  type TerrainId = String
}
trait TerrainError
case class TerrainAddError(msg: String) extends TerrainError

case class TerrainBlock(
  centerRef: Ref[Vector[Double]],
  size: Ref[Int],
  radius: Double
) extends Terrain {
  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] = ???

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def get_terrain(): IO[TerrainError, Seq[Terrain]] = ???

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]] = ???
}
object TerrainBlock extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
}
