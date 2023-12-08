package entity

import entity.Terrain.TerrainId
import entity.TerrainBlock.generate_lattice
import zio.IO
import zio.Ref
import zio.ZIO

trait Terrain {
  def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit]
  def remove_terrain(id: TerrainId): IO[TerrainError, Unit]
  def get_terrain(): IO[TerrainError, Unit]
  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Unit]
}
object Terrain {
  type TerrainId = String
}
trait TerrainError

//Vector(1 , -1 , 1)
//modify to take Ref[TerrainBlock] in map to avoid writing update logic
case class TerrainBlock(
  center: Vector[Double],
  terrainRef: Ref[Map[Vector[Double], TerrainBlock] | TerrainId],
  size: Double
) extends Terrain {

  def base(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, TerrainBlock] = for {
    ref <- Ref.make[Map[Vector[Double], TerrainBlock] | TerrainId](id)
  } yield TerrainBlock(location, ref, 0)

  def generate_quadrants(): IO[TerrainError, Seq[Vector[Double]]] = for {
    res <- ZIO.succeed(
      generate_lattice(Vector((0 to center.length).map(_ => 0.0): _*))
    )
  } yield res

  def is_within_block(location: Vector[Double]): IO[TerrainError, Boolean] =
    ??? // compute manhattan distance from center for max bounds

  def get_quadrant(
    location: Vector[Double]
  ): IO[TerrainError, Option[Vector[Double]]] =
    for {
      in_block <- is_within_block(location)
      res =
        if in_block then {
          Some(location.zip(center).map((l, c) => if l >= c then 1.0 else -1.0))
        } else None
    } yield res

  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] =
    (for {
      quad <- get_quadrant(location).flatMap(ZIO.fromOption(_))
      _ <- terrainRef.get.flatMap(x =>
        x match {
          case m: Map[Vector[Double], TerrainBlock] =>
            ZIO.fromOption(m.get(quad)).flatMap(_.add_terrain(id, location))
          case t: TerrainId => base(id, location)
        }
      )
    } yield ()).orElseFail(???)

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def get_terrain(): IO[TerrainError, Unit] = ???

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Unit] = ???
}
object TerrainBlock {
  type QuadrantMap[+A, +B] = Map[Int, Map[Int, A] | B]
  type M =
    QuadrantMap[Map[Int, QuadrantMap[Any, Any]], (TerrainId, Vector[Double])]

  def generate_lattice(
    src: Vector[Double],
    ind: Int = 0,
    accum: Seq[Vector[Double]] = Seq()
  ): Seq[Vector[Double]] =
    if ind >= src.length then Seq(src)
    else
      accum ++ generate_lattice(
        src.updated(ind, 1),
        ind + 1,
        Seq()
      ) ++ generate_lattice(src.updated(ind, -1), ind + 1, Seq())

  def main(args: Array[String]): Unit = {
    val res = generate_lattice(Vector(0, 0, 0, 0, 0))
    println(res)
    println(res.toSet.size)
  }
}
