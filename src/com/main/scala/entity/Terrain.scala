package entity

import entity.Terrain.TerrainId
import entity.TerrainBlock.generate_lattice
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

case class TerrainBlock(
  centerRef: Ref[Vector[Double]],
  terrainRef: Ref[Map[Vector[Double], Ref[Terrain]] | TerrainId],
  size: Ref[Int],
  radius: Double
) extends Terrain {

  def base(
    id: TerrainId,
    location: Vector[Double],
    size: Int,
    radius: Double
  ): IO[TerrainError, TerrainBlock] = for {
    ref <- Ref.make[Map[Vector[Double], Ref[Terrain]] | TerrainId](
      if radius > 1 then Map() else id
    )
    loc <- Ref.make(location)
    sz <- Ref.make(0)
  } yield TerrainBlock(loc, ref, sz, radius)

  def generate_quadrants(): IO[TerrainError, Seq[Vector[Double]]] = for {
    center <- centerRef.get
    res <- ZIO.succeed(
      generate_lattice(Vector((0 to center.length).map(_ => 0.0): _*))
    )
  } yield res

  // manhattan distance used to test for inclusion within block
  def is_within_block(location: Vector[Double]): IO[TerrainError, Boolean] =
    for {
      center <- centerRef.get
      diffs = center.zip(location).map((a, b) => abs(a - b))
      res = diffs.forall(diff => diff < radius)
    } yield res

  def get_quadrant(
    location: Vector[Double]
  ): IO[TerrainError, Option[Vector[Double]]] =
    for {
      center <- centerRef.get
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
      quadr <- get_quadrant(location).flatMap(ZIO.fromOption(_))
      _ <- terrainRef.get.flatMap {
        case m: Map[Vector[Double], Ref[Terrain]] =>
          for {
            sz <- size.get
            bkp_ <- base(id, location, 0, radius / 2)
            bkp <- Ref.make[Terrain](bkp_)
            // if quadrant is a terrain block -> defer add_terrain to the sub-block
            // otherwise if no block exists, we add a default block.
            // The default will either represent another sub block (if radius > 1)
            // or a single point containing an entity id (if radius <= 1)
            block <- ZIO
              .fromOption(m.get(quadr))
              .flatMap(_.get.map { case t: TerrainBlock => t })
              .flatMapError(_ =>
                terrainRef.update { case m: Map[Vector[Double], Ref[Terrain]] =>
                  m.updated(quadr, bkp)
                }
              )
              .fold(_ => bkp_, x => x)
            _ <- block.add_terrain(id, location)
            _ <- block.size.update(_ + 1)
            _ <- size.update(_ + 1)
          } yield ()

        case t: TerrainId => ZIO.unit
      }
    } yield ()).orElseFail(???)

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def get_terrain(): IO[TerrainError, Unit] = ???

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Unit] = ???
}
object TerrainBlock extends ZIOAppDefault {

  private type TerrainRef = Map[Vector[Double], Ref[Terrain]] | TerrainId

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    cr <- Ref.make(Vector(0.0, 0, 0))
    tr <- Ref.make[TerrainRef](Map())
    sz <- Ref.make(0)
    tb = TerrainBlock(cr, tr, sz, 10)
    _ <- tb.add_terrain("1", Vector(5, 5, 5))
    _ <- tb.add_terrain("2", Vector(5, -5, 5))
    _ <- ZIO.log(tb.toString)
    sz <- tb.size.get
    _ <- ZIO.log(sz.toString)
  } yield ()

  // generates a powerset of vectors that model
  // all combinations of (1,-1) in each parameter
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
}
