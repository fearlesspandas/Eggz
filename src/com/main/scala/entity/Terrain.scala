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
  ): IO[TerrainError, Seq[Terrain]]
}
object Terrain {
  type TerrainId = String
}
trait TerrainError
case class TerrainAddError(msg: String) extends TerrainError

case class TerrainBlock(
  centerRef: Ref[Vector[Double]],
  terrainRef: Ref[Map[Vector[Int], Ref[Terrain]] | TerrainId],
  size: Ref[Int],
  radius: Double
) extends Terrain {

  def base(
    id: TerrainId,
    location: Vector[Double],
    size: Int,
    radius: Double,
    quad: Vector[Int]
  ): IO[TerrainError, TerrainBlock] = for {
    center <- centerRef.get
    sub_block_origin =
      if quad.length > 0 && radius > 1 then
        center.zip(quad).map((a, b) => a + (b * radius))
      else location
    ref <- Ref.make[Map[Vector[Int], Ref[Terrain]] | TerrainId](
      if radius > 1 then Map()
      else {
        println(s"new id $id , $location")
        id
      }
    )
    loc <- Ref.make(sub_block_origin)
    sz <- Ref.make(0)
  } yield TerrainBlock(loc, ref, sz, radius)

  def generate_quadrants(): IO[TerrainError, Seq[Vector[Double]]] = for {
    center <- centerRef.get
    res <- ZIO.succeed(
      generate_lattice(Vector((0 to center.length).map(_ => 0.0): _*))
    )
  } yield res

  def is_within_disance(
    location: Vector[Double],
    center: Vector[Double],
    distance: Double
  ): Boolean = {
    val diffs = center.zip(location).map((a, b) => abs(a - b))
    diffs.forall(diff => diff <= radius)
  }
  // manhattan distance used to test for inclusion within block
  def is_within_block(location: Vector[Double]): IO[TerrainError, Boolean] =
    for {
      center <- centerRef.get
    } yield is_within_disance(location, center, radius)

  def get_quadrant(
    location: Vector[Double]
  ): IO[TerrainError, Option[Vector[Int]]] =
    for {
      center <- centerRef.get
      in_block <- is_within_block(location)
      res =
        if in_block then {
          Some(location.zip(center).map((l, c) => if l >= c then 1 else -1))
        } else None
    } yield res

  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] =
    (for {
      quadr <- get_quadrant(location).flatMap(ZIO.fromOption(_))
      _ <- terrainRef.get.flatMap {
        case m: Map[Vector[Int], Ref[Terrain]] =>
          for {
            sz <- size.get
            center <- centerRef.get
            bkp_ <- base(id, location, 0, radius / 2, quadr).debug
            bkp <- Ref.make[Terrain](bkp_)
            // if quadrant is a terrain block -> defer add_terrain to the sub-block
            // otherwise if no block exists, we add a default block.
            // The default will either represent another sub block (if radius > 1)
            // or a single point containing an entity id (if radius <= 1)
            // Note: We could generalize this for an arbitrary epsilon if desired.
            block <- ZIO
              .fromOption(m.get(quadr))
              // .flatMap(_.get.map { case t: TerrainBlock => t })
              .flatMapError(_ =>
                for {
                  _ <- terrainRef.update {
                    case m: Map[Vector[Int], Ref[Terrain]] =>
                      m.updated(quadr, bkp)
                  }
                } yield ()
              )
              .fold(_ => bkp, x => x)
            _ <- block.get.flatMap { case t: TerrainBlock =>
              t.add_terrain(id, location)
            }
            _ <- block.get.flatMap { case t: TerrainBlock =>
              t.size.update(_ + 1)
            }
            _ <- size.update(_ + 1)
          } yield ()

        case t: TerrainId => ZIO.log(s"quad:$quadr")
      }
    } yield ())
      .fold(_ => (), x => x) // .mapError(err => TerrainAddError(s"$err"))

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def get_terrain(): IO[TerrainError, Unit] = ???

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]] =
    (for {
      quad <- get_quadrant(location).flatMap(ZIO.fromOption(_))
      center <- centerRef.get
      // for any overflow, we take the overflow of that 'boundary' location.x + distance.x - (center.x + radius.x)
      overflow = location
        .zip(center)
        .zip(quad)
        .map((p, sign) =>
          sign * (abs(p._1) + distance) - (p._2 + sign * radius / 2)
        )
      _ <- ZIO.log(s"Quad:$quad")
      t <- terrainRef.get
      res <- t match {
        case m: Map[Vector[Int], Ref[Terrain]] =>
          (for {
            _ <- ZIO.log(s"map val :${m.get(quad)}")
            block <- ZIO.fromOption(m.get(quad)).flatMap(_.get)
            res <- block.get_terrain_within_distance(location, distance)
          } yield res).fold(_ => Seq(), x => x)
        case terrainId: TerrainId =>
          for {
            center <- centerRef.get
          } yield
            if is_within_disance(location, center, distance) then Seq(this)
            else Seq()
      }
    } yield res).fold(_ => Seq(), x => x) // .orElseFail(???)
}
object TerrainBlock extends ZIOAppDefault {

  private type TerrainRef = Map[Vector[Int], Ref[Terrain]] | TerrainId

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    cr <- Ref.make(Vector(0.0, 0, 0))
    tr <- Ref.make[TerrainRef](Map())
    sz <- Ref.make(0)
    tb = TerrainBlock(cr, tr, sz, 10)
    _ <- tb.add_terrain("1", Vector(5, 5, 5))
    // _ <- tb.add_terrain("2", Vector(5, -5, 5))
    res <- tb.get_terrain_within_distance(Vector(5.5, 5.5, 5.5), 10)
    _ <- ZIO.log(tb.toString)
    sz <- tb.size.get
    _ <- ZIO.log(sz.toString)
    _ <- ZIO.log(s"Query:$res")
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
