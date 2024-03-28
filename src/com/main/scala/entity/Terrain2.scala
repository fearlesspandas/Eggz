package entity

import entity.Terrain2.Quadrant
import entity.Terrain2.TerrainId
import entity.Terrain2.distance
import entity.Terrain2.get_quadrant
import entity.Terrain2.is_within_disance
import entity.Terrain2.is_within_radius
import entity.Terrain2.is_within_range
import zio.IO
import zio.Ref
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault

import scala.math.Ordered.orderingToOrdered
import scala.math.abs

trait TerrainManager {
  def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit]

  def remove_terrain(id: TerrainId): IO[TerrainError, Unit]

  def get_terrain(): IO[TerrainError, Seq[Terrain2]]

  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain2]]
}

trait Terrain2 {

  def get_terrain(): IO[TerrainError, Seq[Terrain2]]

  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain2]]
}

object Terrain2 {

  type TerrainId = String
  type Quadrant = Vector[Int]

  def is_within_disance(
    location: Vector[Double],
    center: Vector[Double],
    distance: Double
  ): Boolean = {
    val diffs: Seq[Double] = center.zip(location).map((a, b) => abs(a - b))
    val dist = diffs.foldLeft(0.0)((a, c) => a + c)
    dist <= distance
  }

  def is_within_range(
    location: Vector[Double],
    center: Vector[Double],
    distance: Double
  ): Boolean = {
    val diffs: Seq[Double] = center.zip(location).map((a, b) => abs(a - b))
    diffs.forall(diff => diff <= distance)
  }
  def distance(a: Vector[Double], b: Vector[Double]): Double = ???
  def is_within_radius(
    location: Vector[Double],
    center: Vector[Double],
    radius: Double
  ): Boolean =
    ???

  def get_quadrant(
    location: Vector[Double],
    center: Vector[Double],
    radius: Double
  ): IO[TerrainError, Option[Quadrant]] =
    for {
      in_block <- ZIO.succeed(is_within_disance(location, center, radius))
      res =
        if (true) {
          Some(
            location
              .zip(center) map ((l, c) => if (l >= c) 1 else -1)
          )
        } else None
    } yield res
}
//might make sense to distinguish between Quadrant (top level) and subquadrant (localized)
case class QUADRANT(
  center: Vector[
    Double
  ], // center is relative coords for subquads and global for top level
  radius: Double,
  terrain: Ref[Map[Quadrant, Terrain2]]
) extends Terrain2
    with TerrainManager {
  // determine minimum boundary distances from location, then find distance from that boundary
  def get_boundaries_distance(location: Vector[Double]): Double = {
    val positive_bounds = center.map(_ + radius)
    val negative_bounds = center.map(_ - radius)
    val bounds = positive_bounds.zip(negative_bounds)
    val all = location.zip(bounds)
    all
      .map { case (l, (p, n)) =>
        if (abs(l - p) < abs(l - n)) abs(l - p) else abs(l - n)
      }
      .foldLeft(0.0)((acc, curr) => acc + curr)
  }
  override def get_terrain(): IO[TerrainError, Seq[Terrain2]] = for {
    res <- terrain.get.flatMap(t =>
      ZIO.collectAll(t.values.map(_.get_terrain()))
    )
  } yield res.flatMap(x => x).toSeq

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain2]] = if (
    !is_within_range(location, center, math.max(radius, distance))
  ) ZIO.succeed(Seq())
  else
    for {
      // check quadrants to see if their boundaries are within distance
      quads <- terrain.get.map(m =>
        m.values.filter(t =>
          t match {
            case q: QUADRANT =>
              val boundaryDist = q.get_boundaries_distance(location)
              val isWithin = is_within_disance(
                location,
                q.center,
                math.min(distance, q.radius)
              ) || boundaryDist < distance
              isWithin
            case u: TerrainUnit2 => true // defer to terrain unit def
          }
        )
      )
      res <- ZIO.collectAll(
        quads.map(t =>
          t.get_terrain_within_distance(
            location,
            distance
          )
        )
      )
    } yield res.flatten.toSeq

  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] =
    if (!Terrain2.is_within_range(location, center, radius)) ZIO.unit
    else
      for {
        // get quadrant of location
        quad <- get_quadrant(location, center, radius)
          .flatMap(
            ZIO.fromOption(_).mapError(_ => ???)
          )
        subQuad <- terrain.get
          .map(t => t.get(quad))
        _ <- (subQuad match {
          // recurse to sub quad
          case Some(q: QUADRANT) => q.add_terrain(id, location)
          // create new sub-quad that contains both new and old terrain unit
          case Some(u: TerrainUnit2) =>
            // todo double check the location logic on this
            // case for when coords exactly match
            for {
              tref <- Ref.make(Map[Quadrant, Terrain2]())
              newCenter = quad
                .map(i => i * radius / 2)
                .zip(center)
                .map(x => x._1 + x._2)
              newQuadLocOp <- get_quadrant(u.offset, newCenter, radius / 2)
              newQuadLoc <- ZIO
                .fromOption(newQuadLocOp)
                .mapError(_ => TerrainAddError("whoops"))
              _ <- tref.update(_.updated(newQuadLoc, u))
              newQuad = QUADRANT(newCenter, radius / 2, tref)
              _ <- newQuad.add_terrain(id, location)
              _ <- terrain.update(_.updated(quad, newQuad))
            } yield ()
          case None =>
            for {
              tu <- TerrainUnit2.make(id, location)
              _ <- terrain.update(_.updated(quad, tu))
            } yield ()

        }).fold(
          x => x,
          e =>
            for {
              _ <- ZIO.logError("Creating Terrain")
              tu <- TerrainUnit2.make(id, location)
              _ <- terrain.update(_.updated(quad, tu))
            } yield ()
        )
      } yield ()

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???
}

//terrain unit represents the most granular cell of terrain geometry
//i.e. if two terrain entities are "close enough" they can be considered
//part of the same cell and will share a TerrainUnit
//There is no explicit size assumed for TerrainUnit's so they are arbitrarily scalable
//and can cover all levels of granularity
//todo currently TerrainUnit has no 'radius' meaning only exact location matches will be
//  considered the same cell
case class TerrainUnit2(id: TerrainId, offset: Vector[Double])
    extends Terrain2 {

  override def get_terrain(): IO[TerrainError, Seq[Terrain2]] =
    ZIO.succeed(Seq(this))

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain2]] =
    if (is_within_disance(location, offset, distance)) {
      ZIO.succeed(Seq(this))
    } else ZIO.succeed(Seq())

}
object TerrainUnit2 {
  def make(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Terrain2] = for {
    idref <- Ref.make(Map(id, 1))
  } yield TerrainUnit2(id, location)
}
case class Terrainblock(
  center: Vector[Double],
  radius: Ref[Double],
  terrain: Ref[Seq[Terrain2]]
) extends TerrainManager
    with Terrain2 {
  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] = for {
    _ <- terrain.update(_ :+ TerrainUnit(id, location))
    rad <- radius.get
    _ <- ZIO.when(distance(location, center) > rad)(
      radius.update(_ => distance(location, center))
    )
  } yield ()

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def get_terrain(): IO[TerrainError, Seq[Terrain2]] = ???

  override def get_terrain_within_distance(
    location: Vector[Double],
    dist: Double
  ): IO[TerrainError, Seq[Terrain2]] = for {
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
}
case class TerrainUnit(id: TerrainId, center: Vector[Double]) extends Terrain2 {
  override def get_terrain(): IO[TerrainError, Seq[Terrain2]] =
    ZIO.succeed(Seq(this))

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): zio.IO[TerrainError, Seq[Terrain2]] = ZIO
    .when(is_within_radius(location, center, distance))(ZIO.succeed(Seq(this)))
    .map(_.getOrElse(Seq()))
}
object TerrainUnit {
  def make(): IO[TerrainError, Terrain2] = ???
}

object TerrainTests extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      tref <- Ref.make(Map.empty[Quadrant, Terrain2])
      quad = QUADRANT(Vector(0, 0, 0), 20, tref)
      _ <- quad.add_terrain("testid", Vector(1, 0, 2))
      _ <- quad.add_terrain("testid2", Vector(1, 1, 2))
      _ <- quad.add_terrain("testid3", Vector(1, 1, 2.25))
      _ <- quad.add_terrain("testid4", Vector(2, 1, 2))
      _ <- quad.add_terrain("testid5", Vector(4, 1, 2))
      _ <- quad.add_terrain("testid6", Vector(1, 3, 3))
      _ <- quad.add_terrain("testid7", Vector(1, 5, 2))
      _ <- quad.add_terrain("testid8", Vector(1, 5, 10))
      res <- quad.get_terrain_within_distance(Vector(0, 0, 0), 3)
      res2 <- quad.get_terrain_within_distance(Vector(0, 0, 0), 4)
      res3 <- quad.get_terrain_within_distance(Vector(0, 0, 0), 5)
      allterain <- quad.get_terrain()
      _ <- ZIO.log(s"Terrain within Distance 3: ${res.toString}")
      _ <- ZIO.log(s"Terrain within Distance 4: ${res2.toString}")
      _ <- ZIO.log(s"Terrain within Distance 5: ${res3.toString}")
      _ <- ZIO.log(s"All Terrain: $allterain")
    } yield {}
}
