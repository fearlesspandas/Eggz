package entity

import entity.Terrain2.Quadrant
import entity.Terrain2.TerrainId
import entity.Terrain2.get_quadrant
import entity.Terrain2.is_within_disance
import zio.IO
import zio.Ref
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault

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
    val diffs = center.zip(location).map((a, b) => abs(a - b))
    diffs.forall(diff => diff <= distance)
  }

  def get_quadrant(
    location: Vector[Double],
    center: Vector[Double],
    radius: Double
  ): IO[TerrainError, Option[Quadrant]] =
    for {
      in_block <- ZIO.succeed(is_within_disance(location, center, radius))
      res =
        if in_block then {
          Some(location.zip(center).map((l, c) => if l >= c then 1 else -1))
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
    !is_within_disance(location, center, distance) && !is_within_disance(
      location,
      center,
      radius
    )
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
            case u: TerrainUnit => true // defer to terrain unit def
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
    } yield res.flatMap(x => x).toSeq

  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] =
    if (!Terrain2.is_within_disance(location, center, radius)) ZIO.unit
    else
      for {
        // get quadrant of location
        quad <- get_quadrant(location, center, radius).flatMap(
          ZIO.fromOption(_).mapError(_ => ???)
        )
        subQuad <- terrain.get
          .map(t => t.get(quad))
        _ <- (subQuad match {
          // recurse to sub quad
          case Some(q: QUADRANT) => q.add_terrain(id, location)
          // create new sub-quad that contains both new and old terrain unit
          case Some(u: TerrainUnit) =>
            // todo double check the location logic on this
            for {
              tref <- Ref.make(Map[Quadrant, Terrain2]())
              newCenter = quad
                .map(i => i * radius / 2)
                .zip(center)
                .map(x => x._1 + x._2)
              newQuadLocOp <- get_quadrant(u.offset, newCenter, radius / 2)
              newQuadLoc <- ZIO.fromOption(newQuadLocOp).mapError(_ => ???)
              _ <- tref.update(_.updated(newQuadLoc, u))
              newQuad = QUADRANT(newCenter, radius / 2, tref)
              _ <- newQuad.add_terrain(id, location)
              _ <- terrain.update(_.updated(quad, newQuad))
            } yield () // create new quadrant with new terrain and existing terrain
          // make sure new center is relative/local not global
          // update terrainRef
          case None =>
            terrain.update(_.updated(quad, TerrainUnit(id, location)))
        }).fold(
          x => x,
          e =>
            for {
              _ <- ZIO.log("Creating Terrain")
              tu = TerrainUnit(id, location)
              _ <- terrain.update(_.updated(quad, tu))
            } yield ()
        )
      } yield ()

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???
}

//terrain unit represents a single terrain entity with its location
case class TerrainUnit(id: TerrainId, offset: Vector[Double]) extends Terrain2 {

  override def get_terrain(): IO[TerrainError, Seq[Terrain2]] =
    ZIO.succeed(Seq(this))

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain2]] = if (
    is_within_disance(location, offset, distance)
  ) ZIO.succeed(Seq(this))
  else ZIO.succeed(Seq())

}

object TerrainTests extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      tref <- Ref.make(Map.empty[Quadrant, Terrain2])
      quad = QUADRANT(Vector(0, 0, 0), 20, tref)
      _ <- quad.add_terrain("testid", Vector(1, 0, 2))
      _ <- quad.add_terrain("testid2", Vector(1, 1, 1))
      res <- quad.get_terrain_within_distance(Vector(0, 0, 0), 2.5)
//      res <- quad.get_terrain()
      _ <- ZIO.log(res.toString)
    } yield ()
}
