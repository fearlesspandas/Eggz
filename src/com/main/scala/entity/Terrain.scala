package entity

import entity.Terrain.Quadrant
import entity.Terrain.TerrainId
import entity.Terrain.distance
import entity.Terrain.get_quadrant
import entity.Terrain.is_within_disance
import entity.Terrain.is_within_radius
import entity.Terrain.is_within_range
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.*
import src.com.main.scala.entity.EggzOps.*
import zio.ExitCode
import zio.IO
import zio.Ref
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault

import scala.math.Ordered.orderingToOrdered
import scala.math.abs
import scala.util.Random

trait TerrainManager {
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

trait Terrain {

  def get_terrain(): IO[TerrainError, Seq[Terrain]]

  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]]

  def serialize: IO[TerrainError, Set[TerrainModel]]
}

object Terrain {

  type TerrainId = String
  type Quadrant = Vector[Int]

  def is_within_disance(
    location: Vector[Double],
    center: Vector[Double],
    distance: Double
  ): Boolean = {
    val diffs: Seq[Double] = center.zip(location).map((a, b) => abs(b - a))
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
//Regions form tree structure of hash tree
//they either contain only a single TerrainUnit
//or split themselves into further Regions that separate units.
//Because they must be able to split themselves geometrically,
//they must also have a size (radius) > 0 unlike units which can be
//arbitrarily granular down to zero size
case class TerrainRegion(
  center: Vector[Double],
  radius: Double,
  terrain: Ref[Map[Quadrant, Terrain]]
) extends Terrain
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
  override def get_terrain(): IO[TerrainError, Seq[Terrain]] = for {
    res <- terrain.get.flatMap(t =>
      ZIO.collectAll(t.values.map(_.get_terrain()))
    )
  } yield res.flatMap(x => x).toSeq

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]] = if (
    !is_within_range(location, center, math.max(radius, distance))
  ) ZIO.succeed(Seq())
  else
    for {
      // check quadrants to see if their boundaries are within distance
      quads <- terrain.get.map(m =>
        m.values.filter(t =>
          t match {
            case q: TerrainRegion =>
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
    } yield res.flatten.toSeq

  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] =
    if (!Terrain.is_within_range(location, center, radius)) ZIO.unit
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
          case Some(q: TerrainRegion) => q.add_terrain(id, location)
          // create new sub-quad that contains both new and old terrain unit
          case Some(u: TerrainUnit) =>
            // if coords match, they share a 'cell'/TerrainUnit
            if (u.location == location) u.add_terrain_unit(id, 1)
            else
              for {
                tref <- Ref.make(Map[Quadrant, Terrain]())
                newCenter = quad
                  .map(i => i * radius / 2)
                  .zip(center)
                  .map(x => x._1 + x._2)
                newQuadLocOp <- get_quadrant(u.location, newCenter, radius / 2)
                newQuadLoc <- ZIO
                  .fromOption(newQuadLocOp)
                  .mapError(_ => TerrainAddError("whoops"))
                _ <- tref.update(_.updated(newQuadLoc, u))
                newQuad = TerrainRegion(newCenter, radius / 2, tref)
                _ <- newQuad.add_terrain(id, location)
                _ <- terrain.update(_.updated(quad, newQuad))
              } yield ()
          case None =>
            for {
              tu <- TerrainUnit.make(id, location)
              _ <- terrain.update(_.updated(quad, tu))
            } yield ()

        }).fold(
          x => x,
          e =>
            for {
              tu <- TerrainUnit.make(id, location)
              _ <- terrain.update(_.updated(quad, tu))
            } yield ()
        )
      } yield ()

  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def serialize: IO[TerrainError, Set[TerrainModel]] = for {
    r1 <- get_terrain()
    r2 <- ZIO.foreachPar(r1)(_.serialize)
  } yield r2.flatten.toSet
}

object TerrainRegion {
  def make(
    center: Vector[Double],
    radius: Double
  ): IO[Nothing, TerrainManager] = for {
    t <- Ref.make(Map.empty[Quadrant, Terrain])
  } yield TerrainRegion(center, radius, t)
}
//terrain unit represents the most granular cell of terrain geometry
//i.e. if two terrain entities are "close enough" they can be considered
//part of the same cell and will share a TerrainUnit
//There is no explicit size assumed for TerrainUnit's so they are arbitrarily scalable
//and can cover all levels of granularity
//todo currently TerrainUnit has no 'radius' meaning only exact location matches will be
//  considered the same cell
case class TerrainUnit(
  entitiesRef: Ref[Map[TerrainId, Int]],
  location: Vector[Double]
) extends Terrain {

  def add_terrain_unit(id: TerrainId, units: Int): IO[TerrainError, Unit] =
    for {
      entityCount <- entitiesRef.get.map(_.getOrElse(id, 0))
      _ <- entitiesRef.update(_.updated(id, entityCount + units))
    } yield ()

  override def get_terrain(): IO[TerrainError, Seq[Terrain]] =
    ZIO.succeed(Seq(this))

  override def get_terrain_within_distance(
    loc: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]] =
    if (is_within_disance(location, loc, distance)) {
      ZIO.succeed(Seq(this))
    } else ZIO.succeed(Seq())

  override def serialize: IO[TerrainError, Set[TerrainModel]] =
    for {
      entities <- entitiesRef.get
    } yield Set(TerrainUnitM(location, entities))
}
object TerrainUnit {
  def make(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Terrain] = for {
    idref <- Ref.make(Map((id, 1)))
    glob <- GlobzInMem.make(id).mapError(_ => ???)
  } yield TerrainUnit(idref, location)
}

object TerrainTests extends ZIOAppDefault {

  def randomVec(min: Double, max: Double): Vector[Double] = {
    val negX = Random.nextBoolean()
    val negy = Random.nextBoolean()
    val negz = Random.nextBoolean()
    val x = Random.nextDouble() * (if negX then min else max)
    val y = Random.nextDouble() * (if negy then min else max)
    val z = Random.nextDouble() * (if negz then min else max)
    Vector(x, y, z)
  }
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      tref <- Ref.make(Map.empty[Quadrant, Terrain])
      quad = TerrainRegion(Vector(0, 0, 0), 20, tref)
      minRand = -100
      maxRand = 100
      testterrain = (0 to 100000).map(x => (s"testId$x", randomVec(-10, 10)))
      _ <- quad.add_terrain("testid", Vector(-1, 0, 2))
      _ <- quad.add_terrain("testid2", Vector(1, 1, 2))
      _ <- quad.add_terrain("testid3", Vector(1, 1, 2))
      _ <- quad.add_terrain("testid4", Vector(2, 1, 2))
      _ <- quad.add_terrain("testid5", Vector(4, 1, 2))
      _ <- quad.add_terrain("testid6", Vector(1, 3, 3))
      _ <- quad.add_terrain("testid7", Vector(1, 5, 2))
      _ <- quad.add_terrain("testid8", Vector(1, 5, 10))
      _ <- ZIO.collectAll(testterrain.map { case (id, v) =>
        quad.add_terrain(id, v)
      })
      res <- quad.get_terrain_within_distance(Vector(0, 0, 0), 3)
      res2 <- quad.get_terrain_within_distance(Vector(0, 0, 0), 4)
      res3 <- quad.get_terrain_within_distance(Vector(0, 0, 0), 5)
      allterain <- quad.get_terrain()

      _ <- ZIO.log(s"Terrain within Distance 3: ${res.toString}")
//      _ <- ZIO.log(s"Terrain within Distance 4: ${res2.toString}")
//      _ <- ZIO.log(s"Terrain within Distance 5: ${res3.toString}")
      // _ <- ZIO.log(s"All Terrain: $allterain")
    } yield {

      val failed = res.filter {
        case tu: TerrainUnit =>
          tu.location.foldLeft(0.0)((a, c) => a + abs(c)) > 3
        case _ => false
      }
      assert(
        res.forall {
          case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c)) <= 3
          case _ => true
        },
        s"some locations are outside the range of 3: ${failed.map { case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c))
          }}"
      )
    }
}
