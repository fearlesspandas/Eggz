package entity

import entity.Terrain.Quadrant
import entity.Terrain.TerrainId
import entity.Terrain.TerrainManagement
import entity.Terrain.distance
import entity.Terrain.get_all_quadrants
import entity.Terrain.get_quadrant
import entity.Terrain.is_within_disance
import entity.Terrain.is_within_radius
import entity.Terrain.is_within_range
import entity.TerrainRegion.GetTerrainByQuadrantError
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.*
import src.com.main.scala.entity.EggzOps.*
import zio.Chunk
import zio.ExitCode
import zio.IO
import zio.Ref
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault

import java.util.UUID
import scala.math.abs
import scala.util.Random
import implicits._
object implicits {
  implicit class VecOps(vec: Vector[Double] | Vector[Int]) {
    def +(vector: Vector[Double]): Vector[Double] =
      vec.zip(vector).map {
        case (a: Int, b)    => a + b
        case (a: Double, b) => a + b
      }
    def -(vector: Vector[Double]): Vector[Double] =
      vec.zip(vector).map {
        case (a: Int, b)    => a - b
        case (a: Double, b) => a - b
      }
    def *(scalar: Double): Vector[Double] = vec.map {
      case a: Double => a * scalar
      case a: Int    => a * scalar
    }
  }
  implicit def vecDtoVecI(vector: Vector[Int]): Vector[Double] = vector
}
trait TerrainManager {

  def cacheTerrain(terr: Chunk[Terrain]): IO[TerrainError, Unit]

  def get_cached(uuid: UUID): IO[TerrainError, Option[Terrain]]

  def get_count(): IO[TerrainError, Int]

  def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit]

  def addQuadrant(region: TerrainRegion): IO[TerrainError, Unit]

  def expandTerrain(): IO[TerrainError, Terrain with TerrainManager]

  def remove_terrain(id: TerrainId): IO[TerrainError, Unit]

  def get_terrain(): IO[TerrainError, Seq[Terrain]]

  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]]

  def get_terrain_by_quadrant(
    quadrant: Quadrant
  ): IO[TerrainError, Seq[Terrain]]

  def get_top_terrain(size: Double): IO[TerrainError, Chunk[Terrain]]

  def get_top_terrain_within_distance(
    location: Vector[Double],
    distance: Double,
    size: Double
  ): IO[TerrainError, Chunk[Terrain]]

  def serializeMini(
    relative: Vector[Double] = Vector(0, 0, 0),
    non_relative: Boolean,
    radius: Double
  ): IO[TerrainError, TerrainRegionM]
}

trait Terrain {

  val uuid: UUID = UUID.randomUUID()

  def get_terrain(): IO[TerrainError, Seq[Terrain]]

  def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]]

  def serialize_relative(
    location: Vector[Double] = Vector(0, 0, 0),
    radius: Double
  ): IO[TerrainError, Set[TerrainModel]]

  def serialize(): IO[TerrainError, Set[TerrainModel]]

}

object Terrain {

  type TerrainId = String
  type Quadrant = Vector[Int]
  type TerrainManagement = Terrain with TerrainManager
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

  def get_all_quadrants(
    dimensions: Int
  ): IO[TerrainError, Set[Quadrant]] =
    ZIO
      .succeed(
        Set[Quadrant](
          Vector(1, 1, 1),
          Vector(1, 1, -1),
          Vector(1, -1, 1),
          Vector(-1, 1, 1),
          Vector(1, -1, -1),
          Vector(-1, -1, 1),
          Vector(-1, 1, -1),
          Vector(-1, -1, -1)
        )
      )
      .when(dimensions == 3)
      .flatMap(x => ZIO.fromOption(x).orElseFail(???))

  def get_all_quadrants2(
    dimensions: Int
  ): IO[TerrainError, Set[Quadrant]] =
    for {
      starting_vec <- ZIO.succeed(Vector.fill(dimensions)(0))
      basis <- ZIO.succeed(0 until dimensions)
      // for each 0 in basis we generate a set from acc that maps to two vectors containing 1,-1 at the current index, for each vec in acc (then we recursively add to acc)
      res = basis.foldLeft(Set.empty[Vector[Double]]) { (acc, curr) =>
        val filled = Vector.fill(dimensions)(0)
        val newVecs =
          Set(filled.updated(dimensions, 1), filled.updated(dimensions, -1))
        acc ++ acc.flatMap(v => Set(???)) // ++ newVecs
      }
    } yield Set.empty[Quadrant]
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
  terrain: Ref[Map[Quadrant, Terrain]],
  count: Ref[Int],
  cached: Ref[Map[UUID, Terrain]]
) extends Terrain
    with TerrainManager {

  override def get_count(): IO[TerrainError, Int] = count.get
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
      .reduce(math.max(_, _))
//      .foldLeft(0.0)((acc, curr) => acc + curr)
  }
  override def get_terrain(): IO[TerrainError, Seq[Terrain]] = for {
    res <- terrain.get.flatMap(t => ZIO.foreach(t.values)(_.get_terrain()))
  } yield res.flatten.toSeq
  // returns a collection of the first regions within each quadrant that are under
  // a certain size. This is used for deferred terrain retrieval (cached chunking)
  def get_top_terrain(size: Double): IO[TerrainError, Chunk[Terrain]] =
    if (radius <= size) { ZIO.succeed(Chunk.succeed(this)) }
    else
      for {
        quads <- terrain.get.map(_.values)
        thing <- terrain.get
          .map(_.values)
          .flatMap(quads =>
            ZIO.foreachPar(quads) {
              case tr: TerrainRegion if tr.radius <= size =>
                ZIO.succeed(Chunk.succeed(tr))
              case tr: TerrainRegion => tr.get_top_terrain(size)
              case tu: TerrainUnit   => ZIO.succeed(Chunk.succeed(tu))
              // todo not sure whether to include the size check here yet
              case em: EmptyTerrain => ZIO.succeed(Chunk.succeed(em))
            }
          )
      } yield thing.foldLeft(Chunk.empty[Terrain])((acc, curr) => acc ++ curr)

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]] = if (
    !is_within_range(location, center, math.max(radius, distance)) && this
      .get_boundaries_distance(location) > distance
  ) ZIO.succeed(Seq.empty[Terrain])
  else
    for {
      // check quadrants to see if their boundaries are within distance
      quads <- terrain.get.map(m =>
        m.values.filter(t =>
          t match {
            case q: TerrainRegion =>
              val boundaryDist = q.get_boundaries_distance(location)
              val isWithin = is_within_range(
                location,
                q.center,
                math.max(distance, q.radius)
              ) || boundaryDist < distance
              isWithin
            case u: TerrainUnit => true // defer to terrain unit def
          }
        )
      )
      res <- ZIO.foreach(quads) { t =>
        t.get_terrain_within_distance(
          location,
          distance
        )
      }
    } yield res.flatten.toSeq

  // returns the first region in each quadrant that's below a certain size
  // and also within 'distance' of the specified location. This is used for
  // deferred terrain retrieval (cached chunking)
  def get_top_terrain_within_distance(
    location: Vector[Double],
    distance: Double,
    size: Double
  ): IO[TerrainError, Chunk[Terrain]] = if (
    !is_within_range(location, center, math.max(radius, distance)) && this
      .get_boundaries_distance(location) > distance
  ) { ZIO.succeed(Chunk.empty[Terrain]) }
  else if (radius < size) ZIO.succeed(Chunk.succeed(this))
  else {
    for {
      // check quadrants to see if their boundaries are within distance
      quads_within_range <- terrain.get.map(m =>
        m.values.filter(t =>
          t match {
            case q: TerrainRegion =>
              val boundaryDist = q.get_boundaries_distance(location)
              val isWithin = is_within_range(
                location,
                q.center,
                math.max(distance, q.radius)
              ) || boundaryDist < distance
              isWithin // || is_within_range(location, q.center, distance)
            case u: TerrainUnit =>
              is_within_range(
                location,
                u.location,
                distance
              ) // defer to terrain unit def
          }
        )
      )
      res <- ZIO.foreach(quads_within_range) {
        case tr: TerrainRegion if tr.radius <= size =>
          ZIO.succeed(Chunk.succeed(tr))
        case tr: TerrainRegion =>
          tr.get_top_terrain_within_distance(
            location,
            distance,
            size
          )
        case tu: TerrainUnit => ZIO.succeed(Chunk.succeed(tu))
      }
    } yield Chunk.from(res.flatten)
  }

  override def expandTerrain(): IO[TerrainError, Terrain with TerrainManager] =
    for {
      r <- TerrainRegion.make(this.center, this.radius * 2).map {
        case tr: TerrainRegion => tr
      }
      _ <- this.get_count().flatMap(c => this.count.update(_ => c))
      quads <- get_all_quadrants(3)
      emptyQuads <- ZIO.foreach(quads)(quadrant =>
        r.terrain.update(
          _.updated(
            quadrant,
            EmptyTerrain(
              (quadrant * this.radius) + this.center,
              this.radius
            )
          )
        )
      )
      _ <- r.addQuadrant(this)
    } yield r

  override def addQuadrant(region: TerrainRegion): IO[TerrainError, Unit] =
    for {
      quadrant <- get_quadrant(region.center, this.center, this.radius)
        .flatMap(ZIO.fromOption(_))
        .mapError(err => TerrainAddError("Error adding quadrant to terrain"))
      _ <- terrain.update(_.updated(quadrant, region))
      _ <- region.count.get.flatMap(in_count => count.update(_ + in_count))
    } yield ()

  override def add_terrain(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Unit] =
    (for {
      // get quadrant of location
      quad <- get_quadrant(location, center, radius)
        .flatMap(
          ZIO.fromOption(_).mapError(_ => ???)
        )
      subQuad <- terrain.get
        .map(t => t.get(quad))
      _ <- subQuad match {
        // recurse to sub quad
        case Some(q: TerrainRegion) =>
          for {
            _ <- q.add_terrain(id, location)
            _ <- count.update(_ + 1)
          } yield ()
        // create new sub-quad that contains both new and old terrain unit
        case Some(u: TerrainUnit) =>
          // if coords match, they share a 'cell'/TerrainUnit
          if (u.location == location) for {
            _ <- u.add_terrain_unit(id, 1)
            _ <- count.update(_ + 1)
          } yield ()
          else
            for {
              newCenter <- ZIO.succeed(
                quad
                  .map(i => i * radius / 2)
                  .zip(center)
                  .map(x => x._1 + x._2)
              )
              newQuadLoc <- get_quadrant(u.location, newCenter, radius / 2)
                .flatMap(ZIO.fromOption(_))
                .orElseFail(
                  TerrainAddError("Could not create new quadrant location")
                )
              quadrantMap <- Ref
                .make(Map[Quadrant, Terrain]())
                .tap(ref => ref.update(_.updated(newQuadLoc, u)))
              newCount <- Ref.make(0)
              newcached <- Ref.make(Map.empty[UUID, Terrain])
              newQuad <- TerrainRegion.make(
                newCenter,
                radius / 2,
                Map.empty[Quadrant, Terrain].updated(newQuadLoc, u)
              )
              _ <- newQuad.add_terrain(id, location)
              _ <- terrain.update(_.updated(quad, newQuad))
              _ <- count.update(_ + 1)
            } yield ()
        case Some(e: EmptyTerrain) => ZIO.unit // e.fill(Set((id, location)))
        case None =>
          for {
            tu <- TerrainUnit.make(id, location)
            _ <- terrain.update(_.updated(quad, tu))
            _ <- count.update(_ + 1)
          } yield ()
      }
    } yield ()).when(Terrain.is_within_range(location, center, radius)).unit

  def emptify(): IO[TerrainError, Unit] =
    for {
//      _ <- ZIO.log("emptifying")
      tmap <- this.terrain.get
      _ <- get_all_quadrants(3)
//        this.terrain.get
//        .map(_.keys)
        .map(_.map(q => (q, tmap.get(q))))
        .flatMap(ZIO.foreach(_) {
          case (key, Some(terrain: TerrainRegion)) =>
            terrain.get_count().flatMap {
              case c if c == 0 =>
                this.terrain.update(
                  _.updated(key, EmptyTerrain(terrain.center, terrain.radius))
                ) // mark terrainRegion as emptyTerrain
              case _ =>
                terrain.terrain.get
                  .map(_.values)
                  .flatMap(
                    ZIO.foreach(_) {
                      case inner_terrain: TerrainRegion =>
                        inner_terrain.emptify()
                      case _ => ZIO.unit
                    }
                  ) // recurse to check for empty regions
            }
          case (key, Some(TerrainUnit(_, _))) => ZIO.unit
          case (key, None) =>
            terrain.update(
              _.updated(
                key,
                EmptyTerrain(
                  this.center + (key * (this.radius / 2)),
                  this.radius / 2
                )
              )
            )
        })
    } yield ()
  override def remove_terrain(id: TerrainId): IO[TerrainError, Unit] = ???

  override def serialize_relative(
    location: Vector[Double] = Vector(0, 0, 0),
    radius: Double
  ): IO[TerrainError, Set[TerrainModel]] = for {
    r1 <- get_terrain_within_distance(location, radius)
    r2 <- ZIO.foreachPar(r1)(
      _.serialize_relative(location, radius)
    )
  } yield r2.flatten.toSet

  override def serialize(): IO[TerrainError, Set[TerrainModel]] = for {
    r1 <-
      get_terrain()
    r2 <- ZIO.foreachPar(r1)(
      _.serialize()
    )
  } yield r2.flatten.toSet

  override def serializeMini(
    relative: Vector[Double] = Vector(0, 0, 0),
    non_relative: Boolean,
    radius: Double
  ): IO[TerrainError, TerrainRegionM] = for {
    r1 <-
      if (non_relative) get_terrain()
      else get_terrain_within_distance(relative, radius)
    r2 <- ZIO
      .foreachPar(r1)(_.serialize())
      .map(_.flatten.toSet.map { case tm: TerrainUnitM =>
        (tm.location, tm.entities, tm.uuid)
      })
      .map(TerrainRegionM(_))
  } yield r2

  override def get_terrain_by_quadrant(
    quadrant: Quadrant
  ): IO[TerrainError, Seq[Terrain]] = for {
    quadop <- terrain.get
      .map(_.get(quadrant))
    res <- quadop match {
      case Some(quad) => quad.get_terrain()
      case None       => ZIO.succeed(Seq())
    }
  } yield res
  // todo add list of uuids to terrain regions to verify that caching is valid
  override def cacheTerrain(
    terr: Chunk[Terrain]
  ): IO[TerrainError, Unit] =
    ZIO
      .collectAllPar(
        terr.map(tm => cached.update(_.updated(tm.uuid, tm)))
      )
      .map(_ => ())

  override def get_cached(
    uuid: UUID
  ): IO[TerrainError, Option[Terrain]] = cached.get.map(_.get(uuid))
}

object TerrainRegion {
  def make(
    center: Vector[Double],
    radius: Double
  ): IO[Nothing, TerrainManager with Terrain] = for {
    quadrants <- Ref.make(Map.empty[Quadrant, Terrain])
    count <- Ref.make(0)
    cached <- Ref.make(Map.empty[UUID, Terrain])
  } yield TerrainRegion(center, radius, quadrants, count, cached)

  def make(
    center: Vector[Double],
    radius: Double,
    quadrantMap: Map[Quadrant, Terrain]
  ): IO[Nothing, TerrainManager with Terrain] = for {
    quadrantsRef <- Ref.make(quadrantMap)
    count <- Ref.make(0)
    cached <- Ref.make(Map.empty[UUID, Terrain])
  } yield TerrainRegion(center, radius, quadrantsRef, count, cached)

  case class GetTerrainByQuadrantError(msg: String) extends TerrainError
}

case class EmptyTerrain(center: Vector[Double], radius: Double)
    extends Terrain {

  def fill(
    terrain_to_add: Set[(TerrainId, Vector[Double])]
  ): IO[TerrainError, TerrainManager with Terrain] =
    for {
      tr <- TerrainRegion.make(center, radius).map { case tr: TerrainRegion =>
        tr
      }
      _ <- ZIO.foreachDiscard(terrain_to_add)(t => tr.add_terrain(t._1, t._2))
      count <- tr.get_count()
      _ <- tr.emptify()
    } yield tr

  override def get_terrain(): IO[TerrainError, Seq[Terrain]] =
    ZIO.succeed(Seq.empty[Terrain])

  override def get_terrain_within_distance(
    location: Vector[Double],
    distance: Double
  ): IO[TerrainError, Seq[Terrain]] = ZIO.succeed(Seq.empty[Terrain])

  override def serialize_relative(
    location: Vector[Double],
    radius: Double
  ): IO[TerrainError, Set[TerrainModel]] = ZIO.succeed(Set.empty[TerrainModel])

  override def serialize(): IO[TerrainError, Set[TerrainModel]] =
    ZIO.succeed(Set.empty[TerrainModel])
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
//  val uuid: UUID = UUID.randomUUID()
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

  override def serialize_relative(
    location: Vector[Double] = Vector(0, 0, 0),
    radius: Double
  ): IO[TerrainError, Set[TerrainModel]] =
    for {
      entities <- entitiesRef.get
    } yield Set(TerrainUnitM(location, entities, uuid))

  override def serialize(): IO[TerrainError, Set[TerrainModel]] =
    for {
      entities <- entitiesRef.get
    } yield Set(TerrainUnitM(location, entities, uuid))
}
object TerrainUnit {
  def make(
    id: TerrainId,
    location: Vector[Double]
  ): IO[TerrainError, Terrain] = for {
    idref <- Ref.make(Map((id, 1)))
  } yield TerrainUnit(idref, location)
}

object TerrainTests extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      maxRand <- ZIO.succeed(1000)
      minRand <- ZIO.succeed(-1000)
      terrain <- TerrainRegion
        .make(
          Vector(0, 0, 0),
          math.max(abs(maxRand), abs(minRand))
        )
        .map { case t: TerrainRegion => t }
      testterrain = (0 to 100000).map(x =>
        (s"testId$x", randomVec(-minRand, maxRand))
      )
      _ <- terrain.add_terrain("testid", Vector(-1, 0, 2))
      _ <- terrain.add_terrain("testid2", Vector(1, 1, 2))
      _ <- terrain.add_terrain("testid3", Vector(1, 1, 2))
      _ <- terrain.add_terrain("testid4", Vector(2, 1, 2))
      _ <- terrain.add_terrain("testid5", Vector(4, 1, 2))
      _ <- terrain.add_terrain("testid6", Vector(1, 3, 3))
      _ <- terrain.add_terrain("testid7", Vector(1, 5, 2))
      _ <- terrain.add_terrain("testid8", Vector(1, 5, 10))
      _ <- ZIO.foreachDiscard(testterrain) { case (id, v) =>
        terrain.add_terrain(id, v)
      }
      _ <- ZIO.log("Starting basic distance query tests")
      _ <- basicDistanceQueryTests(terrain)
      _ <- ZIO.log("Completed: Basic Distance Query tests")
      _ <- ZIO.log("Starting random point query tests")
      _ <- randomPointQueryTest(terrain)
      _ <- ZIO.log("Completed: Random point query tests")
      _ <- ZIO.log("Starting terrain expansion tests")
      _ <- terrainExpansionTest(terrain)
      _ <- ZIO.log("Completed: Terrain Expansion Tests")
    } yield {}
  def randomVec(min: Double, max: Double): Vector[Double] = {
    val negX = Random.nextBoolean()
    val negy = Random.nextBoolean()
    val negz = Random.nextBoolean()
    val x = Random.nextDouble() * (if negX then min else max)
    val y = Random.nextDouble() * (if negy then min else max)
    val z = Random.nextDouble() * (if negz then min else max)
    Vector(x, y, z)
  }
  def terrainExpansionTest(startingTerrain: TerrainRegion) =
    for {
      topTerr_100 <- startingTerrain.get_top_terrain(100)
      bigger_terrain <- startingTerrain.expandTerrain().map {
        case t: TerrainRegion =>
          t
      }
      _ <- ZIO.succeed(
        assert(
          bigger_terrain.center == startingTerrain.center,
          s"center points for expanded terrain and starting terrain region do not match [Expanded:${bigger_terrain.center}, Original: ${startingTerrain.center}]"
        )
      )
      _ <- ZIO.log("Passed: center point for expanded terrain matches original")
      _ <- ZIO.succeed(
        assert(
          bigger_terrain.radius == 2 * startingTerrain.radius,
          s"Radius for expanded terrain is not twice the original terrain [Expanded: ${bigger_terrain.radius}, Original:${startingTerrain.radius}]"
        )
      )
      _ <- ZIO.log(
        "Passed: Radius for expanded terrain is twice the size of the original"
      )
      expanded_top_terrain <- bigger_terrain.get_top_terrain(100)
      top_terrain_diff_non_empty <- ZIO.succeed(
        expanded_top_terrain.diff(topTerr_100).filter {
          case e: EmptyTerrain => false; case _ => true
        }
      )
      _ <- ZIO.succeed(
        assert(
          top_terrain_diff_non_empty.isEmpty,
          "Differing results for top terrain between Region and Expanded Region that's not EmptyTerrain"
        )
      )
      _ <- ZIO.log(
        "Passed: Only difference between expanded terrain and original regions for query get_top_terrain(100) are EmptyTerrain"
      )
      bigger_count <- bigger_terrain.get_count()
      original_count <- startingTerrain.get_count()
      _ <- ZIO.succeed(
        assert(
          bigger_count == original_count,
          s"counts are not matching for expanded terrain and original region before fill [Expanded:${bigger_count}, Original:${original_count}]"
        )
      )
      _ <- ZIO.log(
        "Passed: Counts for expanded terrain and original region are matching before fill"
      )
      bigger_quads <- bigger_terrain.terrain.get
      bigger_empty = bigger_quads.flatMap {
        case (q, e: EmptyTerrain) => Set((q, e)); case _ => Set()
      }
      empty_to_be_filled <- ZIO.succeed(bigger_empty.head._2)
      num_fill <- ZIO.succeed(100000)
      fillset <- ZIO.foreach(0 until num_fill)(i =>
        ZIO.succeed(
          (
            s"$i",
            randomVec(
              -empty_to_be_filled.radius,
              empty_to_be_filled.radius
            ) + empty_to_be_filled.center
          )
        )
      )
      first_5_fill <- ZIO.succeed(fillset.take(5))
      filled_5 <- empty_to_be_filled.fill(first_5_fill.toSet).map {
        case tr: TerrainRegion => tr
      }
      _ <- filled_5
        .get_count()
        .map(c =>
          assert(c == 5, "filled_5 count is not equal to 5 when 5 added")
        )
      _ <- ZIO.log("Passed: fill empty terrain with 5 random vectors")
      at_least_one_empty <- filled_5.terrain.get.map(_.filter {
        case (q, tr: TerrainRegion) => false
        case (q, tr: EmptyTerrain)  => true
        case (q, tr: TerrainUnit)   => false
      })
      _ <- ZIO.succeed(
        assert(
          at_least_one_empty.nonEmpty,
          "No empty regions found after fill 5 (should contain at least one)"
        )
      )
      _ <- ZIO.log("Passed: At least one empty quadrant found in fill 5")
      filled <- empty_to_be_filled.fill(fillset.toSet).map {
        case tr: TerrainRegion => tr
      }
      _ <- filled
        .get_count()
        .map(c =>
          assert(
            c == num_fill,
            s"count ${c} does not match the number of items added $num_fill"
          )
        )
      _ <- ZIO.log(
        "Passed: all terrain added to empty terrain was added successfully"
      )
      can_add_with_empties <- filled.add_terrain(
        "postfill",
        randomVec(
          -filled.radius,
          filled.radius
        ) + filled.center
      )
      _ <- ZIO.log(
        "Passed: successfully added to terrain region that contains EmptyTerrain"
      )
      filled_quads <- filled.terrain.get.map(_.map {
        case (q, tr: TerrainRegion) => ("REGION", q, tr.center)
        case (q, tr: EmptyTerrain)  => ("EMPTY", q, tr.center)
        case (q, tr: TerrainUnit)   => ("UNIT", q, tr.location)
      })
      _ <- ZIO.succeed(assert(filled_quads.size == 8))
      _ <- ZIO.log(
        "Passed: Filled terrain has the correct number of explicit quadrants (8)"
      )
      filled_quads_non_empty <- filled.terrain.get.map(_.filter {
        case (q, tr: TerrainRegion) => true
        case (q, tr: EmptyTerrain)  => false
        case (q, tr: TerrainUnit)   => true
      })
      _ <- ZIO.succeed(
        assert(
          filled_quads_non_empty.nonEmpty,
          s"No regions were filled after fill test of $num_fill points"
        )
      )
      _ <- ZIO.log(
        s"Passed: Found regions that were filled and converted to normal terrain after fill of $num_fill points"
      )
    } yield ()
  def randomPointQueryTest(terrain: TerrainRegion) =
    for {
      query_point <- ZIO.succeed(Vector(78.125, 78.125, 390.625))
      query_distance <- ZIO.succeed(100)
      top_terr_within_100 <- terrain.get_top_terrain_within_distance(
        query_point,
        query_distance,
        10
      )
      outside_query_distance <- ZIO.filter(top_terr_within_100) {
        case tr: TerrainRegion =>
          ZIO.succeed(
            (tr.center - query_point)
              .map(math.abs)
              .forall(curr => curr > query_distance + tr.radius)
          )
        case tu: TerrainUnit =>
          ZIO.succeed(
            (tu.location - query_point)
              .map(math.abs)
              .forall(curr => curr > query_distance)
          )
      }
      fmt_outside_query_distance = outside_query_distance
        .filter(x =>
          x match {
            case t: TerrainRegion => true;
            case _                => false
          }
        )
        .map { case tr: TerrainRegion => (tr.center, tr.radius) }
      _ <- ZIO.succeed(
        assert(
          outside_query_distance.isEmpty,
          s"Terrain found outside query distance for test get_top_level_terrain_within_distance($query_distance) : $fmt_outside_query_distance"
        )
      )
      _ <- ZIO.log(
        s"Passed: get_top_terrain_within_distance($query_distance) does not contain points outside of the expected distance"
      )
    } yield ()
  def basicDistanceQueryTests(terrain: TerrainRegion) =
    for {
      res <- terrain.get_terrain_within_distance(Vector(0, 0, 0), 3)
      res2 <- terrain.get_terrain_within_distance(Vector(0, 0, 0), 4)
      res3 <- terrain.get_terrain_within_distance(Vector(0, 0, 0), 5)
      res4 <- terrain.get_terrain_within_distance(
        Vector(78.125, 78.125, 390.625),
        100
      )
      allterain <- terrain.get_terrain()
      topTerr_0 <- terrain.get_top_terrain(0)
      topTerr_5 <- terrain.get_top_terrain(5)
      topTerr_8 <- terrain.get_top_terrain(8)
      topTerr_10 <- terrain.get_top_terrain(10)
      topTerr_100 <- terrain.get_top_terrain(100)
      _ <- ZIO.succeed(
        assert(
          topTerr_0.size == allterain.size,
          "sizes of allterrain and get_top_terrain_within_distance(0) should be the same"
        )
      )
      _ <- ZIO.log(
        s"Passed: get_top_terrain_within_distance(0) size is the same as the size of allterrain"
      )
    } yield {
      val failed_dist_3 = res.filter {
        case tu: TerrainUnit =>
          tu.location.foldLeft(0.0)((a, c) => a + abs(c)) > 3
        case _ => false
      }
      val failed_dist_4 = res.filter {
        case tu: TerrainUnit =>
          tu.location.foldLeft(0.0)((a, c) => a + abs(c)) > 4
        case _ => false
      }
      val failed_dist_5 = res.filter {
        case tu: TerrainUnit =>
          tu.location.foldLeft(0.0)((a, c) => a + abs(c)) > 5
        case _ => false
      }
      assert(
        res.forall {
          case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c)) <= 3
          case _ => true
        },
        s"some locations are outside the range of 3: ${failed_dist_3.map { case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c))
          }}"
      )
      assert(
        res2.forall {
          case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c)) <= 4
          case _ => true
        },
        s"some locations are outside the range of 4: ${failed_dist_4.map { case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c))
          }}"
      )
      assert(
        res3.forall {
          case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c)) <= 5
          case _ => true
        },
        s"some locations are outside the range of 4: ${failed_dist_5.map { case tu: TerrainUnit =>
            tu.location.foldLeft(0.0)((a, c) => a + abs(c))
          }}"
      )
      assert(
        topTerr_0.size == allterain.size,
        s"Size of get_top_terrain(0) do not match the size of all_terrain"
      )
      assert(
        topTerr_0.size > topTerr_5.size && topTerr_5.size >= topTerr_8.size && topTerr_8.size >= topTerr_10.size,
        s"Top Terrain return should be ordered inversely to input"
      )
    }
}
