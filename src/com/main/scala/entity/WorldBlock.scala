package entity

import entity.Terrain.TerrainId
import entity.WorldBlock.GenericWorldBlockError
import entity.WorldBlock.WorldBlockError
import network.BasicPhysicsChannel
import network.PhysicsChannel
import physics.PhysicsCommand
import physics.SendLocation
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz
import zio.Ref
import zio.ZLayer
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http.Client
import zio.http.Handler
import zio.http.WebSocketApp
import zio.http.WebSocketFrame
import zio.json.*
//import src.com.main.scala.entity.Globz
import zio.ExitCode
import zio._

object WorldBlock {

  trait Block {
    def spawnBlob(
      blob: Globz,
      coords: Vector[Double]
    ): IO[WorldBlockError, ExitCode]
    // deprecated
    def spawnFreshBlob(
      coords: Vector[Double]
    ): ZIO[Globz, WorldBlockError, ExitCode]
    def getAllBlobs(): ZIO[Any, WorldBlockError, Set[Globz]]
    def removeBlob(blob: Globz): IO[WorldBlockError, ExitCode]
    def tickAllBlobs(): ZIO[Any, WorldBlockError, ExitCode]
    def getBlob(id: GLOBZ_ID): IO[WorldBlockError, Option[Globz]]

    def updateBlob(blob: Globz): IO[WorldBlockError, ExitCode]

//    val terrain: TerrainManager with Terrain
    def getTerrain: IO[WorldBlockError, TerrainManager with Terrain]
//      ZIO.succeed(terrain)

    def expandTerrain: IO[WorldBlockError, Unit]
  }

  trait Service {
    def make: IO[WorldBlockError, WorldBlock.Block]
  }
  def make: ZIO[WorldBlock.Service, WorldBlockError, WorldBlock.Block] =
    ZIO.service[WorldBlock.Service].flatMap(_.make)

  def spawnBlob(
    blob: Globz,
    coords: Vector[Double]
  ): ZIO[WorldBlock.Block, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.spawnBlob(blob, coords))
    // deprecated
  def spawnFreshBlob(
    coords: Vector[Double]
  ): ZIO[WorldBlock.Block with Globz, WorldBlockError, ExitCode] =
    ZIO.service[WorldBlock.Block].flatMap(_.spawnFreshBlob(coords))
  def getAllBlobs(): ZIO[WorldBlock.Block, WorldBlockError, Set[Globz]] =
    ZIO.environmentWithZIO[WorldBlock.Block](_.get.getAllBlobs())

  def removeBlob(
    blob: Globz
  ): ZIO[WorldBlock.Block, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.removeBlob(blob))

  def tickAllBlobs(): ZIO[WorldBlock.Block, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO[WorldBlock.Block](_.get.tickAllBlobs())
  def getBlob(
    id: GLOBZ_ID
  ): ZIO[WorldBlock.Block, WorldBlockError, Option[Globz]] =
    ZIO.serviceWithZIO[Block](_.getBlob(id))

  def getTerrain
    : ZIO[WorldBlock.Block, WorldBlockError, TerrainManager with Terrain] =
    ZIO.environmentWithZIO(_.get.getTerrain)

  trait WorldBlockError

  case class GenericWorldBlockError(msg: String) extends WorldBlockError

}

case class WorldBlockInMem(
  dbRef: Ref[Map[GLOBZ_ID, Globz]],
  terrain: Ref[TerrainManager with Terrain],
  npc_handler: NPCHandler
) extends WorldBlock.Block {

  override def spawnBlob(
    blob: Globz,
    coords: Vector[Double]
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    for {
      _ <- dbRef.update(_.updated(blob.id, blob))
    } yield ExitCode.success

  override def getAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, Set[Globz]] =
    for {
      db <- dbRef.get
    } yield db.values.toSet

  override def removeBlob(
    blob: Globz
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    for {
      _ <- dbRef.update(_.removed(blob.id))
    } yield ExitCode.success

  override def tickAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, ExitCode] =
    (for {
      all <- getAllBlobs()
      r <- ZIO.foreachPar(all)(g => g.tickAll())
      fail = r.count(_ != ExitCode.success)
    } yield ExitCode.apply(fail))
      .orElseFail(GenericWorldBlockError("error tick blobs"))
  // deprecated
  override def spawnFreshBlob(
    coords: Vector[Double]
  ): ZIO[Globz, WorldBlock.WorldBlockError, ExitCode] =
    ZIO.service[Globz].flatMap(spawnBlob(_, coords))

  override def getBlob(
    id: GLOBZ_ID
  ): IO[WorldBlock.WorldBlockError, Option[Globz]] =
    for {
      res <- dbRef.get.map(_.get(id))
    } yield res

  override def updateBlob(
    blob: Globz
  ): IO[WorldBlock.WorldBlockError, ExitCode] = ???

  override def getTerrain
    : IO[WorldBlock.WorldBlockError, TerrainManager with Terrain] = terrain.get

  override def expandTerrain: IO[WorldBlock.WorldBlockError, Unit] = for {
    t <- terrain.get
    d <- t
      .expandTerrain()
      .map { case tr: TerrainRegion => tr }
      .mapError(_ => ???)
    _ <- terrain.update(_ => d)
//    generated_terrain <- WorldBlockEnvironment.add_terrain(d, d.radius, 10000)
  } yield ()
}
object WorldBlockInMem extends WorldBlock.Service {
  override def make: IO[WorldBlock.WorldBlockError, WorldBlock.Block] =
    for {
      radius <- System
        .env("WORLDBLOCK_RADIUS")
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          err =>
            GenericWorldBlockError(
              s"error initiating worldblock; no WORLDBLOCK_RADIUS env variable set; $err"
            ),
          _.toInt
        )
      //      num = 50000
      num <- System
        .env("RANDOMIZED_SPAWN_COUNT")
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          err =>
            GenericWorldBlockError(
              s"Error initiating worldblock; no RANDOMIZED_SPAWN_COUNT env variable set; $err"
            ),
          _.toInt
        )
      num_prowlers <- System
        .env("PROWLER_COUNT")
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          err =>
            GenericWorldBlockError(
              s"Error initiating worldblock; no PROWLER_COUNT env variable set; $err"
            ),
          _.toInt
        )
      terrain <- TerrainRegion.make(Vector(0, 0, 0), radius).map {
        case tr: TerrainRegion => tr
      }
      terrain <- WorldBlockEnvironment.add_terrain(terrain, terrain.radius, num)
      _ <- terrain // add spawn block to terrain
        .add_terrain("9", Vector(0, -20, 0))
        .orElseFail(
          GenericWorldBlockError("Could not add spawn block to terrain block")
        )
      t_count <- terrain
        .get_count()
        .mapError(err =>
          GenericWorldBlockError(
            s"Failed to get worldblock terrain count : $err"
          )
        )
      _ <- ZIO.log(s"starting terrain with count $t_count")
      npchandler <- NPCHandler
        .make()
        .orElseFail(GenericWorldBlockError("Error while creating npchandler"))
      terrain_ref <- Ref.make(terrain)

      globz_map <- Ref.make(Map.empty[GLOBZ_ID, Globz])
      res <- ZIO
        .attempt(WorldBlockInMem(globz_map, terrain_ref, npchandler))
        .orElseFail(
          GenericWorldBlockError("Failed to create woldblock on startup")
        )
      _ <- res.expandTerrain
      _ <- WorldBlockEnvironment
        .add_prowlers(res, num_prowlers, radius)
        .mapError(err =>
          GenericWorldBlockError(
            s"Errow while adding prowlers to worldblock : $err"
          )
        )
    } yield res
}
object WorldBlockEnvironment {

  import entity.implicits._
  def create_terrain_set(
    num: Int,
    radius: Double,
    center: Vector[Double]
  ): IO[WorldBlockError, Set[(TerrainId, Vector[Double])]] =
    for {
      groups <- ZIO.succeed((0 to num).grouped(num / 1000))
      collected <- ZIO
        .collectAllPar(groups.map { r =>
          ZIO
            .foreach(r) { i =>
              for {
                x <- Random.nextDoubleBetween(-radius, radius)
                y <- Random.nextDoubleBetween(-radius, radius)
                z <- Random.nextDoubleBetween(-radius, radius)
                _ <- ZIO.log(s"Generating terrain $i/$num").when(i % 1000 == 0)
              } yield ("6", Vector(x, y, z) + center)
            }
        }.toSeq)
        .map(_.flatten)
    } yield collected.toSet

  def add_terrain(
    terrain: TerrainManager with Terrain,
    radius: Double,
    num: Int
  ): IO[GenericWorldBlockError, TerrainManager & Terrain] = for {
//    terrain <- TerrainRegion.make(
//      Vector(0, 0, 0),
//      radius
//    ) // create terrain region for world block
    groups <- ZIO.succeed((0 to num).grouped(num / 1000))
    _ <- ZIO
      .collectAllPar(groups.map { r =>
        ZIO
          .foreach(r) { i =>
            for {
              x <- Random.nextDoubleBetween(-radius, radius)
              y <- Random.nextDoubleBetween(-radius, radius)
              z <- Random.nextDoubleBetween(-radius, radius)
              _ <- terrain.add_terrain("6", Vector(x, y, z))
              _ <-
                ZIO.log(s"Generating terrain $i/$num").when(i % 1000 == 0)
            } yield ()
          }
      }.toSeq)
      .mapError(err =>
        GenericWorldBlockError(
          s"Failed to add terrain to worldblock due to : $err"
        )
      )
  } yield terrain

  def add_prowlers(worldblock: WorldBlockInMem, count: Int, radius: Double) =
    for {
      prowlers <- ZIO.foreachPar(1 to count) { i =>
        for {
          prowler <- Globz
            .create(s"Prowler_$i")
            .provide(ZLayer.succeed(Prowler))
            .map { case p: Prowler => p }
          maxspeed <- prowler.getMaxSpeed
          speed <- prowler.getSpeed
          _ <- prowler.adjustMaxSpeed(-maxspeed + 30)
          - <- prowler.adjustSpeed(-speed + 30)
        } yield prowler
      }
      _ <- ZIO.foreachDiscard(prowlers) { p =>
        for {
          x <- Random.nextDouble.map(t => (t * radius) - radius / 2)
          y <- Random.nextDouble.map(t => (t * radius) - radius / 2)
          z <- Random.nextDouble.map(t => (t * radius) - radius / 2)
          _ <- worldblock.spawnBlob(p, Vector(x, y, z))
          _ <- p.teleport(Vector(x, y, z))
          _ <- worldblock.npc_handler.add_entity_as_npc(p)
          _ <- worldblock.npc_handler
            .scheduleEgg(
              p,
              p.follow_player("2")
                .provide(ZLayer.succeed(worldblock))
                .mapError(err => err.toString)
            )
            .mapError(err =>
              GenericWorldBlockError(s"Error while adding prowlers $err")
            )
        } yield ()
      }
    } yield ()

}
