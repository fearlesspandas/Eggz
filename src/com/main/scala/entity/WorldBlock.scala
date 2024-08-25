package entity

import entity.WorldBlock.GenericWorldBlockError
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

    val terrain: TerrainManager with Terrain
    def getTerrain: IO[WorldBlockError, TerrainManager with Terrain] =
      ZIO.succeed(terrain)
    // val physics_channel: PhysicsChannel
//    def getPhysicsChannel: IO[WorldBlockError, PhysicsChannel] =
//      ZIO.succeed(physics_channel)
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
    ZIO.service[WorldBlock.Block].flatMap(_.getBlob(id))

  def getTerrain
    : ZIO[WorldBlock.Block, WorldBlockError, TerrainManager with Terrain] =
    ZIO.environmentWithZIO(_.get.getTerrain)

//  def getPhysicsChannel
//    : ZIO[WorldBlock.Block, WorldBlockError, PhysicsChannel] =
//    ZIO.environmentWithZIO(_.get.getPhysicsChannel)

  trait WorldBlockError

  case class GenericWorldBlockError(msg: String) extends WorldBlockError

}

case class WorldBlockInMem(
  dbRef: Ref[Map[GLOBZ_ID, Globz]],
  terrain: TerrainManager with Terrain,
  npc_handler: NPCHandler
//  physics_channel: PhysicsChannel
) extends WorldBlock.Block {

  override def spawnBlob(
    blob: Globz,
    coords: Vector[Double]
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    for {
      _ <- dbRef.update(_.updated(blob.id, blob))
//      _ <- physics_channel.register(blob.id).mapError(_ => ???)
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
  // .mapError(_ => GenericWorldBlockError("error removing blob"))

  override def tickAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, ExitCode] =
    (for {
      all <- getAllBlobs()
      r <- ZIO.collectAllPar(all.map(g => g.tickAll()))
      fail = r.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)).mapError(_ =>
      GenericWorldBlockError("error tick blobs")
    )
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
}
object WorldBlockInMem extends WorldBlock.Service {
  override def make: IO[WorldBlock.WorldBlockError, WorldBlock.Block] =
    for {
      s <- Ref.make(Map.empty[GLOBZ_ID, Vector[Double]])
      t <- Ref.make(Map.empty[GLOBZ_ID, Globz])
      radius <- System
        .env("WORLDBLOCK_RADIUS")
        .flatMap(ZIO.fromOption(_))
        .map(_.toInt)
        .mapError(err =>
          GenericWorldBlockError(s"error initiating worldblock $err")
        )
      terrain <- TerrainRegion.make(
        Vector(0, 0, 0),
        radius
      ) // create terrain region for world block
//      num = 50000
      num <- System
        .env("RANDOMIZED_SPAWN_COUNT")
        .flatMap(ZIO.fromOption(_))
        .map(_.toInt)
        .mapError(err =>
          GenericWorldBlockError(s"Error initiating worldblock $err")
        )

      num_prowlers <- System
        .env("PROWLER_COUNT")
        .flatMap(ZIO.fromOption(_))
        .map(_.toInt)
        .mapError(err =>
          GenericWorldBlockError(s"Error initiating worldblock $err")
        )
      groups = (0 to num).grouped(num / 1000)
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
                  if (i % 1000 == 0)
                    ZIO.log(s"Generating terrain $i/$num")
                  else ZIO.unit
                // _ <- Console.print("\0000b[2J")
              } yield ()
            }
        }.toSeq)
        .mapError(_ => ???)
      _ <- terrain // add spawn block to terrain
        .add_terrain("9", Vector(0, -20, 0))
        .orElseFail(
          GenericWorldBlockError("Could not add spawn block to terrain block")
        )
      t_count <- terrain.get_count().mapError(_ => ???)
      _ <- ZIO.log(s"starting terrain with count $t_count")
      npchandler <- NPCHandler
        .make()
        .orElseFail(GenericWorldBlockError("Error while creating npchandler"))
      res = WorldBlockInMem(t, terrain, npchandler)
      _ <- WorldBlockEnvironment
        .add_prowlers(res, num_prowlers, radius)
        .mapError(_ => ???)
    } yield res
}
object WorldBlockEnvironment {

  def add_prowlers(worldblock: WorldBlockInMem, count: Int, radius: Double) =
    for {
      prowlers <- ZIO.collectAllPar {
        (1 to count).map(i =>
          for {
            prowler <- Globz
              .create(s"Prowler_$i")
              .provide(ZLayer.succeed(Prowler))
              .map { case p: Prowler => p }
            maxspeed <- prowler.getMaxSpeed
            _ <- prowler.adjustMaxSpeed(-maxspeed + 5)
            - <- prowler.adjustSpeed(5)
          } yield prowler
        )
      }
      _ <- ZIO.foreach(prowlers) { p =>
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
            .mapError(_ => ???)
        } yield ()
      }
    } yield ()

}
