package entity

import entity.WorldBlock.GenericWorldBlockError
import network.example.BasicPhysicsChannel
import network.example.PhysicsChannel
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
  coordsRef: Ref[Map[GLOBZ_ID, Vector[Double]]],
  dbRef: Ref[Map[GLOBZ_ID, Globz]],
  terrain: TerrainManager with Terrain,
  npc_handler: NPCHandler,
  physics_channel: PhysicsChannel
) extends WorldBlock.Block {

  val physics_socket: PhysicsChannel => WebSocketApp[Any] =
    (pc: PhysicsChannel) =>
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text(txt)) =>
            (for {
              r <- ZIO
                .fromEither(txt.fromJson[PhysicsCommand])
                .flatMapError(err => ZIO.log(s"Could not map $txt due to $err"))
                .map(_.asInstanceOf[SendLocation])
              _ <- getBlob(r.id).flatMap(ZIO.fromOption(_)).flatMap {
                case pe: PhysicalEntity =>
                  pe.teleport(r.loc)
              }
              _ <- ZIO.log(s"Found Location $r")
            } yield ()).foldZIO(
              err => ZIO.log(s"processing failed on $txt with err $err"),
              x => ZIO.succeed(x)
            )
          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            (for {
              _ <- pc
                .send("""{
                  |"type" : "SET_GLOB_LOCATION",
                  |"body": {
                  | "id": "Prowler_2",
                  | "location" : [0.0,0,0,0,0]
                  |}
                  |}""".stripMargin)
                .provide(ZLayer.succeed(channel))
                .mapError(_ => ???)
              _ <- pc
                .loop(100)
                .provide(ZLayer.succeed(channel))
                .flatMapError(err =>
                  ZIO.log(s"Error while processing loop ${err.toString}")
                )
                .fork
            } yield ()) *> ZIO.log("Channel Connected")
          case ExceptionCaught(cause) =>
            ZIO.logError(s"Error while handling physics socket $cause")

          case x => ZIO.log(s"other traffic $x")
        }
      }

  val physics_app =
    physics_socket(physics_channel).connect(PhysicsChannel.url) *> ZIO.never

  val start_socket =
    physics_app.provide(Client.default, Scope.default)

  override def spawnBlob(
    blob: Globz,
    coords: Vector[Double]
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    for {
      _ <- coordsRef.update(_.updated(blob.id, coords))
      _ <- dbRef.update(_.updated(blob.id, blob))
      _ <- physics_channel.register(blob.id).mapError(_ => ???)
    } yield ExitCode.success

  override def getAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, Set[Globz]] =
    for {
      db <- dbRef.get
    } yield db.values.toSet

  override def removeBlob(
    blob: Globz
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    for {
      _ <- coordsRef.update(_.removed(blob.id))
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
      terrain <- TerrainRegion.make(
        Vector(0, 0, 0),
        1000
      ) // create terrain region for world block
      // condense output schema to increase payload size
      radius = 1000
      _ <- ZIO
        .foreach(0 to 10000) { _ =>
          for {
            x <- Random.nextDouble.map(t => (t * radius) - radius / 2)
            y <- Random.nextDouble.map(t => (t * radius) - radius / 2)
            z <- Random.nextDouble.map(t => (t * radius) - radius / 2)
            _ <- terrain.add_terrain("6", Vector(x, y, z))
            // _ <- ZIO.log(s"Creating terrain: $x, $y, $z")

          } yield ()
        }
        .orElseFail(
          GenericWorldBlockError("Error while adding randomized terrain")
        )
      _ <- terrain // add spawn block to terrain
        .add_terrain("9", Vector(0, -20, 0))
        .orElseFail(
          GenericWorldBlockError("Could not add spawn block to terrain block")
        )
      all <- terrain
        .get_terrain()
        .mapError(_ => GenericWorldBlockError("Error while gettng terrain"))
      // _ <- ZIO.log(s"Initializing with Terrain: $all")

      npchandler <- NPCHandler
        .make()
        .orElseFail(GenericWorldBlockError("Error while creating npchandler"))
      pc <- Ref
        .make(Seq.empty[GLOBZ_ID])
        .flatMap(queue =>
          Ref
            .make(Set.empty[GLOBZ_ID])
            .map(tracked_ids => BasicPhysicsChannel(tracked_ids, queue))
        )
      res = WorldBlockInMem(s, t, terrain, npchandler, pc)

      _ <- WorldBlockEnvironment.add_prowlers(res, 30, 200).mapError(_ => ???)
      _ <- ZIO.log("Attempting to start physics socket")
      _ <- res.start_socket
        .mapError(err =>
          GenericWorldBlockError(s"Error starting physics socket : $err")
        )
        .flatMapError(err => ZIO.log(err.toString))
        .fork
      _ <- ZIO.log("Physics Socket Started")
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

//  val worldblock =
//    ZLayer[Ref[Map[GLOBZ_ID, Vector[Double]]] with Ref[
//      Map[GLOBZ_ID, Globz]
//    ], Nothing, WorldBlock.Block] {
//      ZIO
//        .service[Ref[Map[GLOBZ_ID, Vector[Double]]]]
//        .flatMap(s =>
//          ZIO.service[Ref[Map[GLOBZ_ID, Globz]]].map(t => WorldBlockInMem(s, t))
//        )
//    }
//  val anyref = ZLayer[Any, Nothing, Ref[Map[GLOBZ_ID, Vector[Double]]]] {
//    for {
//      r <- Ref.make(Map.empty[GLOBZ_ID, Vector[Double]])
//    } yield r
//  } ++ ZLayer[Any, Nothing, Ref[Map[GLOBZ_ID, Globz]]] {
//    for {
//      r <- Ref.make(Map.empty[GLOBZ_ID, Globz])
//    } yield r
//  }

}
