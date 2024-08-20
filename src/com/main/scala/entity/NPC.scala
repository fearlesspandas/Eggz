package entity

import controller.Stats
import entity.LivingEntity.LivingEntityEnv
import entity.Player.Item
import physics.BasicDestinations
import physics.Destination
import physics.Destinations
import physics.DestinationsError
import physics.Mode
import physics.WaypointDestination
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Storage
import src.com.main.scala.entity.basicStorage
import zio.ExitCode
import zio.IO
import zio.Ref
import zio.ZIO
import zio.ZLayer

trait NPC extends LivingEntity
trait NPC_ERROR
case class GenericNPCError(msg: String) extends NPC_ERROR
case class Prowler(
  id: ID,
  skillset: SkillSet,
  inventory: Ref[Storage.Service[Item]]
)(
  val healthRef: Ref[Double],
  val energyRef: Ref[Double],
  val physics: PhysicalEntity,
  val glob: Globz,
  val destinations: Destinations
) extends NPC {
  override def doAction[E, B](
    action: ZIO[LivingEntityEnv, E, B]
  ): ZIO[LivingEntity, E, B] =
    ???

  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] =
    (for {
      health <- this.health
      energy <- this.energy
      stats = Stats(this.id, health, energy)
      location <- getLocation.flatMap(vec =>
        ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2)))
      )
    } yield ProwlerModel(this.id, stats, location))
      .orElseFail(s"Error while trying to Serialize glob ${glob.id}")

  override def serializeEgg: IO[Eggz.EggzError, EggzModel] =
    for {
      health <- health
      energy <- energy
      stats = Stats(id, health, energy)
      loc <- getLocation.orElseFail(???)
      location <- ZIO
        .succeed(loc(0))
        .zip(ZIO.succeed(loc(1)))
        .zip(ZIO.succeed(loc(2)))
    } yield PROWLER_EGG(id, stats, location)

  def follow_player(id: ID): ZIO[WorldBlock.Block, NPC_ERROR, Unit] = for {
    worldblock <- ZIO.service[WorldBlock.Block]
    player <- worldblock
      .getBlob(id)
      .flatMap { l =>
        ZIO.fromOption(l)
      }
      .mapError(_ => GenericNPCError(s"Player not found for $id"))
    _ <- player match {
      case l: LivingEntity =>
        for {
          loc <- l.physics.getLocation.mapError(_ => ???)
          _ <- destinations.clearDestinations().mapError(_ => ???)
          _ <- destinations
            .addDestination(WaypointDestination(loc, 0))
            .mapError(_ => ???)
          ms <- physics.getMaxSpeed.mapError(_ => ???)
          _ <-
            if (ms <= 0) physics.adjustMaxSpeed(1).mapError(_ => ???)
            else ZIO.unit
        } yield ()
    }
  } yield ()

  override def defaultOP[Env]: ZIO[Env, GLOBZ_ERR, ExitCode] = ???

  override def op: ZIO[Globz, GLOBZ_ERR, ExitCode] = ???
}

object Prowler extends Globz.Service {
  override def make(
    id: GLOBZ_ID
  ): IO[GLOBZ_ERR, _root_.src.com.main.scala.entity.Globz] =
    for {
      ss <- SkillSet.make.provide(ZLayer.succeed(BasicSkillset))
      stor <- Storage.make[Item](() => basicStorage[Item](Set()))
      href <- Ref.make(1000.0)
      eref <- Ref.make(1000.0)
      pe <- BasicPhysicalEntity.make
      g <- GlobzInMem.make(id)
      dests <- BasicDestinations.make()
      res = Prowler(id, ss, stor)(href, eref, pe, g, dests)
      _ <- res.adjustMaxSpeed(10).mapError(_ => ???)
    } yield res

}
