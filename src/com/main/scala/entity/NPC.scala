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
import src.com.main.scala.entity.Eggz.EggzError
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Storage
import src.com.main.scala.entity.basicStorage
import zio.ExitCode
import zio.IO
import zio.Ref
import zio.ZIO
import zio.ZLayer

import java.util.UUID

trait NPC extends LivingEntity
trait NPC_ERROR
case class GenericNPCError(msg: String) extends NPC_ERROR
case class Prowler(
  id: ID,
  skillset: SkillSet,
  inventory: Storage.Service[Item]
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
      health <- health.orElseFail(NPCStatsNotFoundError)
      energy <- energy.orElseFail(NPCStatsNotFoundError)
      stats = Stats(id, health, energy)
      loc <- getLocation.orElseFail(NPCStatsNotFoundError)
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
        } yield ()
    }
  } yield ()

  override def defaultOP[Env]: ZIO[Env, GLOBZ_ERR, ExitCode] = ???

  override def op: ZIO[Globz, GLOBZ_ERR, ExitCode] = ???

  override def setActiveDest(id: UUID): IO[DestinationsError, Unit] =
    destinations.setActiveDest(id)

  override def setIndex(index: Int): IO[DestinationsError, Unit] =
    destinations.setIndex(index)
}

object Prowler extends Globz.Service {
  override def make(
    id: GLOBZ_ID
  ): IO[GLOBZ_ERR, _root_.src.com.main.scala.entity.Globz] =
    for {
      ss <- SkillSet.make.provide(ZLayer.succeed(BasicSkillset))
      stor <- Storage.make[Item]
      href <- Ref.make(1000.0)
      eref <- Ref.make(1000.0)
      pe <- BasicPhysicalEntity.make
      g <- GlobzInMem.make(id)
      dests <- BasicDestinations.make()
      res = Prowler(id, ss, stor)(href, eref, pe, g, dests)
      _ <- res.adjustMaxSpeed(10).mapError(_ => ???)
    } yield res

}

case class Spider(
  id: ID,
  skillset: SkillSet,
  inventory: Storage.Service[Item]
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
    } yield AxisSpiderModel(this.id, stats, location))
      .orElseFail(s"Error while trying to Serialize glob ${glob.id}")

  override def serializeEgg: IO[Eggz.EggzError, EggzModel] =
    for {
      health <- health.orElseFail(NPCStatsNotFoundError)
      energy <- energy.orElseFail(NPCStatsNotFoundError)
      stats = Stats(id, health, energy)
      loc <- getLocation.orElseFail(NPCStatsNotFoundError)
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
        } yield ()
    }
  } yield ()

  override def defaultOP[Env]: ZIO[Env, GLOBZ_ERR, ExitCode] = ???

  override def op: ZIO[Globz, GLOBZ_ERR, ExitCode] = ???

  override def setActiveDest(id: UUID): IO[DestinationsError, Unit] =
    destinations.setActiveDest(id)

  override def setIndex(index: Int): IO[DestinationsError, Unit] =
    destinations.setIndex(index)
}

object Spider extends Globz.Service {
  override def make(
    id: GLOBZ_ID
  ): IO[GLOBZ_ERR, _root_.src.com.main.scala.entity.Globz] =
    for {
      ss <- SkillSet.make.provide(ZLayer.succeed(BasicSkillset))
      stor <- Storage.make[Item]
      href <- Ref.make(1000.0)
      eref <- Ref.make(1000.0)
      pe <- BasicPhysicalEntity.make
      g <- GlobzInMem.make(id)
      dests <- BasicDestinations.make()
      res = Spider(id, ss, stor)(href, eref, pe, g, dests)
      _ <- res
        .adjustMaxSpeed(10)
        .orElseFail("failed while making axis spider")
    } yield res

}
case object NPCStatsNotFoundError extends EggzError
