package entity

import controller.Stats
import entity.Player.Item
import entity.Player.PlayerEnv
import entity.Player.PlayerError
import entity.Skill.Experience
import entity.Skill.Level
import entity.SkillSet.SkillId
import entity.LivingEntity.*
import physics.BasicDestinations
import physics.Destination
import physics.Destinations
import physics.DestinationsError
import physics.Mode
import src.com.main.scala
import src.com.main.scala.entity
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Eggz.EggzError
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Storage
import src.com.main.scala.entity.StorageEgg
import src.com.main.scala.entity.basicStorage
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz.GLOBZ_OUT
import zio.ExitCode
import zio.IO
import zio.Ref
import zio.ZIO
import zio.ZLayer

import java.util.UUID

trait Player extends LivingEntity {}

object Player {
  type Item = Int
  type PlayerEnv = Globz
  trait PlayerError
}

case class BasicPlayer(
  id: ID,
  skillset: SkillSet,
  inventory: Ref[Storage.Service[Item]]
)(
  val healthRef: Ref[Double],
  val energyRef: Ref[Double],
  val physics: PhysicalEntity,
  val glob: Globz,
  val destinations: Destinations
) extends Player {
  def doAction2[E, B](
    action: ZIO[PlayerEnv, E, B]
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
    } yield PlayerGlob(this.id, stats, location))
      .orElseFail(s"Error while trying to Serialize glob ${glob.id}")

  override def serializeEgg: IO[Eggz.EggzError, EggzModel] =
    for {
      health <- health.orElseFail(PlayerStatsNotFound)
      energy <- energy.orElseFail(PlayerStatsNotFound)
      stats = Stats(id, health, energy)
      loc <- getLocation.orElseFail(PlayerStatsNotFound)
      location <- ZIO
        .succeed(loc(0))
        .zip(ZIO.succeed(loc(1)))
        .zip(ZIO.succeed(loc(2)))
    } yield PLAYER_EGG(id, stats, location)

  override def defaultOP[GLOBZ_OUT]: ZIO[GLOBZ_OUT, GLOBZ_ERR, ExitCode] =
    ZIO.succeed(ExitCode.success)

  override def doAction[E, B](
    action: ZIO[LivingEntityEnv, E, B]
  ): ZIO[LivingEntity, E, B] = ???

  override def op: ZIO[GLOBZ_OUT, GLOBZ_ERR, ExitCode] = ???

  override def setActiveDest(id: UUID): IO[DestinationsError, Unit] =
    destinations.setActiveDest(id)

  override def setIndex(index: Level): IO[DestinationsError, Unit] =
    destinations.setIndex(index)
}

object BasicPlayer extends Globz.Service {
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
    } yield BasicPlayer(id, ss, stor)(href, eref, pe, g, dests)

}
case object PlayerStatsNotFound extends EggzError
