package entity

import controller.HealthSet
import controller.MSG
import controller.MultiResponse
import controller.QueryResponse
import controller.QueuedClientBroadcast
import controller.QueuedPhysicsMessage
import controller.QueuedServerMessage
import controller.Stats
import controller.TeleportToNext
import entity.Ability.ABILITY_ID
import entity.Player.Item
import entity.Player.PlayerEnv
import entity.Player.PlayerError
import entity.Pocket.POCKET_STORE
import entity.Skill.Experience
import entity.Skill.Level
import entity.SkillSet.SkillId
import entity.LivingEntity.*
import physics.BasicDestinations
import physics.Destination
import physics.Destinations
import physics.DestinationsError
import physics.Mode
import physics.PhysicsTeleport
import src.com.main.scala
import src.com.main.scala.entity
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Eggz.EggzError
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Storage
import src.com.main.scala.entity.StorageEgg
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz.GLOBZ_OUT
import src.com.main.scala.entity.Storage.REF_STORE
import zio.Chunk
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
  skillset: SkillSet
)(
  val healthRef: Ref[Double],
  val energyRef: Ref[Double],
  val ability_data_ref: Ref[Map[DATA_TYPE, DATA]],
  val pocket_contents: POCKET_STORE,
  val storage: REF_STORE[Item],
  val physics: PhysicalEntity,
  val glob: Globz,
  val destinations: Destinations,
  val fieldOps: FieldOps
) extends Player {

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

}

object BasicPlayer extends Globz.Service {
  override def make(
    id: GLOBZ_ID
  ): IO[GLOBZ_ERR, _root_.src.com.main.scala.entity.Globz] =
    for {
      ss <- SkillSet.make.provide(ZLayer.succeed(BasicSkillset))
      href <- Ref.make(1000.0)
      eref <- Ref.make(1000.0)
      pe <- BasicPhysicalEntity.make
      g <- GlobzInMem.make(id)
      dests <- BasicDestinations.make()
      field_ops <- FieldOps.make()
      ability_data <- Ref.make(Map.empty[DATA_TYPE, DATA])
      pocket_contents <- Pocket.make
      inventory <- Storage.make[Item]
    } yield BasicPlayer(id, ss)(
      href,
      eref,
      ability_data,
      pocket_contents,
      inventory,
      pe,
      g,
      dests,
      field_ops
    )

}
case object PlayerStatsNotFound extends EggzError
