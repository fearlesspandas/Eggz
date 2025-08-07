package entity
import entity.Eggz.EggzError
import entity.EggzOps.ID
import entity.Globz.GLOBZ_ERR
import entity.Globz.GLOBZ_ID
import zio.*
import entity.Player.Item
import entity.Pocket.POCKET_STORE
import entity.Storage.REF_STORE
import physics.BasicDestinations
import physics.Destinations

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
  val stats    : Ref[Map[StatType,Double]],
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
      location <- getLocation.flatMap(vec =>
        ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2)))
      )
    } yield PlayerGlob(this.id, location, Some(health)))
      .orElseFail(s"Error while trying to Serialize glob ${glob.id}")

}

object BasicPlayer extends Globz.Service {
  override def make(
    id: GLOBZ_ID
  ): IO[GLOBZ_ERR, Globz] =
    for {
      stats <- Ref.make(Map.empty[StatType,Double])
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
      stats,
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
