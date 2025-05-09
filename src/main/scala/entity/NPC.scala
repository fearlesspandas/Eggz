package entity

import zio.*
import controller.QueryResponse
import controller.QueuedClientMessage
import controller.Stats
import entity.Ability.ABILITY_ID
import entity.Eggz.EggzError
import entity.EggzOps.ID
import entity.Globz.GLOBZ_ERR
import entity.Globz.GLOBZ_ID
import entity.Player.Item
import entity.Pocket.POCKET_STORE
import entity.Storage.REF_STORE
import entity.implicits.*
import physics.BasicDestinations
import physics.Destinations
trait NPC extends LivingEntity {
  def follow_and_attack(ability_id: ABILITY_ID, target: GLOBZ_ID) = for {
    _ <- this.follow_player(target)
    _ <- this.attack_within_distance(ability_id, target)
  } yield ()
  def attack_within_distance(
    ability_id: ABILITY_ID,
    target: GLOBZ_ID
  ): ZIO[WorldBlock.Block, NPC_ERROR, Unit] = for {
    ability <- Ability
      .make(ability_id, this.id, (0, 0))
      .orElseFail {
        AttackWithinDistancError(
          s"Error while trying to create ability with id : $ability_id"
        )
      }
    loc <- this.physics.getLocation.orElseFail(
      AttackWithinDistancError(s"Couldn't get location for id ${this.id}")
    )
    target_entity <- WorldBlock
      .getBlobOption(target)
      .flatMap(ZIO.fromOption(_))
      .mapBoth(
        _ => AttackWithinDistancError(s"Could not find target entity $target"),
        { case li: LivingEntity => li }
      )
    target_loc <- target_entity.physics.getLocation.orElseFail(
      AttackWithinDistancError(
        s"Couldn't get target entity location for $target"
      )
    )
    controller <- WorldBlock.get_controller().mapError(_ => ???)
    res <- ability.run
      .orElseFail(
        AttackWithinDistancError("Error while attempting ability")
      )
      .flatMap(responses => controller.queueQuery(ZIO.succeed(responses)))
      .when((target_loc - loc).length() <= 10)
    // .orElseFail(AttackWithinDistancError("Ability not within distance"))
    // _ <- controller.queueQuery(ZIO.succeed(res))
  } yield res
}
trait NPC_ERROR
case class GenericNPCError(msg: String) extends NPC_ERROR
case class AttackWithinDistancError(msg: String) extends NPC_ERROR
case class Prowler(
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
) extends NPC {

  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] =
    (for {
      health <- this.health
      location <- getLocation.flatMap(vec =>
        ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2)))
      )
    } yield ProwlerModel(this.id, location, Some(health)))
      .orElseFail(s"Error while trying to Serialize glob ${glob.id}")
}

object Prowler extends Globz.Service {
  override def make(
    id: GLOBZ_ID
  ): IO[GLOBZ_ERR, Globz] =
    for {
      ss <- SkillSet.make.provide(ZLayer.succeed(BasicSkillset))
      href <- Ref.make(1000.0)
      eref <- Ref.make(1000.0)
      fieldOps <- FieldOps.make()
      pe <- BasicPhysicalEntity.make
      g <- GlobzInMem.make(id)
      dests <- BasicDestinations.make()
      ability_data <- Ref.make(Map.empty[DATA_TYPE, DATA])
      pocket_contents <- Pocket.make
      inventory <- Storage.make[Item]
      res = Prowler(id, ss)(
        href,
        eref,
        ability_data,
        pocket_contents,
        inventory,
        pe,
        g,
        dests,
        fieldOps
      )
      _ <- res
        .adjustMaxSpeed(10)
        .mapError(e => s"Prowler:Could not adjust speed on creation: ${e}")
    } yield res

}

case class Spider(
  id: ID,
  skillset: SkillSet
)(
  val healthRef: Ref[Map[GLOBZ_ID, Double]],
  val energyRef: Ref[Map[GLOBZ_ID, Double]],
  val storage: REF_STORE[Item],
  val physics: PhysicalEntity,
  val glob: Globz,
  val destinations: Destinations
) extends InstanceEntity {

  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] =
    (for {
      health <- ZIO.succeed(this.starting_health)
      energy <- ZIO.succeed(this.starting_energy)
      stats = Stats(health)
      location <- getLocation.flatMap(vec =>
        ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2)))
      )
    } yield AxisSpiderModel(this.id, stats, location))
      .orElseFail(s"Error while trying to Serialize glob ${glob.id}")

  override val starting_health: Double = 100000
  override val starting_energy: Double = 100000

}

object Spider extends Globz.Service {
  override def make(
    id: GLOBZ_ID
  ): IO[GLOBZ_ERR, Globz] =
    for {
      ss <- SkillSet.make.provide(ZLayer.succeed(BasicSkillset))
      href <- Ref.make(Map.empty[GLOBZ_ID, Double])
      eref <- Ref.make(Map.empty[GLOBZ_ID, Double])
      inventory <- Storage.make[Item]
      pe <- BasicPhysicalEntity.make
      g <- GlobzInMem.make(id)
      dests <- BasicDestinations.make()
      res = Spider(id, ss)(href, eref, inventory, pe, g, dests)
      _ <- res
        .adjustMaxSpeed(10)
        .orElseFail("failed while making axis spider")
    } yield res

}

case class MonkGarden(
  id: ID,
  physics: PhysicalEntity,
  glob: Globz,
  inventory: Ref[Chunk[ABILITY_ID]]
) extends TerrainEntity {
  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] = for {
    items <- inventory.get.map(_.toSet)
    res <-
      physics.getLocation
        .flatMap(vec =>
          ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2)))
        )
        .mapBoth(_ => "", loc => MonkGardenModel(id, loc, items))
  } yield res

  def getInventory(): UIO[Chunk[ABILITY_ID]] =
    inventory.get

  def buyItem(
    entityId: GLOBZ_ID,
    item: ABILITY_ID
  ): ZIO[WorldBlock.Block, MonkGardenAPIError, QueryResponse] = for {
    entity <- WorldBlock
      .getBlob(entityId)
      .orElseFail(???)
  } yield QueuedClientMessage(entityId, Chunk())
}
trait MonkGardenAPIError
object MonkGarden {
  def make(id: ID): IO[GLOBZ_ERR, MonkGarden] = for {
    physics <- BasicPhysicalEntity.make
    glob <- GlobzInMem.make(id)
    inventory <- Ref.make(Chunk(0, 1, 2))
  } yield MonkGarden(id, physics, glob, inventory)

}

case class Planet(
  id: ID,
  physics: PhysicalEntity,
  glob: Globz
) extends TerrainEntity {
  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] = for {
    res <-
      physics.getLocation
        .flatMap(vec =>
          ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2)))
        )
        .mapBoth(_ => "", loc => PlanetModel(id, loc))
  } yield res

}
trait PlanetAPIError
object Planet {
  def make(id: ID): IO[GLOBZ_ERR, Planet] = for {
    physics <- BasicPhysicalEntity.make
    glob <- GlobzInMem.make(id)
  } yield Planet(id, physics, glob)

}
case object NPCStatsNotFoundError extends EggzError
