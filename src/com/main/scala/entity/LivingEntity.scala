package entity

import controller.Stats
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
import physics.WaypointDestination
import src.com.main
import src.com.main.scala
import src.com.main.scala.entity
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Storage
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz.GLOBZ_OUT
import zio.ExitCode
import zio.Fiber
import zio.IO
import zio.Ref
import zio.ZIO

import java.util.UUID

trait LivingEntity
    extends Storage.Service[Item]
    with Eggz.Service
    with Globz
    with PhysicalEntity
    with Health
    with AbilityData
    with Destinations {

  def doAction[E, B](
    action: ZIO[LivingEntityEnv, E, B]
  ): ZIO[LivingEntity, E, B]

  def defaultOP[Env]: ZIO[Env, GLOBZ_ERR, ExitCode]

  val id: ID

  val skillset: SkillSet

  val inventory: Storage.Service[Item]

  val healthRef: Ref[Double]

  val energyRef: Ref[Double]

  val physics: PhysicalEntity

  val glob: Globz

  val destinations: Destinations

  def skills: IO[SkillError, Set[Skill]] = skillset.getSkills

  def getName: IO[PlayerError, String] = ZIO.succeed(id)

  def add(
    item: Item*
  ): IO[Storage.ServiceError, Storage.Service[Item]] =
    inventory.add(item: _*)
  def remove(
    item: Item*
  ): IO[Storage.ServiceError, Storage.Service[Item]] =
    inventory.remove(item: _*)

  def getInventory(): IO[Storage.ServiceError, Set[Item]] =
    inventory.getInventory()

  def setHealth(health: Double): IO[HealthError, Health] =
    for {
      _ <- healthRef.update(_ => health)
    } yield this

  def setEnergy(value: Double): IO[HealthError, Health] =
    for {
      _ <- healthRef.update(_ => value)
    } yield this

  def health: IO[HealthError, Double] = healthRef.get

  def energy: IO[HealthError, Double] = energyRef.get

  def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT] =
    glob.update(eggz)

  def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_IN]] =
    glob.get(id)

  def remove(id: ID): IO[GLOBZ_ERR, Unit] =
    glob.remove(id)

  def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]] =
    glob.getAll()

  def tickAll(): ZIO[Any, GLOBZ_ERR, ExitCode] =
    glob.tickAll()

  def relate(
    egg1: GLOBZ_ID,
    egg2: GLOBZ_ID,
    bidirectional: Boolean,
    process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] =
    glob.relate(egg1, egg2, bidirectional, process)

  def neighbors(
    egg: GLOBZ_ID,
    direction: Int
  ): IO[GLOBZ_ERR, Vector[GLOBZ_ID]] =
    glob.neighbors(egg, direction)

  def scheduleEgg(
    egg: GLOBZ_IN,
    op: ZIO[GLOBZ_IN, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] =
    glob.scheduleEgg(egg, op) // change to check if id is for player

  def getLocation: IO[PhysicsError, Vector[Experience]] =
    physics.getLocation

  def getVelocity: IO[PhysicsError, Vector[Experience]] =
    physics.getVelocity

  def teleport(location: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.teleport(location)

  def setVelocity(
    velocity: Vector[Experience]
  ): IO[PhysicsError, Unit] =
    physics.setVelocity(velocity)

  def addDestination(dest: Destination): IO[DestinationsError, Unit] =
    destinations.addDestination(dest)

  def getNextDestination(): IO[DestinationsError, Option[Destination]] =
    destinations.getNextDestination()

  def getAllDestinations(): IO[DestinationsError, Seq[Destination]] =
    destinations.getAllDestinations()

  def popNextDestination(): IO[DestinationsError, Option[Destination]] =
    destinations.popNextDestination()

  def toggleDestinations(): IO[DestinationsError, Unit] =
    destinations.toggleDestinations()

  override def setIsActive(value: Boolean): IO[DestinationsError, Unit] =
    destinations.setIsActive(value)
  def isActive(): IO[DestinationsError, Boolean] =
    destinations.isActive()

  def toggleGravitate(): IO[DestinationsError, Unit] =
    destinations.toggleGravitate()

  override def setGravitate(value: Boolean): IO[DestinationsError, Unit] =
    destinations.setGravitate(value)

  def isGravitating(): IO[DestinationsError, Boolean] =
    destinations.isGravitating()

  def setMode(mode: Mode): IO[DestinationsError, Unit] =
    destinations.setMode(mode)

  def getMode(): IO[DestinationsError, Mode] = destinations.getMode()

  def getIndex(): IO[DestinationsError, Level] =
    destinations.getIndex()

  def getDestAtIndex(ind: Int): IO[DestinationsError, Option[Destination]] =
    destinations.getDestAtIndex(ind)

  def follow_player(id: ID): ZIO[WorldBlock.Block, NPC_ERROR, Unit] = for {
    worldblock <- ZIO.service[WorldBlock.Block]
    player <- worldblock
      .getBlob(id)
      .flatMap { l =>
        ZIO.fromOption(l)
      }
      .orElseFail(GenericNPCError(s"Player not found for $id"))
    _ <- player match {
      case l: LivingEntity =>
        for {
          loc <- l.physics.getLocation.orElseFail(
            GenericNPCError(s"Could not find location for entity ${l.id}")
          )
          _ <- destinations
            .clearDestinations()
            .orElseFail(GenericNPCError(s"Error trying to clear destinations"))
          _ <- destinations
            .addDestination(WaypointDestination(loc, 0))
            .orElseFail(GenericNPCError("Error while adding destination"))
        } yield ()
    }
  } yield ()

  override def getDestAtCurrentIndex()
    : IO[DestinationsError, Option[Destination]] =
    destinations.getDestAtCurrentIndex()

  override def deleteDest(uuid: UUID): IO[DestinationsError, Unit] =
    destinations.deleteDest(uuid)

  def increment(): IO[DestinationsError, Unit] =
    destinations.increment()

  def decrement(): IO[DestinationsError, Unit] =
    destinations.decrement()

  def unrelate(
    egg1: GLOBZ_ID,
    egg2: GLOBZ_ID,
    bidirectional: Boolean,
    process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] = glob.unrelate(egg1, egg2, bidirectional, process)

  def unrelateAll(
    egg: GLOBZ_ID,
    direction: Level,
    cleanup_process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] = glob.unrelateAll(egg, direction, cleanup_process)

  def setInputVec(vec: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.setInputVec(vec)

  def getInputVec: IO[PhysicsError, Option[Vector[Experience]]] =
    physics.getInputVec

  def clearDestinations(): IO[DestinationsError, Unit] =
    destinations.clearDestinations()

  def adjustMaxSpeed(delta: Experience): IO[PhysicsError, Unit] =
    physics.adjustMaxSpeed(delta)

  def getMaxSpeed: IO[PhysicsError, Experience] =
    physics.getMaxSpeed

  def adjustSpeed(delta: Double): IO[PhysicsError, Unit] =
    physics.adjustSpeed(delta)

  def getSpeed: IO[PhysicsError, Experience] = physics.getSpeed
}

object LivingEntity {
  type Item = Int
  type LivingEntityEnv = Globz with WorldBlock.Block
  trait PlayerError
}
