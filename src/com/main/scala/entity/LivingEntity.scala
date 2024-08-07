package entity

import controller.Stats
import entity.Player.Item
import entity.Player.PlayerError
import entity.Skill.Experience
import entity.Skill.Level
import entity.SkillSet.SkillId
import entity.LivingEntity.*
import physics.BasicDestinations
import physics.Destination
import physics.Destinations
import physics.DestinationsError
import src.com.main
import src.com.main.scala
import src.com.main.scala.entity
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
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

trait LivingEntity
    extends StorageEgg[Item]
    with Globz
    with PhysicalEntity
    with Destinations {

  def doAction[E, B](
    action: ZIO[LivingEntityEnv, E, B]
  ): ZIO[LivingEntity, E, B]

  def defaultOP[Env]: ZIO[Env, GLOBZ_ERR, ExitCode]

  val id: ID

  val skillset: SkillSet

  val inventory: Ref[Storage.Service[Item]]

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
    inventory.get.flatMap(_.add(item: _*))

  def remove(
    item: Item*
  ): IO[Storage.ServiceError, Storage.Service[Item]] =
    inventory.get.flatMap(_.remove(item: _*))

  def getInventory(): IO[Storage.ServiceError, Set[Item]] =
    inventory.get.flatMap(_.getInventory())

  def setHealth(health: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- healthRef.update(_ => health)
    } yield this

  def setEnergy(value: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- healthRef.update(_ => value)
    } yield this

  def health: IO[Eggz.EggzError, Double] = healthRef.get

  def energy: IO[Eggz.EggzError, Double] = energyRef.get

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
    egg1: Eggz.Service,
    egg2: Eggz.Service,
    bidirectional: Boolean
  ): IO[GLOBZ_ERR, Unit] =
    glob.relate(egg1, egg2, bidirectional)

  def neighbors(
    egg: Eggz.Service,
    direction: Int
  ): IO[GLOBZ_ERR, Vector[Eggz.Service]] =
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

  def move(location: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.move(location)

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

  def unrelate(
    egg1: scala.entity.Globz.GLOBZ_IN,
    egg2: scala.entity.Globz.GLOBZ_IN,
    bidirectional: Boolean
  ): IO[GLOBZ_ERR, Unit] = glob.unrelate(egg1, egg2, bidirectional)

  def unrelateAll(
    egg: scala.entity.Globz.GLOBZ_IN,
    direction: Level
  ): IO[GLOBZ_ERR, Unit] = glob.unrelateAll(egg, direction)

  def setInputVec(vec: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.setInputVec(vec)

  def getInputVec(): IO[PhysicsError, Option[Vector[Experience]]] =
    physics.getInputVec()

  def clearDestinations(): IO[DestinationsError, Unit] =
    destinations.clearDestinations()

  def adjustMaxSpeed(delta: Experience): IO[PhysicsError, Unit] =
    physics.adjustMaxSpeed(delta)

  def getMaxSpeed(): IO[PhysicsError, Experience] =
    physics.getMaxSpeed()
}

object LivingEntity {
  type Item = String
  type LivingEntityEnv = Globz with WorldBlock.Block
  trait PlayerError
}
