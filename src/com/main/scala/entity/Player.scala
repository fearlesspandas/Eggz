package entity

import controller.Stats
import entity.Player.Item
import entity.Player.PlayerEnv
import entity.Player.PlayerError
import entity.Skill.Experience
import entity.Skill.Level
import entity.Skill.SkillError
import entity.SkillSet.SkillId
import entity.SkillSet.SkillsetError
import physics.BasicDestinations
import physics.DestinationError
import physics.Destinations
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

trait SkillSet {
  def getSkills: IO[SkillError, Set[Skill]]
  def getSkill(id: SkillId): IO[SkillError, Skill]
  def getTotalExperience: IO[SkillError, Experience]
  def update(skill: Skill): IO[SkillError, ExitCode]

}
case class BasicSkillset() extends SkillSet {
  override def getSkills: IO[SkillError, Set[Skill]] = ???

  override def getSkill(id: SkillId): IO[SkillError, Skill] = ???

  override def getTotalExperience: IO[SkillError, Experience] = ???

  override def update(skill: Skill): IO[SkillError, ExitCode] = ???
}
object BasicSkillset extends SkillSet.Service {
  override def make: IO[Nothing, SkillSet] = ZIO.succeed(BasicSkillset())
}
object SkillSet {
  type SkillId = String
  trait Service {
    def make: IO[Nothing, SkillSet]
  }
  def make: ZIO[SkillSet.Service, Nothing, SkillSet] = ZIO.service[SkillSet.Service].flatMap(_.make)
  trait SkillsetError
}
trait Skill {
  def getExp: IO[SkillError, Experience]
  def addExperience: IO[SkillError, Unit]
  def getLevel: IO[SkillError, Level]
}
object Skill {
  type Experience = Double
  type Level = Int
  trait SkillError
}

trait Player extends StorageEgg[Item] with Globz with PhysicalEntity {

  def doAction[E, B](action: ZIO[PlayerEnv, E, B]): ZIO[Player, E, B]

  def skills: IO[SkillError, Set[Skill]]
  def getName: IO[PlayerError, String]

}

object Player {
  type Item = String
  type PlayerEnv = Globz
  trait PlayerError
}

case class BasicPlayer(id: ID, skillset: SkillSet, inventory: Ref[Storage.Service[Item]])(
  healthRef: Ref[Double],
  energyRef: Ref[Double],
  physics: PhysicalEntity,
  glob: Globz,
  destinations: Destinations
) extends Player
    with Destinations {
  override def doAction[E, B](action: ZIO[PlayerEnv, E, B]): ZIO[Player, E, B] = ???

  override def skills: IO[SkillError, Set[Skill]] = skillset.getSkills

  override def getName: IO[PlayerError, String] = ZIO.succeed(id)

  override def add(item: Item*): IO[Storage.ServiceError, Storage.Service[Item]] =
    inventory.get.flatMap(_.add(item: _*))

  override def remove(item: Item*): IO[Storage.ServiceError, Storage.Service[Item]] =
    inventory.get.flatMap(_.remove(item: _*))

  override def getInventory(): IO[Storage.ServiceError, Set[Item]] =
    inventory.get.flatMap(_.getInventory())

  override def setHealth(health: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- healthRef.update(_ => health)
    } yield this

  override def setEnergy(value: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- healthRef.update(_ => value)
    } yield this

  override def health(): IO[Eggz.EggzError, Double] = healthRef.get

  override def energy(): IO[Eggz.EggzError, Double] = energyRef.get

  override def op: ZIO[_root_.src.com.main.scala.entity.Globz, GLOBZ_ERR, ExitCode] =
    ZIO.succeed(ExitCode.success)

  override def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT] =
    glob.update(eggz)

  override def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_IN]] =
    glob.get(id)

  override def remove(id: ID): IO[GLOBZ_ERR, Unit] =
    glob.remove(id)

  override def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]] =
    glob.getAll()

  override def tickAll(): ZIO[Any, GLOBZ_ERR, ExitCode] =
    glob.tickAll()

  override def relate(
    egg1: Eggz.Service,
    egg2: Eggz.Service,
    bidirectional: Boolean
  ): IO[GLOBZ_ERR, Unit] =
    glob.relate(egg1, egg2, bidirectional)

  override def neighbors(egg: Eggz.Service, direction: Int): IO[GLOBZ_ERR, Vector[Eggz.Service]] =
    glob.neighbors(egg, direction)

  override def scheduleEgg(id: GLOBZ_IN): IO[GLOBZ_ERR, Unit] =
    glob.scheduleEgg(id)

  override def getLocation: IO[PhysicsError, Vector[Experience]] = physics.getLocation

  override def getDestination: IO[PhysicsError, Option[Vector[Experience]]] = physics.getDestination

  override def setDestination(dest: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.setDestination(dest)

  override def getVelocity: IO[PhysicsError, Vector[Experience]] = physics.getVelocity

  override def move(location: Vector[Experience]): IO[PhysicsError, Unit] = physics.move(location)

  override def teleport(location: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.teleport(location)

  override def setVelocity(velocity: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.setVelocity(velocity)

  override def addDestination(location: Vector[Experience]): IO[DestinationError, Unit] =
    destinations.addDestination(location)

  override def getNextDestination(): IO[DestinationError, Option[Vector[Experience]]] =
    destinations.getNextDestination()

  override def getAllDestinations(): IO[DestinationError, Seq[Vector[Experience]]] =
    destinations.getAllDestinations()

  override def popNextDestination(): IO[DestinationError, Option[Vector[Experience]]] =
    destinations.popNextDestination()

  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] =
    (for {
      health <- this.health()
      energy <- this.energy()
      stats = Stats(this.id, health, energy)
      location <- getLocation.flatMap(vec =>
        ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2)))
      )
    } yield PlayerGlob(this.id, stats, location))
      .orElseFail(s"Error while trying to Serialize glob ${glob.id}")

  override def serializeEgg: IO[Eggz.EggzError, EggzModel] =
    for {
      health <- health()
      energy <- energy()
      stats = Stats(id, health, energy)
      loc <- getLocation.orElseFail(???)
      location <- ZIO.succeed(loc(0)).zip(ZIO.succeed(loc(1))).zip(ZIO.succeed(loc(2)))
    } yield PLAYER_EGG(id, stats, location)

  override def unrelate(
    egg1: scala.entity.Globz.GLOBZ_IN,
    egg2: scala.entity.Globz.GLOBZ_IN,
    bidirectional: Boolean
  ): IO[GLOBZ_ERR, Unit] = glob.unrelate(egg1, egg2, bidirectional)

  override def unrelateAll(
    egg: scala.entity.Globz.GLOBZ_IN,
    direction: Level
  ): IO[GLOBZ_ERR, Unit] = glob.unrelateAll(egg, direction)

  override def setInputVec(vec: Vector[Experience]): IO[PhysicsError, Unit] =
    physics.setInputVec(vec)

  override def getInputVec(): IO[PhysicsError, Option[Vector[Experience]]] = physics.getInputVec()

  override def clearDestinations(): IO[DestinationError, Unit] = destinations.clearDestinations()

  override def adjustMaxSpeed(delta: Experience): IO[PhysicsError, Unit] =
    physics.adjustMaxSpeed(delta)

  override def getMaxSpeed(): IO[PhysicsError, Experience] = physics.getMaxSpeed()
}

object BasicPlayer extends Globz.Service {
  override def make(id: GLOBZ_ID): IO[GLOBZ_ERR, _root_.src.com.main.scala.entity.Globz] =
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
