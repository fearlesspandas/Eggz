package entity

import entity.Player.Item
import entity.Player.PlayerEnv
import entity.Player.PlayerError
import entity.Skill.Experience
import entity.Skill.Level
import entity.Skill.SkillError
import entity.SkillSet.SkillId
import entity.SkillSet.SkillsetError
import src.com.main.scala.entity
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.GlobzInMem
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
object SkillSet {
  type SkillId = String
  def make: IO[Nothing, SkillSet] = ???
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

trait Player extends StorageEgg[Item] {

  def doAction[E, B](action: ZIO[PlayerEnv, E, B]): ZIO[Player, E, B]

  def skills: IO[SkillError, Set[Skill]]
  def getName: IO[PlayerError, String]

}

object Player {
  type Item = String
  type PlayerEnv = Globz.Glob
  trait PlayerError
}

case class BasicPlayer(id: ID, skillset: SkillSet, inventory: Ref[Storage.Service[Item]])(
  healthRef: Ref[Double],
  energyRef: Ref[Double],
  glob: Globz.Glob
) extends Player
    with Globz.Glob {
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

  override def op: ZIO[_root_.src.com.main.scala.entity.Globz.Glob, GLOBZ_ERR, ExitCode] =
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

  override def relate(egg1: Eggz.Service, egg2: Eggz.Service): IO[GLOBZ_ERR, Unit] =
    glob.relate(egg1, egg2)

  override def neighbors(egg: Eggz.Service): IO[GLOBZ_ERR, Vector[Eggz.Service]] =
    glob.neighbors(egg)

  override def scheduleEgg(id: entity.Globz.GLOBZ_IN): IO[GLOBZ_ERR, Unit] =
    glob.scheduleEgg(id)
}

object BasicPlayer extends Globz.Service {
  override def create(id: GLOBZ_ID): IO[GLOBZ_ERR, _root_.src.com.main.scala.entity.Globz.Glob] =
    for {
      ss <- SkillSet.make
      stor <- Storage.make[Item](() => basicStorage[Item](Set()))
      href <- Ref.make(1000.0)
      eref <- Ref.make(1000.0)
      g <- GlobzInMem.create(id)
    } yield BasicPlayer(id, ss, stor)(href, eref, g)
}