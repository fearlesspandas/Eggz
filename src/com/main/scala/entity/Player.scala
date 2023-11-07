package entity

import entity.Player.Item
import entity.Player.PlayerEnv
import entity.Player.PlayerError
import entity.Skill.Experience
import entity.Skill.Level
import entity.Skill.SkillError
import entity.SkillSet.SkillId
import entity.SkillSet.SkillsetError
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Storage
import zio.ExitCode
import zio.IO
import zio.ZIO

trait SkillSet {
  def getSkills: IO[SkillsetError, Set[Skill]]
  def getSkill(id: SkillId): IO[SkillsetError, Skill]
  def getTotalExperience: IO[SkillsetError, Experience]
  def update(skill: Skill): IO[SkillsetError, ExitCode]
}
object SkillSet {
  type SkillId = String
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

trait Player extends Storage.Service[Item] {

  def doAction[E, B](action: ZIO[PlayerEnv, E, B]): ZIO[Player, E, B]

  def skills: IO[PlayerError, Set[Skill]]
  def getName: IO[PlayerError, String]

}

object Player {
  type Item = String
  type PlayerEnv = Globz.Service
  trait PlayerError
}

case class BasicPlayer(skillset: SkillSet) extends Player {
  override def doAction[E, B](action: ZIO[PlayerEnv, E, B]): ZIO[Player, E, B] = ???

  override def skills: IO[PlayerError, Set[Skill]] = ???

  override def getName: IO[PlayerError, String] = ???

  override def add(item: Item*): IO[Storage.ServiceError, Storage.Service[Item]] = ???

  override def remove(item: Item*): IO[Storage.ServiceError, Storage.Service[Item]] = ???

  override def getAll(): IO[Storage.ServiceError, Set[Item]] = ???
}
