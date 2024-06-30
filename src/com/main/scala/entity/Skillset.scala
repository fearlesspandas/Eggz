package entity

import entity.Skill.{Experience, Level}
import entity.SkillSet.SkillId
import zio.ExitCode
import zio.IO
import zio.ZIO

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

trait SkillsetError
object SkillSet {
  type SkillId = String
  trait Service {
    def make: IO[Nothing, SkillSet]
  }
  def make: ZIO[SkillSet.Service, Nothing, SkillSet] =
    ZIO.service[SkillSet.Service].flatMap(_.make)

}
trait Skill {
  def getExp: IO[SkillError, Experience]
  def addExperience: IO[SkillError, Unit]
  def getLevel: IO[SkillError, Level]
}

trait SkillError
object Skill {
  type Experience = Double
  type Level = Int

}
