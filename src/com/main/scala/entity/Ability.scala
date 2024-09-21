package entity

import controller.Command
import controller.DoAbility
import controller.MultiResponse
import controller.QueryResponse
import controller.QueuedClientMessage
import controller.QueuedServerMessage
import controller.SerializableCommand
import entity.Ability.ABILITY_ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.*

trait Ability extends Command[WorldBlock.Block, QueryResponse] {
  val id: ABILITY_ID
}
object Ability {
  type ABILITY_ID = Int
  private val ability_config = Chunk(Smack)
  def make(id: ABILITY_ID, from: GLOBZ_ID): IO[AbilityError, Ability] =
    id match {
      case 0 => ZIO.succeed(Smack(from))
      case _ => ZIO.fail(AbilityDoesNotExistError)
    }
}
trait AbilityError
case class SmackAbilityError(msg: String) extends AbilityError
case object AbilityRequirementsNotMetError extends AbilityError
case object AbilityDoesNotExistError extends AbilityError
case class Smack(from: GLOBZ_ID) extends Ability {
  override val id: ABILITY_ID = 0

  override def run
    : ZIO[WorldBlock.Block, SerializableCommand.CommandError, QueryResponse] =
    for {
      res <- ZIO
        .succeed(
          MultiResponse(
            Chunk(
              QueuedServerMessage(Chunk(DoAbility(from, 0))),
              QueuedClientMessage(from, Chunk(DoAbility(from, 0)))
            )
          )
        )
    } yield res
}
