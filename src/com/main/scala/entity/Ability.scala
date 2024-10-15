package entity

import controller.Command
import controller.DoAbility
import controller.MultiResponse
import controller.QueryResponse
import controller.QueuedClientBroadcast
import controller.QueuedClientMessage
import controller.QueuedServerMessage
import controller.SerializableCommand
import entity.Ability.ABILITY_ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.*
import zio.json._

trait Ability extends Command[WorldBlock.Block, QueryResponse] {
  val id: ABILITY_ID
}
object Ability {
  type ABILITY_ID = Int
  private val ability_config = Chunk(Smack)
  def make(
    id: ABILITY_ID,
    from: GLOBZ_ID,
    args: AbilityArgs = NoArgs
  ): IO[AbilityError, Ability] =
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
              QueuedClientBroadcast(Chunk(DoAbility(from, 0)))
            )
          )
        )
    } yield res
}

case class GlobularTeleport(
  from: GLOBZ_ID,
  points: Shape
) extends Ability {
  override val id: ABILITY_ID = 0
  override def run
    : ZIO[WorldBlock.Block, SerializableCommand.CommandError, QueryResponse] =
    for {
      res <- ZIO
        .succeed(
          MultiResponse(
            Chunk(
              QueuedServerMessage(Chunk(DoAbility(from, 0, points))),
              QueuedClientBroadcast(Chunk(DoAbility(from, 0, points)))
            )
          )
        )
    } yield res
}
sealed trait AbilityArgs
object AbilityArgs {
  implicit val encoder: JsonEncoder[AbilityArgs] =
    DeriveJsonEncoder.gen[AbilityArgs]
  implicit val decoder: JsonDecoder[AbilityArgs] =
    DeriveJsonDecoder.gen[AbilityArgs]
}

case object NoArgs extends AbilityArgs

case class Shape(points: Set[(Double, Double, Double)]) extends AbilityArgs
