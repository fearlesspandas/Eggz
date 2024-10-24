package entity

import controller.Command
import controller.MultiResponse
import controller.ProgressUpdate
import controller.QueryResponse
import controller.QueuedClientMessage
import controller.QueuedServerMessage
import controller.SerializableCommand
import controller.SerializableCommand.CommandError
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.Chunk
import zio.UIO
import zio.ZIO
import zio.ZLayer
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

trait ProgressData {
  def handle(
    id: GLOBZ_ID,
    args: ProgressArgs
  ): ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    args match {
      case arg: TutorialArgs =>
        ZIO.serviceWithZIO[WorldBlock.Block] { wb =>
          Tutorial(id, args).run
            .provide(ZLayer.succeed(this) ++ ZLayer.succeed(wb))
        }
      case _ => ZIO.fail(ArgsNotRecognizedError)
    }

  def tutorialStage(id: GLOBZ_ID, stage: Int): ZIO[Any, ProgressError, Unit]
}
object ProgressData {
  def make: UIO[ProgressData] = for {
    _ <- ZIO.unit
  } yield BasicProgressData()
}
trait ProgressError extends CommandError
case object ArgsNotRecognizedError extends ProgressError

case class BasicProgressData() extends ProgressData {

  override def tutorialStage(
    id: GLOBZ_ID,
    stage: Int
  ): ZIO[Any, ProgressError, Unit] = ZIO.unit
}

trait Progress
    extends Command[WorldBlock.Block with ProgressData, QueryResponse]

case class Tutorial(id: GLOBZ_ID, args: ProgressArgs) extends Progress {
  override def run: ZIO[
    WorldBlock.Block with ProgressData,
    SerializableCommand.CommandError,
    QueryResponse
  ] =
    args match {
      case TutorialComplete(stage) =>
        ZIO
          .serviceWithZIO[ProgressData](_.tutorialStage(id, stage))
          .as(
            MultiResponse(
              Chunk(
                QueuedServerMessage(Chunk(ProgressUpdate(id, args))),
                QueuedClientMessage(id, Chunk(ProgressUpdate(id, args)))
              )
            )
          )
      case _ => ZIO.fail(ArgsNotRecognizedError)
    }
}
sealed trait ProgressArgs
object ProgressArgs {
  implicit val encoder: JsonEncoder[ProgressArgs] =
    DeriveJsonEncoder.gen[ProgressArgs]
  implicit val decoder: JsonDecoder[ProgressArgs] =
    DeriveJsonDecoder.gen[ProgressArgs]
}
sealed trait TutorialArgs extends ProgressArgs
//object TutorialArgs {
//  implicit val encoder: JsonEncoder[TutorialArgs] =
//    DeriveJsonEncoder.gen[TutorialArgs]
//  implicit val decoder: JsonDecoder[TutorialArgs] =
//    DeriveJsonDecoder.gen[TutorialArgs]
//}
case class TutorialComplete(stage: Int) extends TutorialArgs
