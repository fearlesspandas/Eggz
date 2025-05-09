package entity
import zio.*
import controller.SerializableCommand.CommandError
import entity.Globz.GLOBZ_ID

trait BindableMovement {
  def bind_entities(
    binded: GLOBZ_ID,
    target: GLOBZ_ID
  ): ZIO[WorldBlock.Block, CommandError, Unit] =
    for {
      wb <- ZIO.service[WorldBlock.Block]
      b <- wb
        .getBlobOption(binded)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(NoEntityFoundBindableError)

    } yield ()

}
trait BindableMovementError extends CommandError
case object NoEntityFoundBindableError extends BindableMovementError
