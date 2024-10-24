package entity

import controller.SerializableCommand.CommandError
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.ZIO

trait BindableMovement {
  def bind_entities(
    binded: GLOBZ_ID,
    target: GLOBZ_ID
  ): ZIO[WorldBlock.Block, CommandError, Unit] =
    for {
      wb <- ZIO.service[WorldBlock.Block]
      b <- wb
        .getBlob(binded)
        .flatMap(ZIO.fromOption(_))
        .orElseFail(NoEntityFoundBindableError)

    } yield ()

}
trait BindableMovementError extends CommandError
case object NoEntityFoundBindableError extends BindableMovementError
