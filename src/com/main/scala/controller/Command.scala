package controller

import controller.Command.CommandError
import controller.Command.GenericCommandError
import entity.WorldBlock
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.IO
import zio.ZIO
import zio.ZLayer

trait Command[Env, Out] {
  def run: ZIO[Env, CommandError, Out]
}

object Command {
  type CommandKey = String

  trait Service {
    def make[Env, Out](key: CommandKey): IO[CommandError, Command[Env, Out]]
  }
  def make[Env, Out](key: CommandKey): ZIO[Command.Service, CommandError, Command[Env, Out]] =
    ZIO.service[Command.Service].flatMap(_.make(key))
  trait CommandError
  case class GenericCommandError(msg: String) extends CommandError
}

case class CREATE_GLOB(globId: GLOBZ_ID, location: Vector[Double])
    extends Command[Globz.Service with WorldBlock.Service, Unit] {
  override def run: ZIO[Globz.Service with WorldBlock.Service, CommandError, Unit] =
    (for {
      glob <- Globz.create(globId)
      _ <- WorldBlock.spawnBlob(glob, location)
    } yield ()).mapError(_ => GenericCommandError("error creating glob"))
}

case class GET_ALL_GLOBS()
    extends Command[Globz.Service with WorldBlock.Service, Set[Globz.Service]] {
  override def run: ZIO[Globz.Service with WorldBlock.Service, CommandError, Set[Globz.Service]] =
    (for {
      res <- WorldBlock.getAllBlobs()
    } yield res).mapError(_ => GenericCommandError("Error retrieving blobs"))
}
