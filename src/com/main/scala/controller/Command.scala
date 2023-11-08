package controller

import controller.Command.CommandError
import controller.Command.GenericCommandError
import entity.WorldBlock
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.EggzOps
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.ExitCode
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

case class GET_ALL_GLOBS() extends Command[WorldBlock.Service, Set[Globz.Glob]] {
  override def run: ZIO[WorldBlock.Service, CommandError, Set[Globz.Glob]] =
    (for {
      res <- WorldBlock.getAllBlobs()
    } yield res).mapError(_ => GenericCommandError("Error retrieving blobs"))
}

case class CREATE_EGG(eggid: Eggz.Service, globz: Globz.Glob) extends Command[Any, Unit] {
  override def run: ZIO[Any, CommandError, Unit] =
    (for {
      res <- globz.update(eggid)
    } yield ()).mapError(_ => GenericCommandError("Error Creating egg"))
}

case class ADD_EGG(egg: Eggz.Service, glob: Globz.Glob) extends Command[Any, Unit] {
  override def run: ZIO[Any, CommandError, Unit] =
    (for {
      _ <- glob.update(egg)
    } yield ()).mapError(_ => GenericCommandError("error addng egg to glob"))
}
case class GET_BLOB(id: GLOBZ_ID) extends Command[WorldBlock.Service, Option[Globz.Glob]] {
  override def run: ZIO[WorldBlock.Service, CommandError, Option[Globz.Glob]] =
    (for {
      g <- WorldBlock.getBlob(id)
    } yield g).mapError(_ => GenericCommandError(s"Error finding blob with $id"))
}

case class TICK_WORLD() extends Command[WorldBlock.Service, Unit] {
  override def run: ZIO[WorldBlock.Service, CommandError, Unit] =
    WorldBlock.tickAllBlobs().mapError(_ => GenericCommandError("Error ticking world")).map(_ => ())
}

case class START_EGG(eggid: ID, globId: GLOBZ_ID) extends Command[WorldBlock.Service, Unit] {
  override def run: ZIO[WorldBlock.Service, CommandError, Unit] =
    (for {
      g <- WorldBlock.getBlob(globId)
      _ <- ZIO
        .fromOption(g)
        .flatMap(glob =>
          for {
            egg <- glob.get(eggid)
            _ <- ZIO.fromOption(egg).flatMap(glob.scheduleEgg(_))
          } yield ()
        )

    } yield ()).mapError(_ => GenericCommandError("error starting egg"))
}
