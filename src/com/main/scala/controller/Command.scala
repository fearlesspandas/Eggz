package controller

import controller.Command.CommandError
import controller.Command.GenericCommandError
import entity.WorldBlock
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.RepairEgg
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.IO
import zio.ZIO
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait Command[-Env, +Out] extends Product with Serializable {
  def run: ZIO[Env, CommandError, Out]
}

object Command {
//  implicit val encoder: JsonEncoder[Command[Globz.Service with WorldBlock.Block, Any]] =
//    DeriveJsonEncoder.gen[Command[Globz.Service with WorldBlock.Block, Any]]
//  implicit val decoder: JsonDecoder[Command[Globz.Service with WorldBlock.Block, Any]] =
//    DeriveJsonDecoder.gen[Command[Globz.Service with WorldBlock.Block, Any]]
  implicit val encoder: JsonEncoder[Command[_, _]] =
    DeriveJsonEncoder.gen[Command[Nothing, Any]].contramap(x => x)
  implicit val decoder: JsonDecoder[Command[_, _]] =
    DeriveJsonDecoder.gen[Command[Nothing, Any]].map(x => x)
  type CommandKey = String

  def make[Env, Out](key: CommandKey): ZIO[Command.Service, CommandError, Command[Env, Out]] =
    ZIO.service[Command.Service].flatMap(_.make(key))

  trait Service {
    def make[Env, Out](key: CommandKey): IO[CommandError, Command[Env, Out]]
  }

  trait CommandError

  case class GenericCommandError(msg: String) extends CommandError

}

sealed trait SimpleCommand[-Env] extends Command[Env, Unit]
object SimpleCommand {
  implicit val encoder: JsonEncoder[SimpleCommand[_]] =
    DeriveJsonEncoder.gen[SimpleCommand[Nothing]].contramap(x => x)
  implicit val decoder: JsonDecoder[SimpleCommand[_]] =
    DeriveJsonDecoder.gen[SimpleCommand[Nothing]].map(x => x)
}
sealed trait Query[-Env, +A] extends Command[Env, A]
object Query {
  implicit val encoder: JsonEncoder[Query[_, _]] =
    DeriveJsonEncoder.gen[Query[Nothing, Any]].contramap(x => x)
  implicit val decoder: JsonDecoder[Query[_, _]] =
    DeriveJsonDecoder.gen[Query[Nothing, Any]].map(x => x)
}
case class CREATE_GLOB(globId: GLOBZ_ID, location: Vector[Double])
    extends SimpleCommand[Globz.Service with WorldBlock.Block] {
  override def run: ZIO[Globz.Service with WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- Globz.create(globId)
      _ <- WorldBlock.spawnBlob(glob, location)
    } yield ()).mapError(_ => GenericCommandError("error creating glob"))
}
object CREATE_GLOB {
  implicit val encoder: JsonEncoder[CREATE_GLOB] = DeriveJsonEncoder.gen[CREATE_GLOB]
  implicit val decoder: JsonDecoder[CREATE_GLOB] = DeriveJsonDecoder.gen[CREATE_GLOB]
}
case class GET_ALL_GLOBS() extends Query[WorldBlock.Block, Set[Globz.Glob]] {
  override def run: ZIO[WorldBlock.Block, CommandError, Set[Globz.Glob]] =
    (for {
      res <- WorldBlock.getAllBlobs()
    } yield res).mapError(_ => GenericCommandError("Error retrieving blobs"))
}
object GET_ALL_GLOBS {
  implicit val encoder: JsonEncoder[GET_ALL_GLOBS] = DeriveJsonEncoder.gen[GET_ALL_GLOBS]
  implicit val decoder: JsonDecoder[GET_ALL_GLOBS] = DeriveJsonDecoder.gen[GET_ALL_GLOBS]
}
case class CREATE_REPAIR_EGG(eggId: ID, globId: GLOBZ_ID) extends SimpleCommand[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      egg <- RepairEgg.make(eggId, 1000, 20)
      glob <- WorldBlock.getBlob(globId)
      res <- ZIO.fromOption(glob).flatMap(_.update(egg))
    } yield ()).mapError(_ => GenericCommandError("Error Creating egg"))
}
object CREATE_REPAIR_EGG {
  implicit val encoder: JsonEncoder[CREATE_REPAIR_EGG] = DeriveJsonEncoder.gen[CREATE_REPAIR_EGG]
  implicit val decoder: JsonDecoder[CREATE_REPAIR_EGG] = DeriveJsonDecoder.gen[CREATE_REPAIR_EGG]
}
//case class ADD_EGG(egg: Eggz.Service, glob: Globz.Glob) extends Command[Any, Unit] {
//  override def run: ZIO[Any, CommandError, Unit] =
//    (for {
//      _ <- glob.update(egg)
//    } yield ()).mapError(_ => GenericCommandError("error addng egg to glob"))
//}
//object ADD_EGG{
//  implicit val encoder: JsonEncoder[ADD_EGG] = DeriveJsonEncoder.gen[ADD_EGG]
//  implicit val decoder: JsonDecoder[ADD_EGG] = DeriveJsonDecoder.gen[ADD_EGG]
//}
case class GET_BLOB(id: GLOBZ_ID) extends Query[WorldBlock.Block, Option[Globz.Glob]] {
  override def run: ZIO[WorldBlock.Block, CommandError, Option[Globz.Glob]] =
    (for {
      g <- WorldBlock.getBlob(id)
    } yield g).mapError(_ => GenericCommandError(s"Error finding blob with $id"))
}
object GET_BLOB {
  implicit val encoder: JsonEncoder[GET_BLOB] = DeriveJsonEncoder.gen[GET_BLOB]
  implicit val decoder: JsonDecoder[GET_BLOB] = DeriveJsonDecoder.gen[GET_BLOB]
}
case class RELATE_EGGS(egg1: ID, egg2: ID, globId: GLOBZ_ID)
    extends Command[WorldBlock.Block, Unit] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      globOp <- WorldBlock.getBlob(globId)
      glob <- ZIO.fromOption(globOp)
      e1 <- glob.get(egg1).flatMap(ZIO.fromOption(_))
      e2 <- glob.get(egg2).flatMap(ZIO.fromOption(_))
      _ <- glob.relate(e1, e2)
    } yield ()).mapError(_ => GenericCommandError("Error relating eggz"))
}
object RELATE_EGGS {
  implicit val encoder: JsonEncoder[RELATE_EGGS] = DeriveJsonEncoder.gen[RELATE_EGGS]
  implicit val decoder: JsonDecoder[RELATE_EGGS] = DeriveJsonDecoder.gen[RELATE_EGGS]
}
case class TICK_WORLD() extends Command[WorldBlock.Block, Unit] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    WorldBlock.tickAllBlobs().mapError(_ => GenericCommandError("Error ticking world")).map(_ => ())
}
object TICK_WORLD {
  implicit val encoder: JsonEncoder[TICK_WORLD] = DeriveJsonEncoder.gen[TICK_WORLD]
  implicit val decoder: JsonDecoder[TICK_WORLD] = DeriveJsonDecoder.gen[TICK_WORLD]
}
case class START_EGG(eggid: ID, globId: GLOBZ_ID) extends Command[WorldBlock.Block, Unit] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
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
object START_EGG {
  implicit val encoder: JsonEncoder[START_EGG] = DeriveJsonEncoder.gen[START_EGG]
  implicit val decoder: JsonDecoder[START_EGG] = DeriveJsonDecoder.gen[START_EGG]
}
