package controller

import controller.Command.CMD
import controller.SerializableCommand.CommandError
import controller.SerializableCommand.GenericCommandError
import controller.GET_NEXT_DESTINATION.ADDED_DESTINATION
import controller.SUBSCRIBE.SubscriptionEnv
import entity.BasicPlayer
import entity.EggzModel
import entity.GlobInMemory
import entity.GlobzInMem
import entity.GlobzModel
import entity.PLAYER_EGG
import entity.PhysicalEntity
import entity.Player
import entity.PlayerGlob
import entity.WorldBlock
import physics.Destinations
import physics.Vector3
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.RepairEgg
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.Duration.fromMillis
import zio.IO
import zio.Schedule
import zio.ZIO
import zio.http.ChannelEvent.Read
import zio.http._
import zio.http.WebSocketFrame
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.EncoderOps
import zio.json.JsonDecoder
import zio.json.JsonEncoder

import java.time.Duration

sealed trait SerializableCommand[-Env, +Out] extends Command[Env, Out]

object SerializableCommand {
  //we could implement type specific serialization to map onto what commands have been concretely defined
  //however, there is a natural serialization available regardless of type param
//  implicit val encoder: JsonEncoder[Command[Globz.Service with WorldBlock.Block, Any]] =
//    DeriveJsonEncoder.gen[Command[Globz.Service with WorldBlock.Block, Any]]
//  implicit val decoder: JsonDecoder[Command[Globz.Service with WorldBlock.Block, Any]] =
//    DeriveJsonDecoder.gen[Command[Globz.Service with WorldBlock.Block, Any]]
  implicit val encoder: JsonEncoder[SerializableCommand[_, _]] =
    DeriveJsonEncoder.gen[SerializableCommand[Nothing, Any]].contramap(x => x)
  implicit val decoder: JsonDecoder[SerializableCommand[_, _]] =
    DeriveJsonDecoder.gen[SerializableCommand[Nothing, Any]].map(x => x)
  type CommandKey = String

  def make[Env, Out](
    key: CommandKey
  ): ZIO[SerializableCommand.Service, CommandError, SerializableCommand[Env, Out]] =
    ZIO.service[SerializableCommand.Service].flatMap(_.make(key))

  trait Service {
    def make[Env, Out](key: CommandKey): IO[CommandError, SerializableCommand[Env, Out]]
  }

  trait CommandError

  case class GenericCommandError(msg: String) extends CommandError

}
trait SimpleCommand[-Env] extends Command[Env, Unit]
sealed trait SimpleCommandSerializable[-Env] extends SerializableCommand[Env, Unit]
object SimpleCommandSerializable {
  implicit val encoder: JsonEncoder[SimpleCommandSerializable[_]] =
    DeriveJsonEncoder.gen[SimpleCommandSerializable[Nothing]].contramap(x => x)
  implicit val decoder: JsonDecoder[SimpleCommandSerializable[_]] =
    DeriveJsonDecoder.gen[SimpleCommandSerializable[Nothing]].map(x => x)
}
//todo potentially i should rework this to only output a specific QUERY_RESPONSE type
// that way serialization is handled separately from the query completely
// without contaminating the type heirarchy logic
// Ideally queries as a type can be kept general in output
// but we can defer our implementation to something with an assumed output standard
sealed trait Query[-Env, +A] extends SerializableCommand[Env, A] {}
object Query {
  implicit val encoder: JsonEncoder[Query[_, _]] =
    DeriveJsonEncoder.gen[Query[Nothing, Any]].contramap(x => x)
  implicit val decoder: JsonDecoder[Query[_, _]] =
    DeriveJsonDecoder.gen[Query[Nothing, Any]].map(x => x)
}

sealed trait ResponseQuery[-Env] extends Query[Env, QueryResponse]
object ResponseQuery {
  implicit val encoder: JsonEncoder[ResponseQuery[_]] =
    DeriveJsonEncoder.gen[ResponseQuery[Nothing]].contramap(x => x)
  implicit val decoder: JsonDecoder[ResponseQuery[_]] =
    DeriveJsonDecoder.gen[ResponseQuery[Nothing]].map(x => x)
}

//object RUN_Serializable_COMMAND_ASYNC {
//  implicit val encoder: JsonEncoder[RUN_Serializable_COMMAND_ASYNC[_, _]] =
//    DeriveJsonEncoder.gen[RUN_Serializable_COMMAND_ASYNC[Nothing, Any]] //.contramap(x => x)
//  implicit val decoder: JsonDecoder[RUN_Serializable_COMMAND_ASYNC[_, _]] =
//    DeriveJsonDecoder.gen[RUN_Serializable_COMMAND_ASYNC[Nothing, Any]] //.map(x => x)
//}

sealed trait Subscription[Env] extends SerializableCommand[Env, Unit] {
  val query: ResponseQuery[Env]

  override def run: ZIO[Env, CommandError, Unit] = ???
}
object Subscription {
  implicit val encoder: JsonEncoder[Subscription[SubscriptionEnv]] =
    DeriveJsonEncoder.gen[Subscription[SubscriptionEnv]].contramap(x => x)
  implicit val decoder: JsonDecoder[Subscription[SubscriptionEnv]] =
    DeriveJsonDecoder.gen[Subscription[SubscriptionEnv]].map(x => x)
}
case class SUBSCRIBE(query: ResponseQuery[SubscriptionEnv]) extends Subscription[SubscriptionEnv]

case class SocketSubscribe(socket: WebSocketChannel, sub: SUBSCRIBE)
    extends Command[SubscriptionEnv, Unit] {
  override def run: ZIO[SubscriptionEnv, CommandError, Unit] =
    RUN_COMMAND_ASYNC(
      sub.query.flatMap(response =>
        Command.fromZIO(socket.send(Read(WebSocketFrame.text(response.toJson))))
      ),
      10
    ).run
}
object SUBSCRIBE {
  type SubscriptionEnv = Globz.Service with WorldBlock.Block
  implicit val en: JsonEncoder[ResponseQuery[SubscriptionEnv]] =
    DeriveJsonEncoder.gen[ResponseQuery[_]].contramap(x => x)
  implicit val de: JsonDecoder[ResponseQuery[SubscriptionEnv]] =
    DeriveJsonDecoder.gen[ResponseQuery[SubscriptionEnv]] //.map(x => x)
  implicit val encoder: JsonEncoder[SUBSCRIBE] = DeriveJsonEncoder.gen[SUBSCRIBE]
  implicit val decoder: JsonDecoder[SUBSCRIBE] = DeriveJsonDecoder.gen[SUBSCRIBE]
}
case class CREATE_GLOB(globId: GLOBZ_ID, location: Vector[Double])
    extends SimpleCommandSerializable[Globz.Service with WorldBlock.Block] {
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
case class GET_ALL_GLOBS() extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      models <- ZIO.collectAllPar(res.map(_.serializeGlob))
    } yield GlobSet(models.collect { case g: GlobzModel => g })).mapError(_ =>
      GenericCommandError("Error retrieving blobs")
    )

}
object GET_ALL_GLOBS {
  implicit val encoder: JsonEncoder[GET_ALL_GLOBS] = DeriveJsonEncoder.gen[GET_ALL_GLOBS]
  implicit val decoder: JsonDecoder[GET_ALL_GLOBS] = DeriveJsonDecoder.gen[GET_ALL_GLOBS]
}
case class GET_ALL_ENTITY_IDS() extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
    } yield EntityIDSet(res.map(_.id))).mapError(_ => GenericCommandError("Error retrieving blobs"))
}
object GET_ALL_ENTITY_IDS {
  implicit val encoder: JsonEncoder[GET_ALL_ENTITY_IDS] = DeriveJsonEncoder.gen[GET_ALL_ENTITY_IDS]
  implicit val decoder: JsonDecoder[GET_ALL_ENTITY_IDS] = DeriveJsonDecoder.gen[GET_ALL_ENTITY_IDS]
}
case class GET_ALL_EGGZ() extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      nested <- ZIO.collectAllPar(res.map(_.getAll())).map(d => d.flatMap(x => x))
      stats <- ZIO.collectAllPar(
        nested.map(egg => egg.serializeEgg)
      )
    } yield EggSet(stats)).mapError(_ => GenericCommandError("Error retrieving blobs"))
}
object GET_ALL_EGGZ {
  implicit val encoder: JsonEncoder[GET_ALL_EGGZ] = DeriveJsonEncoder.gen[GET_ALL_EGGZ]
  implicit val decoder: JsonDecoder[GET_ALL_EGGZ] = DeriveJsonDecoder.gen[GET_ALL_EGGZ]
}
case class GET_ALL_STATS() extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      nested <- ZIO.collectAllPar(res.map(_.getAll())).map(d => d.flatMap(x => x))
      s <- ZIO.collectAllPar(
        nested.map(x =>
          for {
            health <- x.health()
            energy <- x.energy()
          } yield Stats(x.id, health, energy)
        )
      )
    } yield AllStats(s)).mapError(_ => GenericCommandError("Error retrieving blobs"))
}
object GET_ALL_STATS {
  implicit val encoder: JsonEncoder[GET_ALL_STATS] = DeriveJsonEncoder.gen[GET_ALL_STATS]
  implicit val decoder: JsonDecoder[GET_ALL_STATS] = DeriveJsonDecoder.gen[GET_ALL_STATS]
}
case class CREATE_REPAIR_EGG(eggId: ID, globId: GLOBZ_ID)
    extends SimpleCommandSerializable[WorldBlock.Block] {
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
case class GET_BLOB(id: GLOBZ_ID) extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      g <- WorldBlock.getBlob(id)
      x <- ZIO.fromOption(g).flatMap(glob => glob.serializeGlob)
    } yield Blob(Some(x)))
      .mapError(_ => GenericCommandError(s"Error finding blob with $id"))
      .fold(err => Blob(None), x => x)
}
object GET_BLOB {
  implicit val encoder: JsonEncoder[GET_BLOB] = DeriveJsonEncoder.gen[GET_BLOB]
  implicit val decoder: JsonDecoder[GET_BLOB] = DeriveJsonDecoder.gen[GET_BLOB]
}

case class GET_GLOB_LOCATION(id: GLOBZ_ID) extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock.getBlob(id)
      location <- ZIO.fromOption(glob).flatMap { case g: PhysicalEntity => g.getLocation }
    } yield Location(id, (location(0), location(1), location(2)))).fold(_ => NoLocation(id), x => x)
}
object GET_GLOB_LOCATION {
  implicit val encoder: JsonEncoder[GET_GLOB_LOCATION] = DeriveJsonEncoder.gen[GET_GLOB_LOCATION]
  implicit val decoder: JsonDecoder[GET_GLOB_LOCATION] = DeriveJsonDecoder.gen[GET_GLOB_LOCATION]
}
case class GET_GLOB_LOCATION_PROCESS(id: GLOBZ_ID) extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock.getBlob(id)
      location <- ZIO.fromOption(glob).flatMap { case g: PhysicalEntity => g.getLocation }
    } yield Location(id, (location(0), location(1), location(2)))).fold(_ => NoLocation(id), x => x)
}
object GET_GLOB_LOCATION_PROCESS {
  implicit val encoder: JsonEncoder[GET_GLOB_LOCATION_PROCESS] =
    DeriveJsonEncoder.gen[GET_GLOB_LOCATION_PROCESS]
  implicit val decoder: JsonDecoder[GET_GLOB_LOCATION_PROCESS] =
    DeriveJsonDecoder.gen[GET_GLOB_LOCATION_PROCESS]
}
case class SET_GLOB_LOCATION(id: GLOBZ_ID, location: Vector[Double])
    extends SimpleCommandSerializable[Globz.Service with WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- WorldBlock.getBlob(id)
      _ <- ZIO.fromOption(glob).flatMap { case pe: PhysicalEntity => pe.teleport(location) }
    } yield ())
      .mapError(_ => GenericCommandError("Error setting glob location"))

}
object SET_GLOB_LOCATION {
  implicit val encoder: JsonEncoder[SET_GLOB_LOCATION] = DeriveJsonEncoder.gen[SET_GLOB_LOCATION]
  implicit val decoder: JsonDecoder[SET_GLOB_LOCATION] = DeriveJsonDecoder.gen[SET_GLOB_LOCATION]
}
case class RELATE_EGGS(egg1: ID, egg2: ID, globId: GLOBZ_ID, bidirectional: Boolean)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      globOp <- WorldBlock.getBlob(globId)
      glob <- ZIO.fromOption(globOp)
      e1 <- glob.get(egg1).flatMap(ZIO.fromOption(_))
      e2 <- glob.get(egg2).flatMap(ZIO.fromOption(_))
      _ <- glob.relate(e1, e2, bidirectional)
    } yield ()).mapError(_ => GenericCommandError("Error relating eggz"))
}
object RELATE_EGGS {
  implicit val encoder: JsonEncoder[RELATE_EGGS] = DeriveJsonEncoder.gen[RELATE_EGGS]
  implicit val decoder: JsonDecoder[RELATE_EGGS] = DeriveJsonDecoder.gen[RELATE_EGGS]
}
case class UNRELATE_EGGS(egg1: ID, egg2: ID, globId: GLOBZ_ID, bidirectional: Boolean)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      globOp <- WorldBlock.getBlob(globId)
      glob <- ZIO.fromOption(globOp)
      e1 <- glob.get(egg1).flatMap(ZIO.fromOption(_))
      e2 <- glob.get(egg2).flatMap(ZIO.fromOption(_))
      _ <- glob.unrelate(e1, e2, bidirectional)
    } yield ()).mapError(_ => GenericCommandError("Error relating eggz"))
}
object UNRELATE_EGGS {
  implicit val encoder: JsonEncoder[UNRELATE_EGGS] = DeriveJsonEncoder.gen[UNRELATE_EGGS]
  implicit val decoder: JsonDecoder[UNRELATE_EGGS] = DeriveJsonDecoder.gen[UNRELATE_EGGS]
}

case class UNRELATE_ALL(egg1: ID, globId: GLOBZ_ID, direction: Int)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      globOp <- WorldBlock.getBlob(globId)
      glob <- ZIO.fromOption(globOp)
      e1 <- glob.get(egg1).flatMap(ZIO.fromOption(_))
      _ <- glob.unrelateAll(e1, direction)
    } yield ()).mapError(_ => GenericCommandError("Error relating eggz"))
}
object UNRELATE_ALL {
  implicit val encoder: JsonEncoder[UNRELATE_ALL] = DeriveJsonEncoder.gen[UNRELATE_ALL]
  implicit val decoder: JsonDecoder[UNRELATE_ALL] = DeriveJsonDecoder.gen[UNRELATE_ALL]
}
case class TICK_WORLD() extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    WorldBlock.tickAllBlobs().mapError(_ => GenericCommandError("Error ticking world")).map(_ => ())
}
object TICK_WORLD {
  implicit val encoder: JsonEncoder[TICK_WORLD] = DeriveJsonEncoder.gen[TICK_WORLD]
  implicit val decoder: JsonDecoder[TICK_WORLD] = DeriveJsonDecoder.gen[TICK_WORLD]
}
case class START_EGG(eggId: ID, globId: GLOBZ_ID)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      g <- WorldBlock.getBlob(globId)
      _ <- ZIO
        .fromOption(g)
        .flatMap(glob =>
          for {
            egg <- glob.get(eggId)
            _ <- ZIO.fromOption(egg).flatMap(glob.scheduleEgg(_))
          } yield ()
        )

    } yield ()).mapError(_ => GenericCommandError("error starting egg"))
}
object START_EGG {
  implicit val encoder: JsonEncoder[START_EGG] = DeriveJsonEncoder.gen[START_EGG]
  implicit val decoder: JsonDecoder[START_EGG] = DeriveJsonDecoder.gen[START_EGG]
}

case class ADD_DESTINATION(id: ID, location: Vector[Double])
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      blob <- WorldBlock.getBlob(id)
      _ <- ZIO
        .fromOption(blob)
        .flatMap({
          case entity: Destinations =>
            entity.addDestination(location)
        })
    } yield ()).mapError(_ => GenericCommandError(s"Error adding destination to entity $id"))
}
object ADD_DESTINATION {
  implicit val encoder: JsonEncoder[ADD_DESTINATION] = DeriveJsonEncoder
    .gen[ADD_DESTINATION]

  implicit val decoder: JsonDecoder[ADD_DESTINATION] =
    DeriveJsonDecoder.gen[ADD_DESTINATION]

}

case class GET_NEXT_DESTINATION(id: ID) extends ResponseQuery[Globz.Service with WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id)
      location <- ZIO
        .fromOption(blob)
        .flatMap { case entity: Destinations => entity.getNextDestination() }
        .mapError(_ => ???)
      loc <- ZIO
        .fromOption(location)
        .flatMap(vec => ZIO.succeed(vec(0)).zip(ZIO.succeed(vec(1))).zip(ZIO.succeed(vec(2))))
    } yield NextDestination(id, loc))
      .mapError(_ => GenericCommandError(s"Error retrieving destination for id $id"))
      .fold(err => NoLocation(id), x => x)
}

object GET_NEXT_DESTINATION {
  implicit val encoder: JsonEncoder[GET_NEXT_DESTINATION] =
    DeriveJsonEncoder.gen[GET_NEXT_DESTINATION]
  implicit val decoder: JsonDecoder[GET_NEXT_DESTINATION] =
    DeriveJsonDecoder.gen[GET_NEXT_DESTINATION]

  case class ADDED_DESTINATION(id: ID, location: String)
}

case class GET_ALL_DESTINATIONS(id: ID) extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id)
      location <- ZIO
        .fromOption(blob)
        .flatMap { case entity: Destinations => entity.getAllDestinations() }
        .mapError(_ => ???)
      loc = location.map(vec => (vec(0), vec(1), vec(2)))
    } yield AllDestinations(id, loc)).mapError(_ =>
      GenericCommandError(s"Error retrieving destination for id $id")
    )
}

object GET_ALL_DESTINATIONS {
  implicit val encoder: JsonEncoder[GET_ALL_DESTINATIONS] =
    DeriveJsonEncoder.gen[GET_ALL_DESTINATIONS]
  implicit val decoder: JsonDecoder[GET_ALL_DESTINATIONS] =
    DeriveJsonDecoder.gen[GET_ALL_DESTINATIONS]

  case class ADDED_DESTINATION(id: ID, location: String)
}
