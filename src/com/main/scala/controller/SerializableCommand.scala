package controller

import controller.CONSOLE.CONSOLE_ENV
import controller.SUBSCRIBE.SubscriptionEnv
import controller.SerializableCommand.CommandError
import controller.SerializableCommand.GenericCommandError
import entity.GlobzModel
import entity.LivingEntity
import entity.PhysicalEntity
import entity.Player
import entity.TerrainChunkM
import entity.TerrainModel
import entity.TerrainRegion
import entity.TerrainRegionM
import entity.TerrainUnit
import entity.WorldBlock
import physics.DESTINATION_TYPE.GRAVITY
import physics.DESTINATION_TYPE.TELEPORT
import physics.DESTINATION_TYPE.WAYPOINT
import physics.Destination
import physics.DestinationModel
import physics.Destinations
import physics.Mode
import physics.SetInputLock
import physics.destination
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.RepairEgg
import zio.Chunk
import zio.ZIO
import zio.ZLayer
import zio.http.ChannelEvent.Read
import zio.http.WebSocketFrame
import zio.http.*
import zio.json.DecoderOps
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.EncoderOps
import zio.json.JsonDecoder
import zio.json.JsonEncoder

import java.util.UUID

sealed trait SerializableCommand[-Env, +Out] extends Command[Env, Out] {
  val REF_TYPE: Any
}
object SerializableCommand {
  implicit val encoder: JsonEncoder[SerializableCommand[_, _]] =
    DeriveJsonEncoder.gen[SerializableCommand[Nothing, Any]].contramap(x => x)
  implicit val decoder: JsonDecoder[SerializableCommand[_, _]] =
    DeriveJsonDecoder.gen[SerializableCommand[Nothing, Any]].map(x => x)
  trait CommandError
  case class GenericCommandError(msg: String) extends CommandError

}
sealed trait SimpleCommandSerializable[-Env]
    extends SerializableCommand[Env, Unit] {}
object SimpleCommandSerializable {
  implicit val encoder: JsonEncoder[SimpleCommandSerializable[_]] =
    DeriveJsonEncoder.gen[SimpleCommandSerializable[Nothing]].contramap(x => x)
  implicit val decoder: JsonDecoder[SimpleCommandSerializable[_]] =
    DeriveJsonDecoder.gen[SimpleCommandSerializable[Nothing]].map(x => x)
}
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

case class SUBSCRIBE(query: ResponseQuery[SubscriptionEnv])
    extends Subscription[SubscriptionEnv] {
  override val REF_TYPE: Any = query.REF_TYPE
}
object SUBSCRIBE {
  type SubscriptionEnv = Globz.Service with WorldBlock.Block
  implicit val en: JsonEncoder[ResponseQuery[SubscriptionEnv]] =
    DeriveJsonEncoder.gen[ResponseQuery[_]].contramap(x => x)
  implicit val de: JsonDecoder[ResponseQuery[SubscriptionEnv]] =
    DeriveJsonDecoder.gen[ResponseQuery[SubscriptionEnv]] // .map(x => x)
  implicit val encoder: JsonEncoder[SUBSCRIBE] =
    DeriveJsonEncoder.gen[SUBSCRIBE]
  implicit val decoder: JsonDecoder[SUBSCRIBE] =
    DeriveJsonDecoder.gen[SUBSCRIBE]
}

case class UNSUBSCRIBE_ALL()
object UNSUBSCRIBE_ALL {
  type SubscriptionEnv = Globz.Service with WorldBlock.Block
  implicit val en: JsonEncoder[ResponseQuery[SubscriptionEnv]] =
    DeriveJsonEncoder.gen[ResponseQuery[_]].contramap(x => x)
  implicit val de: JsonDecoder[ResponseQuery[SubscriptionEnv]] =
    DeriveJsonDecoder.gen[ResponseQuery[SubscriptionEnv]] // .map(x => x)
  implicit val encoder: JsonEncoder[UNSUBSCRIBE_ALL] =
    DeriveJsonEncoder.gen[UNSUBSCRIBE_ALL]
  implicit val decoder: JsonDecoder[UNSUBSCRIBE_ALL] =
    DeriveJsonDecoder.gen[UNSUBSCRIBE_ALL]
}

case class SocketSubscribe(socket: WebSocketChannel, sub: SUBSCRIBE)
    extends Command[SubscriptionEnv, Unit] {
  override def run: ZIO[SubscriptionEnv, CommandError, Unit] =
    RUN_COMMAND_ASYNC(
      sub.query.flatMap(response =>
        Command.fromZIO(socket.send(Read(WebSocketFrame.text(response.toJson))))
      ),
      30
    ).run
}

case class CREATE_GLOB(globId: GLOBZ_ID, location: Vector[Double])
    extends SimpleCommandSerializable[Globz.Service with WorldBlock.Block] {
  val REF_TYPE: Any = CREATE_GLOB
  override def run
    : ZIO[Globz.Service with WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- Globz.create(globId)
      _ <- WorldBlock.spawnBlob(glob, location)
      _ <- glob match {
        case pe: PhysicalEntity => pe.teleport(location); case _ => ZIO.unit
      }
    } yield ()).mapError(_ => GenericCommandError("error creating glob"))
}
object CREATE_GLOB {
  implicit val encoder: JsonEncoder[CREATE_GLOB] =
    DeriveJsonEncoder.gen[CREATE_GLOB]
  implicit val decoder: JsonDecoder[CREATE_GLOB] =
    DeriveJsonDecoder.gen[CREATE_GLOB]
}

case class GET_ALL_GLOBS() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_ALL_GLOBS
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      models <- ZIO.collectAllPar(res.map(_.serializeGlob))
      ser_mods = Chunk.from(
        models
          .collect { case g: GlobzModel => g }
          .grouped(5)
          .map(globs => GlobSet(globs))
      )
    } yield PaginatedResponse(ser_mods)).mapError(_ =>
      GenericCommandError("Error retrieving blobs")
    )
}
object GET_ALL_GLOBS {
  implicit val encoder: JsonEncoder[GET_ALL_GLOBS] =
    DeriveJsonEncoder.gen[GET_ALL_GLOBS]
  implicit val decoder: JsonDecoder[GET_ALL_GLOBS] =
    DeriveJsonDecoder.gen[GET_ALL_GLOBS]
}

case class GET_ALL_ENTITY_IDS() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_ALL_ENTITY_IDS
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
    } yield EntityIDSet(res.map(_.id))).mapError(_ =>
      GenericCommandError("Error retrieving blobs")
    )
}
object GET_ALL_ENTITY_IDS {
  implicit val encoder: JsonEncoder[GET_ALL_ENTITY_IDS] =
    DeriveJsonEncoder.gen[GET_ALL_ENTITY_IDS]
  implicit val decoder: JsonDecoder[GET_ALL_ENTITY_IDS] =
    DeriveJsonDecoder.gen[GET_ALL_ENTITY_IDS]
}

case class GET_ALL_EGGZ() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_ALL_EGGZ
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      nested <- ZIO
        .collectAllPar(res.map(_.getAll()))
        .map(d => d.flatMap(x => x))
      stats <- ZIO.collectAllPar(
        nested.map(egg => egg.serializeEgg)
      )
    } yield EggSet(stats)).mapError(_ =>
      GenericCommandError("Error retrieving blobs")
    )
}
object GET_ALL_EGGZ {
  implicit val encoder: JsonEncoder[GET_ALL_EGGZ] =
    DeriveJsonEncoder.gen[GET_ALL_EGGZ]
  implicit val decoder: JsonDecoder[GET_ALL_EGGZ] =
    DeriveJsonDecoder.gen[GET_ALL_EGGZ]
}

case class GET_ALL_STATS() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_ALL_STATS
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      nested <- ZIO
        .collectAllPar(res.map(_.getAll()))
        .map(d => d.flatMap(x => x))
      s <- ZIO.collectAllPar(
        nested.map(x =>
          for {
            health <- x.health
            energy <- x.energy
          } yield Stats(x.id, health, energy)
        )
      )
    } yield AllStats(s)).mapError(_ =>
      GenericCommandError("Error retrieving blobs")
    )
}
object GET_ALL_STATS {
  implicit val encoder: JsonEncoder[GET_ALL_STATS] =
    DeriveJsonEncoder.gen[GET_ALL_STATS]
  implicit val decoder: JsonDecoder[GET_ALL_STATS] =
    DeriveJsonDecoder.gen[GET_ALL_STATS]
}

case class ADD_HEALTH(id: GLOBZ_ID, value: Double)
    extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = (ADD_HEALTH, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      glob <- ZIO
        .service[WorldBlock.Block]
        .flatMap(_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .map { case li: LivingEntity => li }
        .mapError(_ => GenericCommandError(""))
      _ <- glob.health
        .flatMap(h => glob.setHealth(h + value))
        .mapError(_ => GenericCommandError(""))
      h <- glob.health.mapError(_ => GenericCommandError(""))
    } yield MultiResponse(
      Chunk(
        HealthSet(id, h),
        QueuedClientMessage(id, Chunk(MSG(id, HealthSet(id, h))))
      )
    )
object ADD_HEALTH {
  implicit val encoder: JsonEncoder[ADD_HEALTH] =
    DeriveJsonEncoder.gen[ADD_HEALTH]
  implicit val decoder: JsonDecoder[ADD_HEALTH] =
    DeriveJsonDecoder.gen[ADD_HEALTH]
}

case class REMOVE_HEALTH(id: GLOBZ_ID, value: Double)
    extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = (REMOVE_HEALTH, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      glob <- ZIO
        .service[WorldBlock.Block]
        .flatMap(_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .map { case li: LivingEntity => li }
        .mapError(_ => GenericCommandError(""))
      _ <- glob.health
        .flatMap(h => glob.setHealth(h - value))
        .mapError(_ => GenericCommandError(""))
      h <- glob.health.mapError(_ => GenericCommandError(""))
    } yield MultiResponse(
      Chunk(
        HealthSet(id, h),
        QueuedClientMessage(id, Chunk(MSG(id, HealthSet(id, h))))
      )
    )
object REMOVE_HEALTH {
  implicit val encoder: JsonEncoder[REMOVE_HEALTH] =
    DeriveJsonEncoder.gen[REMOVE_HEALTH]
  implicit val decoder: JsonDecoder[REMOVE_HEALTH] =
    DeriveJsonDecoder.gen[REMOVE_HEALTH]
}

case class CREATE_REPAIR_EGG(eggId: ID, globId: GLOBZ_ID)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = (CREATE_REPAIR_EGG, globId)
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      egg <- RepairEgg.make(eggId, 1000, 20)
      glob <- WorldBlock.getBlob(globId)
      res <- ZIO.fromOption(glob).flatMap(_.update(egg))
    } yield ()).mapError(_ => GenericCommandError("Error Creating egg"))
}
object CREATE_REPAIR_EGG {
  implicit val encoder: JsonEncoder[CREATE_REPAIR_EGG] =
    DeriveJsonEncoder.gen[CREATE_REPAIR_EGG]
  implicit val decoder: JsonDecoder[CREATE_REPAIR_EGG] =
    DeriveJsonDecoder.gen[CREATE_REPAIR_EGG]
}

case class GET_BLOB(id: GLOBZ_ID) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (GET_BLOB, id)
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

@deprecated
case class GET_GLOB_LOCATION(id: GLOBZ_ID)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (GET_GLOB_LOCATION, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock.getBlob(id)
      location <- ZIO.fromOption(glob).flatMap { case g: PhysicalEntity =>
        g.getLocation
      }
    } yield MSG(id, Location(id, (location(0), location(1), location(2)))))
      .fold(_ => NoLocation(id), x => x)
}
object GET_GLOB_LOCATION {
  implicit val encoder: JsonEncoder[GET_GLOB_LOCATION] =
    DeriveJsonEncoder.gen[GET_GLOB_LOCATION]
  implicit val decoder: JsonDecoder[GET_GLOB_LOCATION] =
    DeriveJsonDecoder.gen[GET_GLOB_LOCATION]
}

@deprecated
case class SET_GLOB_LOCATION(id: GLOBZ_ID, location: Vector[Double])
    extends SimpleCommandSerializable[Globz.Service with WorldBlock.Block] {
  override val REF_TYPE: Any = (SET_GLOB_LOCATION, id)
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- WorldBlock.getBlob(id)
      _ <- ZIO.fromOption(glob).flatMap { case pe: PhysicalEntity =>
        pe.teleport(location)
      }
    } yield ())
      .mapError(_ => GenericCommandError("Error setting glob location"))

}
object SET_GLOB_LOCATION {
  implicit val encoder: JsonEncoder[SET_GLOB_LOCATION] =
    DeriveJsonEncoder.gen[SET_GLOB_LOCATION]
  implicit val decoder: JsonDecoder[SET_GLOB_LOCATION] =
    DeriveJsonDecoder.gen[SET_GLOB_LOCATION]
}

case class RELATE_EGGS(
  egg1: ID,
  egg2: ID,
  globId: GLOBZ_ID,
  bidirectional: Boolean
) extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = (RELATE_EGGS, globId)
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
  implicit val encoder: JsonEncoder[RELATE_EGGS] =
    DeriveJsonEncoder.gen[RELATE_EGGS]
  implicit val decoder: JsonDecoder[RELATE_EGGS] =
    DeriveJsonDecoder.gen[RELATE_EGGS]
}

case class UNRELATE_EGGS(
  egg1: ID,
  egg2: ID,
  globId: GLOBZ_ID,
  bidirectional: Boolean
) extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = (UNRELATE_EGGS, globId)
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
  implicit val encoder: JsonEncoder[UNRELATE_EGGS] =
    DeriveJsonEncoder.gen[UNRELATE_EGGS]
  implicit val decoder: JsonDecoder[UNRELATE_EGGS] =
    DeriveJsonDecoder.gen[UNRELATE_EGGS]
}

case class UNRELATE_ALL(egg1: ID, globId: GLOBZ_ID, direction: Int)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  val REF_TYPE = (UNRELATE_ALL, globId)
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      globOp <- WorldBlock.getBlob(globId)
      glob <- ZIO.fromOption(globOp)
      e1 <- glob.get(egg1).flatMap(ZIO.fromOption(_))
      _ <- glob.unrelateAll(e1, direction)
    } yield ()).mapError(_ => GenericCommandError("Error relating eggz"))
}
object UNRELATE_ALL {
  implicit val encoder: JsonEncoder[UNRELATE_ALL] =
    DeriveJsonEncoder.gen[UNRELATE_ALL]
  implicit val decoder: JsonDecoder[UNRELATE_ALL] =
    DeriveJsonDecoder.gen[UNRELATE_ALL]
}
@deprecated
case class TICK_WORLD() extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = TICK_WORLD
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    WorldBlock
      .tickAllBlobs()
      .mapError(_ => GenericCommandError("Error ticking world"))
      .map(_ => ())
}
object TICK_WORLD {
  implicit val encoder: JsonEncoder[TICK_WORLD] =
    DeriveJsonEncoder.gen[TICK_WORLD]
  implicit val decoder: JsonDecoder[TICK_WORLD] =
    DeriveJsonDecoder.gen[TICK_WORLD]
}

case class START_EGG(eggId: ID, globId: GLOBZ_ID)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = (START_EGG, globId)
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      g <- WorldBlock.getBlob(globId)
      _ <- ZIO
        .fromOption(g)
        .flatMap(glob =>
          for {
            egg <- glob.get(eggId)
            _ <- ZIO
              .fromOption(egg)
              .flatMap((egg: GLOBZ_IN) =>
                glob.scheduleEgg(
                  egg,
                  egg.op.provide(ZLayer.succeed(glob)).map(_ => ())
                )
              )
          } yield ()
        )

    } yield ()).mapError(_ => GenericCommandError("error starting egg"))
}
object START_EGG {
  implicit val encoder: JsonEncoder[START_EGG] =
    DeriveJsonEncoder.gen[START_EGG]
  implicit val decoder: JsonDecoder[START_EGG] =
    DeriveJsonDecoder.gen[START_EGG]
}
//--------------------------------DESTINATIONS-------------------------------------------------------
case class TOGGLE_GRAVITATE(id: ID) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (TOGGLE_GRAVITATE, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      wb <- ZIO.service[WorldBlock.Block]
      blob <- wb
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(_ => ???, { case destinations: Destinations => destinations })
      gravitate <- blob
        .toggleGravitate()
        .zipRight(blob.isGravitating())
        .orElseFail(???)
      isActive <- blob.isActive().orElseFail(???)
    } yield MultiResponse(
      Chunk(
        QueuedPhysicsMessage(Chunk(SetInputLock(id, isActive && !gravitate))),
        QueuedServerMessage(Chunk(MSG(id, GravityActive(id, gravitate)))),
        QueuedClientMessage(id, Chunk(GravityActive(id, gravitate)))
      )
    )
}
object TOGGLE_GRAVITATE {
  implicit val encoder: JsonEncoder[TOGGLE_GRAVITATE] =
    DeriveJsonEncoder.gen[TOGGLE_GRAVITATE]
  implicit val decoder: JsonDecoder[TOGGLE_GRAVITATE] =
    DeriveJsonDecoder.gen[TOGGLE_GRAVITATE]
}

case class SET_GRAVITATE(id: ID, value: Boolean)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (TOGGLE_GRAVITATE, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      wb <- ZIO.service[WorldBlock.Block]
      blob <- wb
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(_ => ???, { case destinations: Destinations => destinations })
      res <- blob
        .setGravitate(value)
        .zipRight(blob.isGravitating())
        .orElseFail(???)
      isActive <- blob.isActive().orElseFail(???)
    } yield MultiResponse(
      Chunk(
        QueuedPhysicsMessage(Chunk(SetInputLock(id, isActive && !res))),
        QueuedServerMessage(Chunk(MSG(id, GravityActive(id, res)))),
        QueuedClientMessage(id, Chunk(GravityActive(id, res)))
      )
    )
}
object SET_GRAVITATE {
  implicit val encoder: JsonEncoder[SET_GRAVITATE] =
    DeriveJsonEncoder.gen[SET_GRAVITATE]
  implicit val decoder: JsonDecoder[SET_GRAVITATE] =
    DeriveJsonDecoder.gen[SET_GRAVITATE]
}

case class TOGGLE_DESTINATIONS(id: ID) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (TOGGLE_DESTINATIONS, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] = for {
    wb <- ZIO.service[WorldBlock.Block]
    blob <- wb
      .getBlob(id)
      .flatMap(ZIO.fromOption(_))
      .mapBoth(
        err =>
          GenericCommandError(
            s"Error while attemtping to toggle destinations for $id due to $err"
          ),
        { case pe: Destinations => pe }
      )
    _ <- blob
      .toggleDestinations()
      .mapError(err =>
        GenericCommandError(
          s"Error while toggling destinations for $id due to $err"
        )
      )
    isactive <- blob
      .isActive()
      .mapError(err =>
        GenericCommandError(
          s"Error while retrieving active for $id due to $err"
        )
      )
    gravitate <- blob.isGravitating().orElseFail(???)
  } yield MultiResponse(
    Chunk(
      QueuedPhysicsMessage(Chunk(SetInputLock(id, isactive && !gravitate))),
      QueuedServerMessage(Chunk(MSG(id, DestinationsActive(id, isactive)))),
      QueuedClientMessage(id, Chunk(DestinationsActive(id, isactive)))
    )
  )
}
object TOGGLE_DESTINATIONS {
  implicit val encoder: JsonEncoder[TOGGLE_DESTINATIONS] =
    DeriveJsonEncoder.gen[TOGGLE_DESTINATIONS]
  implicit val decoder: JsonDecoder[TOGGLE_DESTINATIONS] =
    DeriveJsonDecoder.gen[TOGGLE_DESTINATIONS]
}

case class SET_ACTIVE(id: ID, value: Boolean)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (TOGGLE_DESTINATIONS, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] = for {
    wb <- ZIO.service[WorldBlock.Block]
    blob <- wb
      .getBlob(id)
      .flatMap(ZIO.fromOption(_))
      .mapBoth(
        err =>
          GenericCommandError(
            s"Error while attemtping to toggle destinations for $id due to $err"
          ),
        { case pe: Destinations => pe }
      )
    isactive <- blob
      .setIsActive(value)
      .flatMap(_ => blob.isActive())
      .mapError(err =>
        GenericCommandError(
          s"Error while toggling destinations for $id due to $err"
        )
      )
    gravitate <- blob.isGravitating().orElseFail(???)
  } yield MultiResponse(
    Chunk(
      QueuedPhysicsMessage(Chunk(SetInputLock(id, isactive && !gravitate))),
      QueuedServerMessage(Chunk(MSG(id, DestinationsActive(id, isactive)))),
      QueuedClientMessage(id, Chunk(DestinationsActive(id, isactive)))
    )
  )
}
object SET_ACTIVE {
  implicit val encoder: JsonEncoder[SET_ACTIVE] =
    DeriveJsonEncoder.gen[SET_ACTIVE]
  implicit val decoder: JsonDecoder[SET_ACTIVE] =
    DeriveJsonDecoder.gen[SET_ACTIVE]
}

case class ADD_DESTINATION(id: ID, dest: destination)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (ADD_DESTINATION, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id)
      res <- ZIO
        .fromOption(blob)
        .flatMap { case entity: Destinations =>
          for {
            newDest <- dest.deserialize
            _ <- entity.addDestination(newDest)
            ret <- newDest.serialize
          } yield ret
        }
    } yield NewDestination(id, res)).orElseFail(
      GenericCommandError(s"Error adding destination to entity $id")
    )
}
object ADD_DESTINATION {
  implicit val encoder: JsonEncoder[ADD_DESTINATION] = DeriveJsonEncoder
    .gen[ADD_DESTINATION]
  implicit val decoder: JsonDecoder[ADD_DESTINATION] =
    DeriveJsonDecoder.gen[ADD_DESTINATION]
}
case class DELETE_DESTINATION(id: ID, uuid: UUID)
    extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = (DELETE_DESTINATION, id)

  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      glob <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          _ =>
            GenericCommandError(
              s"Could not find $id while deleting destination"
            ),
          { case de: Destinations => de }
        )
      _ <- glob
        .deleteDest(uuid)
        .orElseFail(
          GenericCommandError(s"Error while deleting destination with id $uuid")
        )
      _ <- glob
        .getAllDestinations()
        .flatMap(d => ZIO.log(s"all destinations post delete $d"))
        .orElseFail(GenericCommandError(""))
    } yield DeleteDestination(id, uuid)

object DELETE_DESTINATION {
  implicit val encoder: JsonEncoder[DELETE_DESTINATION] = DeriveJsonEncoder
    .gen[DELETE_DESTINATION]
  implicit val decoder: JsonDecoder[DELETE_DESTINATION] =
    DeriveJsonDecoder.gen[DELETE_DESTINATION]
}
case class GET_NEXT_INDEX(id: ID) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (GET_NEXT_INDEX, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      blob <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          err => GenericCommandError(s"Error getting blob $id, because $err"),
          { case d: Destinations => d }
        )
      res <- blob
        .getIndex()
        .orElseFail(GenericCommandError("Error while trying to get index"))
    } yield NextIndex(id, res)
}
object GET_NEXT_INDEX {
  implicit val encoder: JsonEncoder[GET_NEXT_INDEX] =
    DeriveJsonEncoder.gen[GET_NEXT_INDEX]
  implicit val decoder: JsonDecoder[GET_NEXT_INDEX] =
    DeriveJsonDecoder.gen[GET_NEXT_INDEX]
}

case class GET_NEXT_DESTINATION(id: ID)
    extends ResponseQuery[Globz.Service with WorldBlock.Block] {
  override val REF_TYPE: Any = (GET_NEXT_DESTINATION, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id)
      result <- ZIO.fromOption(blob).flatMap {
        case entity: Destinations with PhysicalEntity =>
          for {
            next <- entity.getNextDestination().flatMap(ZIO.fromOption(_))
            loc <- entity.getLocation
            r: QueryResponse <-
              if (
                GET_NEXT_DESTINATION.distance(next.location, loc) > next.radius
              ) for {
                x <- next.serialize
              } yield MSG(id, NextDestination(id, x))
              else
                for {
                  ser_dest <- entity
                    .popNextDestination()
                    .flatMap(ZIO.fromOption(_))
                    .flatMap(_.serialize)
                  res <- next.dest_type match {
                    case TELEPORT =>
                      for {
                        teleport_to_destination <- entity
                          .getNextDestination()
                          .flatMap(ZIO.fromOption(_))
                        teleport_to <- ZIO
                          .succeed(teleport_to_destination.location(0))
                          .zip(ZIO.succeed(teleport_to_destination.location(1)))
                          .zip(ZIO.succeed(teleport_to_destination.location(2)))
                        server_res = PaginatedResponse(
                          Chunk(
                            MSG(id, TeleportToNext(id, teleport_to)),
                            MSG(id, NextDestination(id, ser_dest))
                          )
                        )
                        client_res <- GET_TOP_LEVEL_TERRAIN_IN_DISTANCE(
                          teleport_to_destination.location,
                          1024
                        ).run.map(qr =>
                          QueuedClientMessage(
                            id,
                            Chunk(qr)
                          )
                        )
                      } yield MultiResponse(
                        Chunk(
                          server_res,
                          client_res
                        )
                      )
                    case GRAVITY =>
                      ZIO.succeed(MSG(id, NextDestination(id, ser_dest)))
                    case WAYPOINT =>
                      ZIO.succeed(MSG(id, NextDestination(id, ser_dest)))
                  }
                  index <- entity.getIndex()
                } yield MultiResponse(
                  Chunk(
                    res,
                    QueuedClientMessage(id, Chunk(NextIndex(id, index)))
                  )
                )

          } yield r
      }
    } yield result)
      .orElseFail(
        GenericCommandError(s"Error retrieving destination for id $id")
      )
      .fold(err => NoLocation(id), x => x)
}
object GET_NEXT_DESTINATION {
  implicit val encoder: JsonEncoder[GET_NEXT_DESTINATION] =
    DeriveJsonEncoder.gen[GET_NEXT_DESTINATION]
  implicit val decoder: JsonDecoder[GET_NEXT_DESTINATION] =
    DeriveJsonDecoder.gen[GET_NEXT_DESTINATION]

  def distance(v1: Vector[Double], v2: Vector[Double]): Double = {
    val minDist = v1.length min v2.length
    val v1_trunc = v1.take(minDist)
    val v2_trun = v2.take(minDist)
    // performance of this might not be great
    math.sqrt(
      v1_trunc
        .zip(v2_trun)
        .foldLeft(0.0)((acc, curr) =>
          acc + (curr._1 - curr._2) * (curr._1 - curr._2)
        )
    )
  }
}
case class GET_NEXT_DESTINATION_CLIENT(id: ID)
    extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = (GET_NEXT_DESTINATION_CLIENT, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      blob <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          err =>
            GenericCommandError(
              s"Error while getting next destination : Could not find glob with id $id"
            ),
          { case de: Destinations => de }
        )
      next_dest <- blob
        .getNextDestination()
        .flatMap(ZIO.fromOption(_))
        .flatMap(_.serialize)
        .orElseFail(
          GenericCommandError(
            s"Error while getting next destination for client $id"
          )
        )
    } yield NextDestination(id, next_dest)

object GET_NEXT_DESTINATION_CLIENT {
  implicit val encoder: JsonEncoder[GET_NEXT_DESTINATION_CLIENT] =
    DeriveJsonEncoder.gen[GET_NEXT_DESTINATION_CLIENT]
  implicit val decoder: JsonDecoder[GET_NEXT_DESTINATION_CLIENT] =
    DeriveJsonDecoder.gen[GET_NEXT_DESTINATION_CLIENT]
}
case class GET_ALL_DESTINATIONS(id: ID)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (GET_ALL_DESTINATIONS, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_)).map {
        case de: Destinations => de
      }
      allDestinations <-
        blob
          .getAllDestinations()
          .orElseFail(
            GenericCommandError(s"entity $id does not support destinations")
          )
      dests <- ZIO.foreachPar(allDestinations)(dest => dest.serialize)
      index <- blob.getIndex()
    } yield PaginatedResponse(
      Chunk(AllDestinations(id, dests), NextIndex(id, index))
    ))
      .orElseFail(
        GenericCommandError(s"Error retrieving destination for id $id")
      )
}
object GET_ALL_DESTINATIONS {
  implicit val encoder: JsonEncoder[GET_ALL_DESTINATIONS] =
    DeriveJsonEncoder.gen[GET_ALL_DESTINATIONS]
  implicit val decoder: JsonDecoder[GET_ALL_DESTINATIONS] =
    DeriveJsonDecoder.gen[GET_ALL_DESTINATIONS]
}

case class CLEAR_DESTINATIONS(id: GLOBZ_ID)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (CLEAR_DESTINATIONS, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      _ <- glob match {
        case pe: Destinations => pe.setIndex(0) *> pe.clearDestinations()
        case _                => ZIO.unit
      }
    } yield ClearDestinations())
      .orElseFail(GenericCommandError(s"Error clearing destinations for $id"))
}
object CLEAR_DESTINATIONS {
  implicit val encoder: JsonEncoder[CLEAR_DESTINATIONS] =
    DeriveJsonEncoder.gen[CLEAR_DESTINATIONS]
  implicit val decoder: JsonDecoder[CLEAR_DESTINATIONS] =
    DeriveJsonDecoder.gen[CLEAR_DESTINATIONS]
}

case class SET_MODE_DESTINATIONS(id: GLOBZ_ID, mode: Mode)
    extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = (SET_MODE_DESTINATIONS, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      wb <- ZIO.service[WorldBlock.Block]
      glob <- wb
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          err =>
            GenericCommandError(
              s"Could not update destination mode for $id due to $err "
            ),
          { case d: Destinations => d }
        )
      _ <- glob
        .setMode(mode)
        .mapError(err =>
          GenericCommandError(
            s"failed while updating destinations mode for $id due to $err"
          )
        )
    } yield ModeSet(mode)

object SET_MODE_DESTINATIONS {
  implicit val encoder: JsonEncoder[SET_MODE_DESTINATIONS] =
    DeriveJsonEncoder.gen[SET_MODE_DESTINATIONS]
  implicit val decoder: JsonDecoder[SET_MODE_DESTINATIONS] =
    DeriveJsonDecoder.gen[SET_MODE_DESTINATIONS]
}

case class SET_ACTIVE_DESTINATION(id: GLOBZ_ID, destination_id: UUID)
    extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = (SET_ACTIVE_DESTINATION, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      wb <- ZIO.service[WorldBlock.Block]
      glob <- wb
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          err =>
            GenericCommandError(
              s"Could not update destination mode for $id due to $err "
            ),
          { case d: Destinations => d }
        )
      _ <- glob
        .setActiveDest(destination_id)
        .mapError(err =>
          GenericCommandError(
            s"failed while updating destinations mode for $id due to $err"
          )
        )
    } yield ActiveDestination(id, destination_id)

object SET_ACTIVE_DESTINATION {
  implicit val encoder: JsonEncoder[SET_ACTIVE_DESTINATION] =
    DeriveJsonEncoder.gen[SET_ACTIVE_DESTINATION]
  implicit val decoder: JsonDecoder[SET_ACTIVE_DESTINATION] =
    DeriveJsonDecoder.gen[SET_ACTIVE_DESTINATION]
}
//---------------------------------INPUT-----------------------------------------------------------------------------------------------------
@deprecated
case class APPLY_VECTOR(id: ID, vec: (Double, Double, Double))
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = (APPLY_VECTOR, id)
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      _ <- glob match {
        case pe: PhysicalEntity =>
          pe.setInputVec(Vector(vec._1, vec._2, vec._3))
        case _ => ZIO.unit
      }
    } yield ()).orElseFail(GenericCommandError(""))
}
object APPLY_VECTOR {
  implicit val encoder: JsonEncoder[APPLY_VECTOR] =
    DeriveJsonEncoder.gen[APPLY_VECTOR]
  implicit val decoder: JsonDecoder[APPLY_VECTOR] =
    DeriveJsonDecoder.gen[APPLY_VECTOR]
}
@deprecated
case class GET_INPUT_VECTOR(id: ID) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (GET_INPUT_VECTOR, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      resraw <- glob match {
        case physicalEntity: PhysicalEntity => physicalEntity.getInputVec
      }
      res <- ZIO
        .fromOption(resraw)
        .flatMap(vec =>
          ZIO
            .succeed(vec(0))
            .zip(ZIO.succeed(vec(1)))
            .zip(ZIO.succeed(vec(2)))
        )
    } yield MSG(id, Input(id, res))).fold(_ => NoInput(id), x => x)
}
object GET_INPUT_VECTOR {
  implicit val encoder: JsonEncoder[GET_INPUT_VECTOR] =
    DeriveJsonEncoder.gen[GET_INPUT_VECTOR]
  implicit val decoder: JsonDecoder[GET_INPUT_VECTOR] =
    DeriveJsonDecoder.gen[GET_INPUT_VECTOR]
}
//------------------------------------STATS--------------------------------------------------------------------
case class SET_LV(id: GLOBZ_ID, lv: (Double, Double, Double))
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = SET_LV
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      _ <- glob match {
        case pe: PhysicalEntity => pe.setVelocity(Vector(lv._1, lv._2, lv._3));
        case _                  => ZIO.unit
      }
    } yield ())
      .orElseFail(GenericCommandError(s"error while trying to set lv for $id"))
}
object SET_LV {
  implicit val encoder: JsonEncoder[SET_LV] =
    DeriveJsonEncoder.gen[SET_LV]
  implicit val decoder: JsonDecoder[SET_LV] =
    DeriveJsonDecoder.gen[SET_LV]
}
case class LAZY_LV(id: GLOBZ_ID) extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = LAZY_LV
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      res <- blob match {
        case physicalEntity: PhysicalEntity =>
          physicalEntity.getVelocity.flatMap(vec =>
            ZIO
              .succeed(vec(0))
              .zip(ZIO.succeed(vec(1)))
              .zip(ZIO.succeed(vec(2)))
          )
      }
    } yield LV(id, res)).orElseFail(GenericCommandError(s"No LV found for $id"))
}
object LAZY_LV {
  implicit val encoder: JsonEncoder[LAZY_LV] =
    DeriveJsonEncoder.gen[LAZY_LV]
  implicit val decoder: JsonDecoder[LAZY_LV] =
    DeriveJsonDecoder.gen[LAZY_LV]
}

case class PhysicalStats(max_speed_delta: Double, speed_delta: Double)
object PhysicalStats {
  implicit val encoder: JsonEncoder[PhysicalStats] =
    DeriveJsonEncoder.gen[PhysicalStats]
  implicit val decoder: JsonDecoder[PhysicalStats] =
    DeriveJsonDecoder.gen[PhysicalStats]
}
case class ADJUST_PHYSICAL_STATS(id: GLOBZ_ID, delta: PhysicalStats)
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = (ADJUST_PHYSICAL_STATS, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .map { case pe: PhysicalEntity => pe }
        .mapError(_ => GenericCommandError(s"Could not find entity $id"))
//      _ <- glob.adjustMaxSpeed(delta.max_speed_delta)
      _ <- glob.adjustSpeed(delta.speed_delta)
      ms <- glob.getMaxSpeed
        .mapError(_ => GenericCommandError("Could not retrieve max speed"))
      speed <- glob.getSpeed.mapError(_ =>
        GenericCommandError("Could not retrieve speed")
      )
    } yield MultiResponse(
      Chunk(
        PhysStat(id, ms, speed),
        QueuedServerMessage(Chunk(PhysStat(id, ms, speed)))
      )
    ))
      .orElseFail(GenericCommandError(s"Error adjusting speed for $id"))
}
object ADJUST_PHYSICAL_STATS {
  implicit val encoder: JsonEncoder[ADJUST_PHYSICAL_STATS] =
    DeriveJsonEncoder.gen[ADJUST_PHYSICAL_STATS]
  implicit val decoder: JsonDecoder[ADJUST_PHYSICAL_STATS] =
    DeriveJsonDecoder.gen[ADJUST_PHYSICAL_STATS]
}

case class SET_SPEED(id: GLOBZ_ID, value: Double)
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = (SET_SPEED, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .map { case pe: PhysicalEntity => pe }
        .mapError(_ => GenericCommandError(s"Could not find entity $id"))
      speed <- glob.getSpeed
      _ <- glob.adjustSpeed(-speed + value)
      ms <- glob.getMaxSpeed
        .mapError(_ => GenericCommandError("Could not retrieve max speed"))
      speed <- glob.getSpeed.mapError(_ =>
        GenericCommandError("Could not retrieve speed")
      )
    } yield MultiResponse(
      Chunk(
        PhysStat(id, ms, speed),
        QueuedClientMessage(id, Chunk(PhysStat(id, ms, speed)))
      )
    ))
      .orElseFail(GenericCommandError(s"Error adjusting speed for $id"))
}
object SET_SPEED {
  implicit val encoder: JsonEncoder[SET_SPEED] =
    DeriveJsonEncoder.gen[SET_SPEED]
  implicit val decoder: JsonDecoder[SET_SPEED] =
    DeriveJsonDecoder.gen[SET_SPEED]
}
case class ADJUST_MAX_SPEED(id: GLOBZ_ID, delta: Double)
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = (ADJUST_MAX_SPEED, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .map { case pe: PhysicalEntity => pe }
        .mapError(_ => GenericCommandError(s"Could not find entity $id"))
      _ <- glob.adjustMaxSpeed(delta)
      ms <- glob.getMaxSpeed
        .mapError(_ => GenericCommandError("Could not retrieve max speed"))
      speed <- glob.getSpeed.mapError(_ =>
        GenericCommandError("Could not retrieve speed")
      )
    } yield MultiResponse(
      Chunk(
        PhysStat(id, ms, speed),
        QueuedClientMessage(id, Chunk(PhysStat(id, ms, speed)))
      )
    ))
      .orElseFail(GenericCommandError(s"Error adjusting speed for $id"))
}
object ADJUST_MAX_SPEED {
  implicit val encoder: JsonEncoder[ADJUST_MAX_SPEED] =
    DeriveJsonEncoder.gen[ADJUST_MAX_SPEED]
  implicit val decoder: JsonDecoder[ADJUST_MAX_SPEED] =
    DeriveJsonDecoder.gen[ADJUST_MAX_SPEED]
}
case class GET_PHYSICAL_STATS(id: GLOBZ_ID)
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = GET_PHYSICAL_STATS
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .map { case pe: PhysicalEntity => pe }
        .mapError(_ => GenericCommandError(s"Could not find entity $id"))
      maxspeed <- glob.getMaxSpeed
      speed <- glob.getSpeed
    } yield PhysStat(id, maxspeed, speed))
      .orElseFail(GenericCommandError(s"Error getting phys_stats for $id "))
}
object GET_PHYSICAL_STATS {
  implicit val encoder: JsonEncoder[GET_PHYSICAL_STATS] =
    DeriveJsonEncoder.gen[GET_PHYSICAL_STATS]
  implicit val decoder: JsonDecoder[GET_PHYSICAL_STATS] =
    DeriveJsonDecoder.gen[GET_PHYSICAL_STATS]
}
//TERRAIN---------------------------------------------------------
case class ADD_TERRAIN(id: String, location: Vector[Double])
    extends SimpleCommandSerializable[WorldBlock.Block] {
  val REF_TYPE: Any = ADD_TERRAIN
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    for {
      _ <- ZIO.log("ADDING TERRAIN")
      t <- WorldBlock.getTerrain.mapError(e =>
        GenericCommandError("Could not retrieve terrain manager")
      )
      _ <- ZIO.log("successfully retrieved terrain manager")
      _ <- t
        .add_terrain(id, location)
        .mapError(_ => GenericCommandError("Could not add terrain"))
      r <- t.get_terrain().mapError(_ => ???)
      _ <- ZIO.log(s"successfully added terrain : ${r}")
    } yield ()
}
object ADD_TERRAIN {
  implicit val encoder: JsonEncoder[ADD_TERRAIN] =
    DeriveJsonEncoder.gen[ADD_TERRAIN]
  implicit val decoder: JsonDecoder[ADD_TERRAIN] =
    DeriveJsonDecoder.gen[ADD_TERRAIN]
}

case class GET_ALL_TERRAIN(id: ID, non_relative: Boolean = false)
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = GET_ALL_TERRAIN
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    // todo split between get_all_terrain_within_block and get_all_terrain_within_radius_of_loc
    for {
      loc <- WorldBlock
        .getBlob(id)
        .flatMap {
          case Some(g: Player) => g.getLocation;
          case None            => ZIO.succeed(Vector(0.0, 0, 0))
        }
        .mapError(_ => GenericCommandError("failed to find player location"))
        .debug
        .fold(_ => Vector(0.0, 0, 0), x => x)
      // .flatMapError(err => ZIO.log(err.msg))
      _ <- ZIO.log(s"retrieving all terrain, non_relative:$non_relative")

      r3 <- WorldBlock.getTerrain
        .flatMap(
          _.serializeMini(loc, non_relative, 1000)
        )
        .mapError(_ => ???)
      rr = Chunk.from(
        r3.terrain.toSeq
          .grouped(100)
          .map(x => TerrainSet(Set(TerrainRegionM(x.toSet))))
      )
    } yield PaginatedResponse(rr)
}

object GET_ALL_TERRAIN {
  implicit val encoder: JsonEncoder[GET_ALL_TERRAIN] =
    DeriveJsonEncoder.gen[GET_ALL_TERRAIN]
  implicit val decoder: JsonDecoder[GET_ALL_TERRAIN] =
    DeriveJsonDecoder.gen[GET_ALL_TERRAIN]
}

case class GET_TERRAIN_WITHIN_DISTANCE(location: Vector[Double], radius: Double)
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = GET_TERRAIN_WITHIN_DISTANCE
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] = for {
    res <- WorldBlock.getTerrain
      .flatMap(
        _.get_terrain_within_distance(location, radius)
      )
      .flatMap(r => ZIO.foreachPar(r)(_.serialize()).map(_.flatten))
      .orElseFail {
        GenericCommandError(
          s"Error while retrieving terrain within radius $radius around point $location"
        )
      }
  } yield TerrainSet(res.toSet)
}

object GET_TERRAIN_WITHIN_DISTANCE {
  implicit val encoder: JsonEncoder[GET_TERRAIN_WITHIN_DISTANCE] =
    DeriveJsonEncoder.gen[GET_TERRAIN_WITHIN_DISTANCE]
  implicit val decoder: JsonDecoder[GET_TERRAIN_WITHIN_DISTANCE] =
    DeriveJsonDecoder.gen[GET_TERRAIN_WITHIN_DISTANCE]
}

case class GET_TERRAIN_WITHIN_PLAYER_DISTANCE(id: ID, radius: Double)
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = (GET_TERRAIN_WITHIN_PLAYER_DISTANCE, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] = for {
    _ <- ZIO.log("GET_TERRAIN_WITHIN_PLAYER_DISTANCE")
    location <- WorldBlock
      .getBlob(id)
      .flatMap(ZIO.fromOption(_))
      .flatMap {
        case p: Player => p.getLocation;
        case _ =>
          ZIO.fail(
            GenericCommandError(s"Player not found when retrieving terrain")
          )
      }
      .orElseFail {
        GenericCommandError(
          s"could not retrieve player location while attempting to get terrain"
        )
      }
    res <- WorldBlock.getTerrain
      .flatMap(
        _.get_terrain_within_distance(location, radius)
      )
      .flatMap(r => ZIO.foreachPar(r)(_.serialize()).map(_.flatten))
      .orElseFail {
        GenericCommandError(
          s"Error while retrieving terrain within radius $radius around point $location"
        )
      }
    _ <- ZIO.log(s"terrain within distance $res")
    rr = Chunk.from(res.grouped(100).map(x => TerrainSet(x.toSet)))
  } yield PaginatedResponse(rr)
}

object GET_TERRAIN_WITHIN_PLAYER_DISTANCE {
  implicit val encoder: JsonEncoder[GET_TERRAIN_WITHIN_PLAYER_DISTANCE] =
    DeriveJsonEncoder.gen[GET_TERRAIN_WITHIN_PLAYER_DISTANCE]
  implicit val decoder: JsonDecoder[GET_TERRAIN_WITHIN_PLAYER_DISTANCE] =
    DeriveJsonDecoder.gen[GET_TERRAIN_WITHIN_PLAYER_DISTANCE]
}

case class GET_TOP_LEVEL_TERRAIN() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_TOP_LEVEL_TERRAIN
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      terrain <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getTerrain)
        .mapError(_ => ???)
      top_terr <- terrain
        .get_top_terrain(1024)
        .mapError(_ => ???)
      _ <- ZIO.log(s"Found Top Terrain ${top_terr.size}")
      res_unit = top_terr.filter {
        case t: TerrainUnit => true; case _ => false
      }
      _ <- ZIO.log(s"bad terrain $res_unit")
      res = top_terr
        .filter {
          case t: TerrainRegion => true; case _ => false
        }
        .map { case t: TerrainRegion =>
          TerrainChunkm(
            t.uuid,
            (t.center(0), t.center(1), t.center(2)),
            t.radius
          )
        }

      _ <- terrain.cacheTerrain(top_terr).mapError(_ => ???)
//      ress = res.grouped(100).map(c => TerrainSet(c.toSet))
    } yield PaginatedResponse(res)
}
object GET_TOP_LEVEL_TERRAIN {
  implicit val encoder: JsonEncoder[GET_TOP_LEVEL_TERRAIN] =
    DeriveJsonEncoder.gen[GET_TOP_LEVEL_TERRAIN]
  implicit val decoder: JsonDecoder[GET_TOP_LEVEL_TERRAIN] =
    DeriveJsonDecoder.gen[GET_TOP_LEVEL_TERRAIN]
}

case class GET_TOP_LEVEL_TERRAIN_IN_DISTANCE(
  loc: Vector[Double],
  distance: Double
) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_TOP_LEVEL_TERRAIN_IN_DISTANCE
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      _ <- ZIO.log(s"Retrieving top terrain within distance $loc $distance")
      terrain <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getTerrain)
        .mapError(_ => ???)
      top_terr <- terrain
        .get_top_terrain_within_distance(loc, distance, 1024)
        .mapError(_ => ???)
      _ <- ZIO.log(s"Found Top Terrain ${top_terr.size}")
      res_unit = top_terr.filter {
        case t: TerrainUnit => true;
        case _              => false
      }
      _ <- ZIO.log(s"bad terrain $res_unit")
      res = top_terr
        .filter {
          case t: TerrainRegion => true;
          case _                => false
        }
        .map { case t: TerrainRegion =>
          TerrainChunkm(
            t.uuid,
            (t.center(0), t.center(1), t.center(2)),
            t.radius
          )
        }
      _ <- terrain.cacheTerrain(top_terr).mapError(_ => ???)
    } yield PaginatedResponse(res)
}
object GET_TOP_LEVEL_TERRAIN_IN_DISTANCE {
  implicit val encoder: JsonEncoder[GET_TOP_LEVEL_TERRAIN_IN_DISTANCE] =
    DeriveJsonEncoder.gen[GET_TOP_LEVEL_TERRAIN_IN_DISTANCE]
  implicit val decoder: JsonDecoder[GET_TOP_LEVEL_TERRAIN_IN_DISTANCE] =
    DeriveJsonDecoder.gen[GET_TOP_LEVEL_TERRAIN_IN_DISTANCE]
}

case class EXPAND_TERRAIN() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = EXPAND_TERRAIN
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] = for {
    terrain <- ZIO
      .serviceWithZIO[WorldBlock.Block](
        _.expandTerrain *> ZIO.log("Terrain Expanded for worldblock")
      )
      .mapError(_ => ???)
  } yield MultiResponse(Chunk())
}
object EXPAND_TERRAIN {
  implicit val encoder: JsonEncoder[EXPAND_TERRAIN] =
    DeriveJsonEncoder.gen[EXPAND_TERRAIN]
  implicit val decoder: JsonDecoder[EXPAND_TERRAIN] =
    DeriveJsonDecoder.gen[EXPAND_TERRAIN]
}

case class GET_CACHED_TERRAIN(id: UUID) extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = GET_CACHED_TERRAIN
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      terrain <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getTerrain)
        .mapError(_ => ???)
      quad <- terrain
        .get_cached(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(_ => ???, { case tr: TerrainRegion => tr })
      res <- quad.serializeMini(Vector(0), true, 0).mapError(_ => ???)
      rr = Chunk.from(
        res.terrain
          .grouped(100)
          .map(x => TerrainRegionm(x))
      )
    } yield PaginatedResponse(rr)

object GET_CACHED_TERRAIN {
  implicit val encoder: JsonEncoder[GET_CACHED_TERRAIN] =
    DeriveJsonEncoder.gen[GET_CACHED_TERRAIN]
  implicit val decoder: JsonDecoder[GET_CACHED_TERRAIN] =
    DeriveJsonDecoder.gen[GET_CACHED_TERRAIN]
}
//---------------------------------CONSOLE--------------------------------------------------------
case class NEXT_CMD() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = NEXT_CMD
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    ZIO.succeed(Completed())
}
object NEXT_CMD {
  implicit val encoder: JsonEncoder[NEXT_CMD] = DeriveJsonEncoder.gen[NEXT_CMD]
  implicit val decoder: JsonDecoder[NEXT_CMD] = DeriveJsonDecoder.gen[NEXT_CMD]
}
case class CONSOLE(
  execute_as: GLOBZ_ID,
  cmd: SerializableCommand[CONSOLE_ENV, Any]
) extends ResponseQuery[CONSOLE_ENV] {
  val REF_TYPE: Any = (cmd.REF_TYPE, execute_as)
  override def run
    : ZIO[Globz.Service with WorldBlock.Block, CommandError, QueryResponse] =
    cmd.run
      .fold(
        err => s"error processing command : $err",
        x => s"success: ${x.toString}"
      )
      .map(ConsoleResponse(_))
}
object CONSOLE {
  type CONSOLE_ENV = Globz.Service with WorldBlock.Block
  implicit val en: JsonEncoder[SerializableCommand[CONSOLE_ENV, Any]] =
    DeriveJsonEncoder.gen[SerializableCommand[_, _]].contramap(x => x)
  implicit val de: JsonDecoder[SerializableCommand[CONSOLE_ENV, Any]] =
    DeriveJsonDecoder.gen[SerializableCommand[CONSOLE_ENV, Any]] // .map(x => x)
  implicit val encoder: JsonEncoder[CONSOLE] = DeriveJsonEncoder.gen[CONSOLE]
  implicit val decoder: JsonDecoder[CONSOLE] = DeriveJsonDecoder.gen[CONSOLE]
}
