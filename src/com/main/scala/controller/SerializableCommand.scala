package controller

import controller.ADD_DESTINATION.AddDestinationError
import controller.CONSOLE.CONSOLE_ENV
import controller.CONSOLE.en
import controller.CREATE_PROWLER.CreateProwlerError
import controller.CREATE_SPIDER.CreateSPIDERError
import controller.DELETE_DESTINATION.DeleteDestinationError
import controller.FOLLOW_ENTITY.FollowEntityError
import controller.SUBSCRIBE.SubscriptionEnv
import controller.SerializableCommand.CommandError
import controller.SerializableCommand.GenericCommandError
import entity.Ability
import entity.AbilityDoesNotExistError
import entity.EmptyTerrain
import entity.GlobzModel
import entity.Health
import entity.LivingEntity
import entity.NPC
import entity.PhysicalEntity
import entity.Player
import entity.Prowler
import entity.Spider
import entity.Terrain
import entity.TerrainChunkM
import entity.TerrainModel
import entity.TerrainRegion
import entity.TerrainRegionM
import entity.TerrainUnit
import entity.TerrainUnitM
import entity.WorldBlock
import entity.WorldBlockEnvironment
import entity.LivingEntity.Item
import entity.TerrainRegion.TERRAIN_KEY
import entity.implicits.*
import physics.DESTINATION_TYPE.GRAVITY
import physics.DESTINATION_TYPE.TELEPORT
import physics.DESTINATION_TYPE.WAYPOINT
import physics.Destination
import physics.DestinationModel
import physics.Destinations
import physics.Mode
import physics.Mode.FORWARD
import physics.PhysicsTeleport
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
    extends ResponseQuery[Globz.Service with WorldBlock.Block] {
  val REF_TYPE: Any = CREATE_GLOB
  override def run
    : ZIO[Globz.Service with WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- Globz.create(globId)
      _ <- WorldBlock.spawnBlob(glob, location)
      _ <- glob match {
        case pe: PhysicalEntity => pe.teleport(location);
        case _                  => ZIO.unit
      }
      loc <- ZIO
        .succeed(location(0))
        .zip(ZIO.succeed(location(1)))
        .zip(ZIO.succeed(location(2)))
      glob_ser <- glob.serializeGlob
    } yield MultiResponse(
      Chunk(
        QueuedPhysicsMessage(Chunk(PhysicsTeleport(globId, loc))),
        QueuedServerMessage(
          Chunk(Entity(glob_ser), TeleportToNext(globId, loc))
        ),
        QueuedClientBroadcast(Chunk(Entity(glob_ser)))
      )
    )).orElseFail(GenericCommandError("error creating glob"))
}
object CREATE_GLOB {
  implicit val encoder: JsonEncoder[CREATE_GLOB] =
    DeriveJsonEncoder.gen[CREATE_GLOB]
  implicit val decoder: JsonDecoder[CREATE_GLOB] =
    DeriveJsonDecoder.gen[CREATE_GLOB]
}

case class CREATE_PROWLER(globId: GLOBZ_ID, location: Vector[Double])
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = CREATE_PROWLER

  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      prowler <- Globz
        .create(globId)
        .provide(ZLayer.succeed(Prowler))
        .mapBoth(
          _ =>
            CreateProwlerError(
              s"failed to create prowler glob with id $globId"
            ),
          { case pr: Prowler => pr }
        )
      _ <- WorldBlock.spawnBlob(prowler, location)
      _ <- prowler match {
        case pe: PhysicalEntity => pe.teleport(location);
        case _                  => ZIO.unit
      }
      maxspeed <- prowler.getMaxSpeed
      speed <- prowler.getSpeed
      _ <- prowler.adjustMaxSpeed(-maxspeed + 100)
      - <- prowler.adjustSpeed(-speed + 100)
      loc <- ZIO
        .succeed(location(0))
        .zip(ZIO.succeed(location(1)))
        .zip(ZIO.succeed(location(2)))
      prowler_ser <- prowler.serializeGlob
    } yield MultiResponse(
      Chunk(
        QueuedPhysicsMessage(Chunk(PhysicsTeleport(globId, loc))),
        QueuedServerMessage(Chunk(Entity(prowler_ser))),
        QueuedClientBroadcast(Chunk(Entity(prowler_ser)))
      )
    ))
      .orElseFail(GenericCommandError("error creating glob"))
}

object CREATE_PROWLER {
  implicit val encoder: JsonEncoder[CREATE_PROWLER] =
    DeriveJsonEncoder.gen[CREATE_PROWLER]
  implicit val decoder: JsonDecoder[CREATE_PROWLER] =
    DeriveJsonDecoder.gen[CREATE_PROWLER]

  case class CreateProwlerError(msg: String) extends CommandError
}

case class CREATE_SPIDER(globId: GLOBZ_ID, location: Vector[Double])
    extends ResponseQuery[WorldBlock.Block] {
  val REF_TYPE: Any = CREATE_SPIDER
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      spider <- Globz
        .create(globId)
        .provide(ZLayer.succeed(Spider))
        .mapBoth(
          _ =>
            CreateSPIDERError(
              s"failed to create prowler glob with id $globId"
            ),
          { case sp: Spider => sp }
        )
      _ <- WorldBlock.spawnBlob(spider, location)
      _ <- spider match {
        case pe: PhysicalEntity => pe.teleport(location);
        case _                  => ZIO.unit
      }
      maxspeed <- spider.getMaxSpeed
      speed <- spider.getSpeed
      _ <- spider.adjustMaxSpeed(-maxspeed)
      - <- spider.adjustSpeed(-speed)
      loc <- ZIO
        .succeed(location(0))
        .zip(ZIO.succeed(location(1)))
        .zip(ZIO.succeed(location(2)))
      spider_ser <- spider.serializeGlob
    } yield MultiResponse(
      Chunk(
        QueuedPhysicsMessage(Chunk(PhysicsTeleport(globId, loc))),
        QueuedServerMessage(Chunk(Entity(spider_ser))),
        QueuedClientBroadcast(Chunk(Entity(spider_ser)))
      )
    ))
      .orElseFail(CreateSPIDERError("error creating axis spider"))
}

object CREATE_SPIDER {
  implicit val encoder: JsonEncoder[CREATE_SPIDER] =
    DeriveJsonEncoder.gen[CREATE_SPIDER]
  implicit val decoder: JsonDecoder[CREATE_SPIDER] =
    DeriveJsonDecoder.gen[CREATE_SPIDER]

  case class CreateSPIDERError(msg: String) extends CommandError
}

case class GET_ALL_GLOBS() extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_ALL_GLOBS
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      models <- ZIO.foreachPar(res)(_.serializeGlob)
      ser_mods = Chunk.from(
        models
          .collect { case g: GlobzModel => g }
          .grouped(5)
          .map(globs => GlobSet(globs))
      )
    } yield PaginatedResponse(ser_mods))
      .orElseFail(GenericCommandError("Error retrieving blobs"))
}
object GET_ALL_GLOBS {
  implicit val encoder: JsonEncoder[GET_ALL_GLOBS] =
    DeriveJsonEncoder.gen[GET_ALL_GLOBS]
  implicit val decoder: JsonDecoder[GET_ALL_GLOBS] =
    DeriveJsonDecoder.gen[GET_ALL_GLOBS]
}

case class GET_GLOB(id: GLOBZ_ID) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = GET_GLOB
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .flatMap(_.serializeGlob)
    } yield GlobSet(Set(res)))
      .orElseFail(GenericCommandError("Error retrieving blobs"))
}

object GET_GLOB {
  implicit val encoder: JsonEncoder[GET_GLOB] =
    DeriveJsonEncoder.gen[GET_GLOB]
  implicit val decoder: JsonDecoder[GET_GLOB] =
    DeriveJsonDecoder.gen[GET_GLOB]
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
        .foreachPar(res)(_.getAll())
        .map(d => d.flatten)
      stats <- ZIO.foreachPar(nested)(egg => egg.serializeEgg)
    } yield EggSet(stats))
      .orElseFail(GenericCommandError("Error retrieving blobs"))
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
        .foreachPar(res)(_.getAll())
        .map(d => d.flatten)
      s <- ZIO.foreachPar(nested) { case x: Health =>
        for {
          health <- x.health
          energy <- x.energy
        } yield Stats(x.id, health, energy)
      }
    } yield AllStats(s))
      .orElseFail(GenericCommandError("Error retrieving blobs stats"))
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
        .serviceWithZIO[WorldBlock.Block](_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .mapBoth(_ => GenericCommandError(""), { case li: LivingEntity => li })
      _ <- glob.health
        .flatMap(h => glob.setHealth(h + value))
        .orElseFail(GenericCommandError(""))
      h <- glob.health.orElseFail(GenericCommandError(""))
    } yield MultiResponse(
      Chunk(
        HealthSet(id, h),
        QueuedClientBroadcast(Chunk(MSG(id, HealthSet(id, h))))
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
        .serviceWithZIO[WorldBlock.Block](_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .mapBoth(_ => GenericCommandError(""), { case li: LivingEntity => li })
      _ <- glob.health
        .flatMap(h => glob.setHealth(h - value))
        .orElseFail(GenericCommandError(""))
      h <- glob.health.orElseFail(GenericCommandError(""))
    } yield MultiResponse(
      Chunk(
        HealthSet(id, h),
        QueuedClientBroadcast(Chunk(MSG(id, HealthSet(id, h))))
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
    } yield ()).orElseFail(GenericCommandError("Error Creating egg"))
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
      .orElseFail(GenericCommandError(s"Error finding blob with $id"))
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
    } yield ()).orElseFail(GenericCommandError("Error setting glob location"))

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
      _ <- glob.relate(egg1, egg2, bidirectional, ZIO.unit)
    } yield ()).orElseFail(GenericCommandError("Error relating eggz"))
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
      _ <- glob.unrelate(egg1, egg2, bidirectional, ZIO.unit)
    } yield ()).orElseFail(GenericCommandError("Error relating eggz"))
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
      _ <- glob.unrelateAll(egg1, direction, ZIO.unit)
    } yield ()).orElseFail(GenericCommandError("Error relating eggz"))
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
      .mapBoth(_ => GenericCommandError("Error ticking world"), _ => ())
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
                  egg.op.provide(ZLayer.succeed(glob)).unit
                )
              )
          } yield ()
        )

    } yield ()).orElseFail(GenericCommandError("error starting egg"))
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
        .mapBoth(
          _ =>
            GenericCommandError("Could not find blob while toggling gravitate"),
          { case destinations: Destinations => destinations }
        )
      gravitate <- blob
        .toggleGravitate()
        .zipRight(blob.isGravitating())
        .orElseFail(GenericCommandError("Error while toggling gravitate"))
      isActive <- blob
        .isActive()
        .orElseFail(GenericCommandError("Error trying to retrieve isActive"))
      client_messages <- blob match {
        case _: NPC => ZIO.succeed(Chunk())
        case _: Player =>
          ZIO.succeed(
            Chunk(
              QueuedClientMessage(id, Chunk(GravityActive(id, gravitate)))
            )
          )
        case _ => ZIO.succeed(Chunk())
      }
    } yield MultiResponse(
      Chunk(
        QueuedPhysicsMessage(Chunk(SetInputLock(id, isActive && !gravitate))),
        QueuedServerMessage(Chunk(MSG(id, GravityActive(id, gravitate))))
      ) ++ client_messages
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
        .mapBoth(
          _ =>
            GenericCommandError(
              "Could not find blob while trying to set gravitate"
            ),
          { case destinations: Destinations => destinations }
        )
      res <- blob
        .setGravitate(value)
        .zipRight(blob.isGravitating())
        .orElseFail(GenericCommandError("Could not set gravitate"))
      isActive <- blob
        .isActive()
        .orElseFail(
          GenericCommandError(
            "Could not retrieve is active after setting gravitate"
          )
        )
      client_messages <- blob match {
        case _: NPC => ZIO.succeed(Chunk())
        case _: Player =>
          ZIO.succeed(
            Chunk(QueuedClientMessage(id, Chunk(GravityActive(id, res))))
          )
        case _ => ZIO.succeed(Chunk())
      }
    } yield MultiResponse(
      Chunk(
        QueuedPhysicsMessage(Chunk(SetInputLock(id, isActive && !res))),
        QueuedServerMessage(Chunk(MSG(id, GravityActive(id, res))))
      ) ++ client_messages
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
    gravitate <- blob
      .isGravitating()
      .orElseFail(GenericCommandError("Could not retrieve isGravitating"))
    client_messages <- blob match {
      case _: NPC => ZIO.succeed(Chunk())
      case _: Player =>
        ZIO.succeed(
          Chunk(
            QueuedClientMessage(id, Chunk(DestinationsActive(id, isactive)))
          )
        )
      case _ => ZIO.succeed(Chunk())
    }
  } yield MultiResponse(
    Chunk(
      QueuedPhysicsMessage(Chunk(SetInputLock(id, isactive && !gravitate))),
      QueuedServerMessage(Chunk(MSG(id, DestinationsActive(id, isactive))))
    ) ++ client_messages
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
      .zipRight(blob.isActive())
      .mapError(err =>
        GenericCommandError(
          s"Error while toggling destinations for $id due to $err"
        )
      )
    gravitate <- blob
      .isGravitating()
      .orElseFail(GenericCommandError("Could not retrieve isGravitating"))
    client_messages <- blob match {
      case _: NPC => ZIO.succeed(Chunk())
      case _: Player =>
        ZIO.succeed(
          Chunk(
            QueuedClientMessage(id, Chunk(DestinationsActive(id, isactive)))
          )
        )
      case _ => ZIO.succeed(Chunk())
    }
  } yield MultiResponse(
    Chunk(
      QueuedPhysicsMessage(Chunk(SetInputLock(id, isactive && !gravitate))),
      QueuedServerMessage(Chunk(MSG(id, DestinationsActive(id, isactive))))
    ) ++ client_messages
  )
}
object SET_ACTIVE {
  implicit val encoder: JsonEncoder[SET_ACTIVE] =
    DeriveJsonEncoder.gen[SET_ACTIVE]
  implicit val decoder: JsonDecoder[SET_ACTIVE] =
    DeriveJsonDecoder.gen[SET_ACTIVE]
}
case class FOLLOW_ENTITY(id: ID, target: ID)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = (FOLLOW_ENTITY, id, target)
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    for {
      worldblock <- ZIO.service[WorldBlock.Block]
      entity <- worldblock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          _ => FollowEntityError(s"Could not find entity with id $id"),
          { case li: LivingEntity =>
            li
          }
        )
      _ <- worldblock
        .npc_handler()
        .flatMap(_.add_entity_as_npc(entity))
        .orElseFail(FollowEntityError("Error while scheduling follow entity "))
      // should maybe be in npc handler and not entity
      _ <- entity
        .relate(
          id,
          target,
          false,
          worldblock
            .npc_handler()
            .flatMap(
              _.scheduleEgg(
                entity,
                entity
                  .follow_player(target)
                  .provide(ZLayer.succeed(worldblock))
                  .mapError(err => err.toString)
              )
            )
            .orElseFail("Error while scheduling")
        )
        .orElseFail(FollowEntityError("Error while following entity"))
    } yield ()
}
object FOLLOW_ENTITY {
  implicit val encoder: JsonEncoder[FOLLOW_ENTITY] = DeriveJsonEncoder
    .gen[FOLLOW_ENTITY]
  implicit val decoder: JsonDecoder[FOLLOW_ENTITY] =
    DeriveJsonDecoder.gen[FOLLOW_ENTITY]

  case class FollowEntityError(msg: String) extends CommandError
}
case class UNFOLLOW_ENTITY(id: GLOBZ_ID, target: GLOBZ_ID)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override val REF_TYPE: Any = (FOLLOW_ENTITY, id, target)
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    for {
      worldblock <- ZIO.service[WorldBlock.Block]
      entity <- worldblock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          _ => FollowEntityError(s"Could not find entity with id $id"),
          { case li: LivingEntity =>
            li
          }
        )
      _ <- worldblock
        .npc_handler()
        .flatMap(_.add_entity_as_npc(entity))
        .orElseFail(FollowEntityError("Error while scheduling follow entity "))
      // should maybe be in npc handler and not entity
      _ <- entity
        .unrelate(
          id,
          target,
          false,
          ZIO.unit
        )
        .orElseFail(FollowEntityError("Error while following entity"))
    } yield ()
}
object UNFOLLOW_ENTITY {
  implicit val encoder: JsonEncoder[UNFOLLOW_ENTITY] = DeriveJsonEncoder
    .gen[UNFOLLOW_ENTITY]
  implicit val decoder: JsonDecoder[UNFOLLOW_ENTITY] =
    DeriveJsonDecoder.gen[UNFOLLOW_ENTITY]

  case class FollowEntityError(msg: String) extends CommandError
}
case class ADD_DESTINATION(id: ID, dest: destination)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (ADD_DESTINATION, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id)
      b <- WorldBlock
        .getBlob(id)
        .flatMap(ZIO.fromOption(_))
        .map { case de: Destinations => de }
        .mapError(_ =>
          AddDestinationError(s"Could not find destinations entity with id $id")
        )
      res <- for {
        newDest <- dest.deserialize
        _ <- b.addDestination(newDest)
        ret <- newDest.serialize
      } yield ret
      ind_maybe <- b.getAllDestinations().map(_.size).map {
        case 1 => Chunk(ActiveDestination(id, res.uuid)); case _ => Chunk()
      }
    } yield PaginatedResponse(
      Chunk(NewDestination(id, res)) ++ ind_maybe
    )).orElseFail(
      GenericCommandError(s"Error adding destination to entity $id")
    )
}
object ADD_DESTINATION {
  implicit val encoder: JsonEncoder[ADD_DESTINATION] = DeriveJsonEncoder
    .gen[ADD_DESTINATION]
  implicit val decoder: JsonDecoder[ADD_DESTINATION] =
    DeriveJsonDecoder.gen[ADD_DESTINATION]

  case class AddDestinationError(msg: String) extends CommandError
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
      active_dest <- glob
        .getDestAtCurrentIndex()
        .mapBoth(
          _ =>
            DeleteDestinationError("Error while retrieving active destination"),
          {
            case Some(dest) => Chunk(ActiveDestination(id, dest.uuid));
            case _          => Chunk()
          }
        )
    } yield PaginatedResponse(
      Chunk(DeleteDestination(id, uuid)) ++ active_dest
    )

object DELETE_DESTINATION {
  implicit val encoder: JsonEncoder[DELETE_DESTINATION] = DeriveJsonEncoder
    .gen[DELETE_DESTINATION]
  implicit val decoder: JsonDecoder[DELETE_DESTINATION] =
    DeriveJsonDecoder.gen[DELETE_DESTINATION]

  case class DeleteDestinationError(msg: String) extends CommandError
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
      active_dest <- blob.getDestAtIndex(index).map {
        case Some(dest) => Chunk(ActiveDestination(id, dest.uuid));
        case _          => Chunk()
      }
    } yield PaginatedResponse(
      Chunk(AllDestinations(id, dests), NextIndex(id, index)) ++ active_dest
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
        .mapBoth(
          _ => GenericCommandError(s"Could not find entity $id"),
          { case pe: PhysicalEntity => pe }
        )
//      _ <- glob.adjustMaxSpeed(delta.max_speed_delta)
      _ <- glob.adjustSpeed(delta.speed_delta)
      ms <- glob.getMaxSpeed.orElseFail(
        GenericCommandError("Could not retrieve max speed")
      )
      speed <- glob.getSpeed.orElseFail(
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
        .mapBoth(
          _ => GenericCommandError(s"Could not find entity $id"),
          { case pe: PhysicalEntity => pe }
        )
      speed <- glob.getSpeed
      _ <- glob.adjustSpeed(-speed + value)
      ms <- glob.getMaxSpeed.orElseFail(
        GenericCommandError("Could not retrieve max speed")
      )
      speed <- glob.getSpeed.orElseFail(
        GenericCommandError("Could not retrieve speed")
      )
      client_messages <- glob match {
        case _: NPC => ZIO.succeed(Chunk())
        case _: Player =>
          ZIO.succeed(
            Chunk(
              QueuedClientMessage(id, Chunk(PhysStat(id, ms, speed)))
            )
          )
        case _ => ZIO.succeed(Chunk())
      }
    } yield MultiResponse(
      Chunk(
        PhysStat(id, ms, speed)
      ) ++ client_messages
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
        .mapBoth(
          _ => GenericCommandError(s"Could not find entity $id"),
          { case pe: PhysicalEntity => pe }
        )
      _ <- glob.adjustMaxSpeed(delta)
      ms <- glob.getMaxSpeed.orElseFail(
        GenericCommandError("Could not retrieve max speed")
      )
      speed <- glob.getSpeed.orElseFail(
        GenericCommandError("Could not retrieve speed")
      )
      client_messages <- glob match {
        case _: NPC => ZIO.succeed(Chunk())
        case _: Player =>
          ZIO.succeed(
            Chunk(
              QueuedClientMessage(id, Chunk(PhysStat(id, ms, speed)))
            )
          )
        case _ => ZIO.succeed(Chunk())
      }
    } yield MultiResponse(
      Chunk(
        PhysStat(id, ms, speed)
      ) ++ client_messages
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
        .mapBoth(
          _ => GenericCommandError(s"Could not find entity $id"),
          { case pe: PhysicalEntity => pe }
        )
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
        .orElseFail(GenericCommandError("Could not add terrain"))
      r <- t
        .get_terrain()
        .orElseFail(
          GenericCommandError("Could not get terrain after adding terrain")
        )
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
        .orElseFail(GenericCommandError("failed to find player location"))
        .debug
        .fold(_ => Vector(0.0, 0, 0), x => x)
      // .flatMapError(err => ZIO.log(err.msg))
      _ <- ZIO.log(s"retrieving all terrain, non_relative:$non_relative")

      r3 <- WorldBlock.getTerrain
        .flatMap(
          _.serializeMini(loc, non_relative, 1000)
        )
        .orElseFail(GenericCommandError("Could not serialize all terrain"))
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
        .orElseFail(GenericCommandError("Could not get terrain for worldblock"))
      top_terr <- terrain
        .get_top_terrain(1024)
        .orElseFail(GenericCommandError("Error while getting All top terrain"))
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
      _ <- terrain
        .cacheTerrain(top_terr)
        .orElseFail {
          GenericCommandError(
            "Error while caching terrain after retrieving all top terrain"
          )
        }
    } yield PaginatedResponse(res)
}
object GET_TOP_LEVEL_TERRAIN {
  implicit val encoder: JsonEncoder[GET_TOP_LEVEL_TERRAIN] =
    DeriveJsonEncoder.gen[GET_TOP_LEVEL_TERRAIN]
  implicit val decoder: JsonDecoder[GET_TOP_LEVEL_TERRAIN] =
    DeriveJsonDecoder.gen[GET_TOP_LEVEL_TERRAIN]
}
implicit class TerrainUtils(terrain: Chunk[Terrain]) {
  def serialize_as_chunks(
    default_chunk_size: Double = 1024
  ): ZIO[Any, Nothing, Chunk[QueryResponse]] =
    for {
      filtered_res <- ZIO.filterPar(terrain) {
        case tr: TerrainRegion => ZIO.succeed(true);
        case e: EmptyTerrain   => ZIO.succeed(true);
        case tu: TerrainUnit   => ZIO.succeed(true);
        case _                 => ZIO.succeed(false)
      }
      res <- ZIO.foreachPar(filtered_res) {
        case t: TerrainUnit =>
          t.entitiesRef.get.map(
            TerrainUnitm(
              t.uuid,
              (t.location(0), t.location(1), t.location(2)),
              _
            )
          )
        case t: TerrainRegion =>
          ZIO.succeed(
            TerrainChunkm(
              t.uuid,
              (t.center(0), t.center(1), t.center(2)),
              t.radius
            )
          )
        case e: EmptyTerrain =>
          ZIO.succeed(
            EmptyChunk(
              e.uuid,
              (e.center(0), e.center(1), e.center(2)),
              e.radius
            )
          )
      }
    } yield res
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
        .orElseFail(GenericCommandError("Could not get worldblock"))
      chunk_size <- ZIO.succeed(1024)
      top_terr <- terrain
        .get_top_terrain_within_distance(loc, distance, chunk_size)
        .orElseFail {
          GenericCommandError(
            "Problem while retrieving top terrain within distance"
          )
        }
//      _ <- ZIO.log(s"Found Top Terrain ${top_terr.size}")
//      res_unit <- ZIO.filterPar(top_terr) {
//        case t: TerrainUnit => ZIO.succeed(true);
//        case _              => ZIO.succeed(false)
//      }
//      _ <- ZIO.log(s"bad terrain ${res_unit.size}").when(res_unit.nonEmpty)
      res <- top_terr.serialize_as_chunks(1024)
      _ <- terrain
        .cacheTerrain(top_terr)
        .orElseFail(GenericCommandError("Problem while caching terrain"))
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
      .orElseFail(GenericCommandError("Error while expanding terrain"))
  } yield MultiResponse(Chunk())
}
object EXPAND_TERRAIN {
  implicit val encoder: JsonEncoder[EXPAND_TERRAIN] =
    DeriveJsonEncoder.gen[EXPAND_TERRAIN]
  implicit val decoder: JsonDecoder[EXPAND_TERRAIN] =
    DeriveJsonDecoder.gen[EXPAND_TERRAIN]
}

case class FILL_EMPTY_CHUNK(id: TERRAIN_KEY, trigger_entity: GLOBZ_ID)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = FILL_EMPTY_CHUNK
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      _ <- ZIO.log(s"Filling empty chunk $id for entity $trigger_entity")
      wb <- ZIO.service[WorldBlock.Block]
      empty_chunk <- wb.getTerrain
        .flatMap { case tr: TerrainRegion => tr.get_cached(id) }
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          _ =>
            GenericCommandError(
              s"Could not find empty chunk with $id in cache"
            ),
          { case e: EmptyTerrain => e }
        )
      fill_set <- WorldBlockEnvironment
        .create_terrain_set(
          1000,
          empty_chunk.radius,
          empty_chunk.center
        )
        .orElseFail(GenericCommandError("Error while creating fill set"))
      filled_chunk <- empty_chunk
        .fill(fill_set)
        .mapBoth(
          _ => GenericCommandError("Problem while filling empty chunk"),
          { case tr: TerrainRegion => tr }
        )
      //      _ <- terrain.remove_cached
      _ <- wb.getTerrain
        .flatMap { case tr: TerrainRegion => tr.addQuadrant(filled_chunk) }
        .orElseFail {
          GenericCommandError(
            "problem while adding filled chunk to global terrain"
          )
        }
      blob_location <- wb
        .getBlob(trigger_entity)
        .flatMap(ZIO.fromOption(_))
        .flatMap { case pe: PhysicalEntity => pe.getLocation }
        .orElseFail(
          GenericCommandError("Could not find glob while filling terrain")
        )
      top_terr <- filled_chunk
        .get_top_terrain_within_distance(
          blob_location,
          2048,
          math.min(filled_chunk.radius / 2, 4096.0)
        )
        .orElseFail(
          GenericCommandError("Error while getting top terrain post fill")
        )
      _ <- wb.getTerrain
        .flatMap { case tr: TerrainRegion =>
          tr.cacheTerrain(top_terr)
        }
        .orElseFail(
          GenericCommandError("Error while caching terrain during generation")
        )
      newchunks <- wb.getTerrain
        .flatMap { case terrain: TerrainRegion =>
          wb.expandTerrain
            .when(
              !(blob_location - terrain.center)
                .forall(math.abs(_) < terrain.radius - 2 * filled_chunk.radius)
            )
        }
        .orElseFail(
          GenericCommandError("Problem while checking if terrain should expand")
        )
      chunksShouldbeEmpty <- ZIO.log(
        s"Expanded chunks ${newchunks.map(_.size)}"
      )
      mapped_result <- (top_terr ++ newchunks.getOrElse(Chunk()))
        .serialize_as_chunks(1024)
      _ <- ZIO.log(s"Sending top terrain post fill ${mapped_result.size}")
    } yield MultiResponse(
      Chunk(
        PaginatedResponse(mapped_result),
        QueuedClientMessage(trigger_entity, mapped_result)
      )
    )
}
object FILL_EMPTY_CHUNK {
  implicit val encoder: JsonEncoder[FILL_EMPTY_CHUNK] =
    DeriveJsonEncoder.gen[FILL_EMPTY_CHUNK]
  implicit val decoder: JsonDecoder[FILL_EMPTY_CHUNK] =
    DeriveJsonDecoder.gen[FILL_EMPTY_CHUNK]
}

case class GET_CACHED_TERRAIN(id: TERRAIN_KEY)
    extends ResponseQuery[WorldBlock.Block]:
  override val REF_TYPE: Any = GET_CACHED_TERRAIN
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      terrain <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getTerrain)
        .orElseFail(
          GenericCommandError("Could not retrieve terrain from worldblock")
        )
      quad <- terrain
        .get_cached(id)
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          _ => GenericCommandError(s"could not find terrain in cache $id"),
          { case tr: TerrainRegion => tr }
        )
      res <- quad
        .serializeMini(Vector(0), true, 0)
        .orElseFail(
          GenericCommandError("Error while serializing terrain cache")
        )
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
//---------------------------------ABILITIES-----------------------------------------------------
case class ABILITY(from: GLOBZ_ID, ability_id: Int)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (ABILITY, from, ability_id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      entity <- ZIO
        .serviceWithZIO[WorldBlock.Block](wb =>
          wb.getBlob(from).flatMap(ZIO.fromOption(_)).map {
            case li: LivingEntity => li
          }
        )
        .orElseFail(GenericCommandError(s"No entity found with Id $from"))
      res <- Ability
        .make(ability_id, from)
        .flatMap(_.run)
        .whenZIO(entity.getInventory().map(_.contains(ability_id)))
        .flatMap(ZIO.fromOption(_))
        .foldZIO(
          {
            case AbilityDoesNotExistError => ZIO.succeed(QueryResponse.Empty)
            case _ =>
              ZIO.succeed(
                Fizzle(
                  from,
                  ability_id,
                  "Item for ability not found in inventory"
                )
              )
          },
          ZIO.succeed(_)
        )

    } yield res
}
object ABILITY {
  implicit val encoder: JsonEncoder[ABILITY] = DeriveJsonEncoder.gen[ABILITY]
  implicit val decoder: JsonDecoder[ABILITY] = DeriveJsonDecoder.gen[ABILITY]
}
case class ADD_ITEM(id: GLOBZ_ID, item: Item)
    extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (ADD_ITEM, id)
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      glob <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          _ => AbilitiesError(s"Could not find living entity for $id"),
          { case li: LivingEntity => li }
        )
      _ <- glob
        .add(item)
        .orElseFail(
          AbilitiesError(s"Error while adding item to inventory for id $id")
        )
    } yield MultiResponse(
      Chunk(
        QueuedClientMessage(id, Chunk(ItemAdded(id, item)))
      )
    )
}
object ADD_ITEM {
  implicit val encoder: JsonEncoder[ADD_ITEM] = DeriveJsonEncoder.gen[ADD_ITEM]
  implicit val decoder: JsonDecoder[ADD_ITEM] = DeriveJsonDecoder.gen[ADD_ITEM]
}
case class GET_INVENTORY(id: GLOBZ_ID) extends ResponseQuery[WorldBlock.Block] {
  override val REF_TYPE: Any = (GET_INVENTORY, id)

  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      glob <- ZIO
        .service[WorldBlock.Block]
        .flatMap(_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .map { case li: LivingEntity => li }
        .mapError(_ => InventoryError(s"Could not find living entity $id "))
      res <- glob
        .getInventory()
        .mapError(_ =>
          InventoryError(s"Error while trying to get inventory for $id")
        )
    } yield Inventory(id, res)
}
object GET_INVENTORY {
  implicit val encoder: JsonEncoder[GET_INVENTORY] =
    DeriveJsonEncoder.gen[GET_INVENTORY]
  implicit val decoder: JsonDecoder[GET_INVENTORY] =
    DeriveJsonDecoder.gen[GET_INVENTORY]
}
case class AbilitiesError(msg: String) extends CommandError
case class InventoryError(msg: String) extends CommandError
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
