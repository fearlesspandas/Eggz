package controller

import controller.CONSOLE.CONSOLE_ENV
import controller.SUBSCRIBE.SubscriptionEnv
import controller.SerializableCommand.CommandError
import controller.SerializableCommand.GenericCommandError
import entity.GlobzModel
import entity.PhysicalEntity
import entity.WorldBlock
import physics.Destination
import physics.DestinationModel
import physics.Destinations
import physics.destination
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.RepairEgg
import zio.ZIO
import zio.ZLayer
import zio.http.ChannelEvent.Read
import zio.http.WebSocketFrame
import zio.http._
import zio.json.DecoderOps
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.EncoderOps
import zio.json.JsonDecoder
import zio.json.JsonEncoder

sealed trait SerializableCommand[-Env, +Out] extends Command[Env, Out]
object SerializableCommand {
  implicit val encoder: JsonEncoder[SerializableCommand[_, _]] =
    DeriveJsonEncoder.gen[SerializableCommand[Nothing, Any]].contramap(x => x)
  implicit val decoder: JsonDecoder[SerializableCommand[_, _]] =
    DeriveJsonDecoder.gen[SerializableCommand[Nothing, Any]].map(x => x)
  trait CommandError
  case class GenericCommandError(msg: String) extends CommandError

}
sealed trait SimpleCommandSerializable[-Env]
    extends SerializableCommand[Env, Unit]
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
    extends Subscription[SubscriptionEnv]
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
      10
    ).run
}

case class CREATE_GLOB(globId: GLOBZ_ID, location: Vector[Double])
    extends SimpleCommandSerializable[Globz.Service with WorldBlock.Block] {
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
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      res <- WorldBlock.getAllBlobs()
      models <- ZIO.collectAllPar(res.map(_.serializeGlob))
    } yield GlobSet(models.collect { case g: GlobzModel => g })).mapError(_ =>
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
  implicit val encoder: JsonEncoder[CREATE_REPAIR_EGG] =
    DeriveJsonEncoder.gen[CREATE_REPAIR_EGG]
  implicit val decoder: JsonDecoder[CREATE_REPAIR_EGG] =
    DeriveJsonDecoder.gen[CREATE_REPAIR_EGG]
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

case class GET_GLOB_LOCATION(id: GLOBZ_ID)
    extends ResponseQuery[WorldBlock.Block] {
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

case class SET_GLOB_LOCATION(id: GLOBZ_ID, location: Vector[Double])
    extends SimpleCommandSerializable[Globz.Service with WorldBlock.Block] {
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

case class TICK_WORLD() extends SimpleCommandSerializable[WorldBlock.Block] {
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
  implicit val encoder: JsonEncoder[START_EGG] =
    DeriveJsonEncoder.gen[START_EGG]
  implicit val decoder: JsonDecoder[START_EGG] =
    DeriveJsonDecoder.gen[START_EGG]
}

case class ADD_DESTINATION(id: ID, dest: destination)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      blob <- WorldBlock.getBlob(id)
      _ <- ZIO
        .fromOption(blob)
        .flatMap { case entity: Destinations =>
          for {
            res <- dest.deserialize
            _ <- entity.addDestination(res)
          } yield ()
        }
    } yield ()).mapError(_ =>
      GenericCommandError(s"Error adding destination to entity $id")
    )
}
object ADD_DESTINATION {
  implicit val encoder: JsonEncoder[ADD_DESTINATION] = DeriveJsonEncoder
    .gen[ADD_DESTINATION]

  implicit val decoder: JsonDecoder[ADD_DESTINATION] =
    DeriveJsonDecoder.gen[ADD_DESTINATION]

}

case class GET_NEXT_DESTINATION(id: ID)
    extends ResponseQuery[Globz.Service with WorldBlock.Block] {

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
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id)
      location <- ZIO
        .fromOption(blob)
        .flatMap { case entity: Destinations with PhysicalEntity =>
          for {
            next <- entity.getNextDestination().flatMap(ZIO.fromOption(_))
            currentLoc <- entity.getLocation
            dist = distance(next.location, currentLoc)
            res <-
              if (dist > next.radius) entity.getNextDestination()
              else entity.popNextDestination()
          } yield res
        }
        .mapError(_ =>
          GenericCommandError(
            "Entity is not of type Destination with PhysicalEntity"
          )
        )
      dest <- ZIO
        .fromOption(location)
        .flatMap(_.serialize)
    } yield MSG(id, NextDestination(id, dest)))
      .mapError(_ =>
        GenericCommandError(s"Error retrieving destination for id $id")
      )
      .fold(err => NoLocation(id), x => x)
}
object GET_NEXT_DESTINATION {
  implicit val encoder: JsonEncoder[GET_NEXT_DESTINATION] =
    DeriveJsonEncoder.gen[GET_NEXT_DESTINATION]
  implicit val decoder: JsonDecoder[GET_NEXT_DESTINATION] =
    DeriveJsonDecoder.gen[GET_NEXT_DESTINATION]

  case class ADDED_DESTINATION(id: ID, location: String)
}

case class GET_ALL_DESTINATIONS(id: ID)
    extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      blob <- WorldBlock.getBlob(id)
      allDestinations <- ZIO
        .fromOption(blob)
        .flatMap { case entity: Destinations => entity.getAllDestinations() }
        .orElseFail(
          GenericCommandError(s"entity $id does not support destinations")
        )
      dests <- ZIO.foreachPar(allDestinations)(dest => dest.serialize)
    } yield AllDestinations(id, dests))
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
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      _ <- glob match {
        case pe: Destinations => pe.clearDestinations()
        case _                => ZIO.unit
      }
    } yield ())
      .orElseFail(GenericCommandError(s"Error clearing destinations for $id"))
}
object CLEAR_DESTINATIONS {
  implicit val encoder: JsonEncoder[CLEAR_DESTINATIONS] =
    DeriveJsonEncoder.gen[CLEAR_DESTINATIONS]
  implicit val decoder: JsonDecoder[CLEAR_DESTINATIONS] =
    DeriveJsonDecoder.gen[CLEAR_DESTINATIONS]
}

case class APPLY_VECTOR(id: ID, vec: (Double, Double, Double))
    extends SimpleCommandSerializable[WorldBlock.Block] {
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

case class GET_INPUT_VECTOR(id: ID) extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      resraw <- glob match {
        case physicalEntity: PhysicalEntity => physicalEntity.getInputVec()
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

case class SET_LV(id: GLOBZ_ID, lv: (Double, Double, Double))
    extends SimpleCommandSerializable[WorldBlock.Block] {
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

case class PhysicalStats(speed_delta: Double)
object PhysicalStats {
  implicit val encoder: JsonEncoder[PhysicalStats] =
    DeriveJsonEncoder.gen[PhysicalStats]
  implicit val decoder: JsonDecoder[PhysicalStats] =
    DeriveJsonDecoder.gen[PhysicalStats]
}
case class ADJUST_PHYSICAL_STATS(id: GLOBZ_ID, delta: PhysicalStats)
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      _ <- glob match {
        case pe: PhysicalEntity => pe.adjustMaxSpeed(delta.speed_delta)
      }
    } yield ())
      .orElseFail(GenericCommandError(s"Error adjusting speed for $id"))
}
object ADJUST_PHYSICAL_STATS {
  implicit val encoder: JsonEncoder[ADJUST_PHYSICAL_STATS] =
    DeriveJsonEncoder.gen[ADJUST_PHYSICAL_STATS]
  implicit val decoder: JsonDecoder[ADJUST_PHYSICAL_STATS] =
    DeriveJsonDecoder.gen[ADJUST_PHYSICAL_STATS]
}

case class GET_PHYSICAL_STATS(id: GLOBZ_ID)
    extends ResponseQuery[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    (for {
      glob <- WorldBlock.getBlob(id).flatMap(ZIO.fromOption(_))
      res <- glob match {
        case pe: PhysicalEntity => pe.getMaxSpeed()
      }
    } yield PhysStat(id, res))
      .orElseFail(GenericCommandError(s"Error getting phys_stats for $id "))
}
object GET_PHYSICAL_STATS {
  implicit val encoder: JsonEncoder[GET_PHYSICAL_STATS] =
    DeriveJsonEncoder.gen[GET_PHYSICAL_STATS]
  implicit val decoder: JsonDecoder[GET_PHYSICAL_STATS] =
    DeriveJsonDecoder.gen[GET_PHYSICAL_STATS]
}

case class ADD_TERRAIN(id: String, location: Vector[Double])
    extends SimpleCommandSerializable[WorldBlock.Block] {
  override def run: ZIO[WorldBlock.Block, CommandError, Unit] =
    for {
      t <- WorldBlock.getTerrain.mapError(_ => ???)
      _ <- t.add_terrain(id, location).mapError(_ => ???)
    } yield ()
}
object ADD_TERRAIN {
  implicit val encoder: JsonEncoder[ADD_TERRAIN] =
    DeriveJsonEncoder.gen[ADD_TERRAIN]
  implicit val decoder: JsonDecoder[ADD_TERRAIN] =
    DeriveJsonDecoder.gen[ADD_TERRAIN]
}

case class GET_ALL_TERRAIN() extends ResponseQuery[WorldBlock.Block]:
  override def run: ZIO[WorldBlock.Block, CommandError, QueryResponse] =
    for {
      res <- WorldBlock.getTerrain.flatMap(_.get_terrain()).mapError(_ => ???)
    } yield ??? // TerrainSet(res.map(_.serialize).toSet)

case class CONSOLE(
  execute_as: GLOBZ_ID,
  cmd: SerializableCommand[CONSOLE_ENV, Any]
) extends ResponseQuery[CONSOLE_ENV] {
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
