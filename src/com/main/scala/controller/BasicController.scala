package controller

import controller.Control.CONTROLLER_ENV
import implicits.*
import entity.BasicPlayer
import entity.ProgressData
import entity.WorldBlock
import entity.WorldBlockInMem
import network.PhysicsChannel
import physics.PhysicsCommand
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.Chunk
import zio.IO
import zio.Queue
import zio.Ref
import zio.ZIO
import zio.ZLayer
object implicits {

  implicit def tupleToEnv(
    x: (WorldBlock.Block, Globz.Service)
  ): Globz.Service with WorldBlock.Block = ???
}
trait BasicController[Env, Queued] {
  def runCommand[E](
    comm: ZIO[Env, E, Unit]
  ): ZIO[Any, E, BasicController[Env, Queued]]
  def runQuery[Q, E](query: ZIO[Env, E, Q]): ZIO[Any, E, Q]

  def queueQuery[Q, E](query: ZIO[Env, E, Q]): ZIO[Any, E, Unit]

  def getQueue[E]: ZIO[Any, E, Queued]

  def getClientQueue[E](id: GLOBZ_ID): ZIO[Any, E, Option[Queued]]

  def addClientQueue[E](id: GLOBZ_ID): ZIO[Any, E, Unit]
//  def broadcast[E]: ZIO[Any, E, Queued]

  def runProcess[Q, E](
    query: ZIO[Env, E, ZIO[Env, E, Q]]
  ): ZIO[Any, E, BasicController[Env, Queued]]
}

trait ControllerError
case class GenericControllerError(msg: String) extends ControllerError
object BasicController {
  type COMMAND = String
  type QUERY = String

  trait Service[Env] {
    def make: IO[ControllerError, BasicController[Env, Queue[QueryResponse]]]
  }
  type DEFINED = Globz.Service with WorldBlock.Block
  def make: ZIO[Service[CONTROLLER_ENV], ControllerError, BasicController[
    CONTROLLER_ENV,
    Queue[
      QueryResponse
    ]
  ]] =
    ZIO.serviceWithZIO[Service[CONTROLLER_ENV]](_.make)
}

case class Control(
  glob: Globz.Service,
  worldBlock: Ref[WorldBlock.Block],
  progress: ProgressData,
  server_response_queue: Queue[QueryResponse],
  client_response_queues: Ref[Map[GLOBZ_ID, Queue[QueryResponse]]],
  physics_channel: PhysicsChannel
) extends BasicController[CONTROLLER_ENV, Queue[
      QueryResponse
    ]] {
  override def runCommand[E](
    comm: ZIO[CONTROLLER_ENV, E, Unit]
  ): ZIO[Any, E, BasicController[CONTROLLER_ENV, Queue[
    QueryResponse
  ]]] =
    for {
      wb <- worldBlock.get
      _ <- comm.provide(ZLayer(ZIO.succeed(glob)) ++ ZLayer {
        ZIO.succeed(wb)
      } ++ ZLayer.succeed(progress))
    } yield this

  override def runQuery[Q, E](
    query: ZIO[CONTROLLER_ENV, E, Q]
  ): ZIO[Any, E, Q] =
    worldBlock.get.flatMap(wb =>
      query.provide(ZLayer(ZIO.succeed(glob)) ++ ZLayer {
        ZIO.succeed(wb)
      } ++ ZLayer.succeed(progress))
    )
  @deprecated
  override def runProcess[Q, E](
    query: ZIO[
      CONTROLLER_ENV,
      E,
      ZIO[CONTROLLER_ENV, E, Q]
    ]
  ): ZIO[Any, E, BasicController[CONTROLLER_ENV, Queue[
    QueryResponse
  ]]] =
    for {
      world <- worldBlock.get
      op <- query.provide(
        ZLayer.succeed(world) ++ ZLayer.succeed(glob) ++ ZLayer.succeed(
          progress
        )
      )
      _ <- op.provide(
        ZLayer.succeed(world) ++ ZLayer.succeed(glob) ++ ZLayer.succeed(
          progress
        )
      )
    } yield this

  override def queueQuery[Q, E](
    query: ZIO[CONTROLLER_ENV, E, Q]
  ): ZIO[Any, E, Unit] =
    for {
      res <- runQuery[Q, E](query).flatMap {
        case QueuedClientMessage(id, messages) =>
          client_response_queues.get
            .map(_.get(id))
            .flatMap(ZIO.fromOption(_))
            .foldZIO(
              _ => ZIO.logError(s"could not find client queue ${id}"),
              q => ZIO.foreach(messages)(q.offer(_))
            )
        case QueuedClientBroadcast(messages) =>
          client_response_queues.get
            .map(_.values)
            .flatMap(
              ZIO.foreachPar(_)(q =>
                ZIO.foreach(messages)(message => q.offer(message))
              )
            )
        case QueuedServerMessage(messages) =>
          ZIO.foreachPar(messages)(server_response_queue.offer(_))
        case QueuedPhysicsMessage(messages) =>
          ZIO.foreach(messages)(physics_channel.add_to_queue)
      }
    } yield ()

  override def getQueue[E]: ZIO[Any, E, Queue[QueryResponse]] =
    ZIO.succeed(server_response_queue)

  override def addClientQueue[E](id: GLOBZ_ID): ZIO[Any, E, Unit] = Queue
    .unbounded[QueryResponse]
    .flatMap(q =>
      client_response_queues
        .update(_.updated(id, q))
    )
    .whenZIO(client_response_queues.get.map(!_.keySet.contains(id)))
    .unit

  override def getClientQueue[E](
    id: GLOBZ_ID
  ): ZIO[Any, E, Option[Queue[QueryResponse]]] =
    client_response_queues.get.map(_.get(id))
}

object Control extends BasicController.Service[CONTROLLER_ENV] {
  type CONTROLLER_ENV = Globz.Service with WorldBlock.Block with ProgressData
  override def make: IO[ControllerError, BasicController[
    CONTROLLER_ENV,
    Queue[QueryResponse]
  ]] =
    for {
      w <- WorldBlock.make
        .provide(ZLayer.succeed(WorldBlockInMem))
        .mapError(err => GenericControllerError(err.toString))
      r <- Ref.make(w)
      queue <- Queue.unbounded[QueryResponse]
      client_queues <- Ref.make(Map.empty[GLOBZ_ID, Queue[QueryResponse]])
      pc <- PhysicsChannel.make.provide(ZLayer.succeed(w)).mapError(_ => ???)
      progress <- ProgressData.make
      _ <- ZIO.log("Attempting to start physics socket")
      _ <- pc.start_socket().mapError(_ => ???)
      _ <- ZIO.log("Physics Socket Started")
    } yield Control(BasicPlayer, r, progress, queue, client_queues, pc)
}
