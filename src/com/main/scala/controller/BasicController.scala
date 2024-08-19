package controller

import implicits.*
import entity.BasicPlayer
import entity.WorldBlock
import entity.WorldBlockInMem
import src.com.main.scala.entity.Globz
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
  def make
    : ZIO[Service[DEFINED], ControllerError, BasicController[DEFINED, Queue[
      QueryResponse
    ]]] =
    ZIO.service[Service[DEFINED]].flatMap(_.make)
}

case class Control(
  glob: Globz.Service,
  worldBlock: Ref[WorldBlock.Block],
  server_response_queue: Queue[QueryResponse]
) extends BasicController[Globz.Service with WorldBlock.Block, Queue[
      QueryResponse
    ]] {
  override def runCommand[E](
    comm: ZIO[Globz.Service with WorldBlock.Block, E, Unit]
  ): ZIO[Any, E, BasicController[Globz.Service with WorldBlock.Block, Queue[
    QueryResponse
  ]]] =
    for {
      wb <- worldBlock.get
      _ <- comm.provide(ZLayer(ZIO.succeed(glob)) ++ ZLayer {
        ZIO.succeed(wb)
      })
    } yield this

  override def runQuery[Q, E](
    query: ZIO[Globz.Service with WorldBlock.Block, E, Q]
  ): ZIO[Any, E, Q] =
    worldBlock.get.flatMap(wb =>
      query.provide(ZLayer(ZIO.succeed(glob)) ++ ZLayer {
        ZIO.succeed(wb)
      })
    )

  override def runProcess[Q, E](
    query: ZIO[
      Globz.Service with WorldBlock.Block,
      E,
      ZIO[Globz.Service with WorldBlock.Block, E, Q]
    ]
  ): ZIO[Any, E, BasicController[Globz.Service with WorldBlock.Block, Queue[
    QueryResponse
  ]]] =
    for {
      world <- worldBlock.get
      op <- query.provide(ZLayer.succeed(world) ++ ZLayer.succeed(glob))
      _ <- op.provide(ZLayer.succeed(world) ++ ZLayer.succeed(glob))
    } yield this

  override def queueQuery[Q, E](
    query: ZIO[Globz.Service with WorldBlock.Block, E, Q]
  ): ZIO[Any, E, Unit] =
    for {
      res <- runQuery[Q, E](query).map{case response : QueryResponse => response}
      _ <- server_response_queue.offer(res)
    } yield ()

  override def getQueue[E]: ZIO[Any, E, Queue[QueryResponse]] =
    ZIO.succeed(server_response_queue)
}

object Control
    extends BasicController.Service[Globz.Service with WorldBlock.Block] {
  override def make: IO[ControllerError, BasicController[
    Globz.Service with WorldBlock.Block,
    Queue[QueryResponse]
  ]] =
    for {
      w <- WorldBlock.make
        .provide(ZLayer.succeed(WorldBlockInMem))
        .mapError(err => GenericControllerError(err.toString))
      r <- Ref.make(w)
      queue <- Queue.unbounded[QueryResponse]
    } yield Control(BasicPlayer, r, queue)
}
