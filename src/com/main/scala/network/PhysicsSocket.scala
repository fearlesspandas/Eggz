package network

package example

import entity.PhysicalEntity
import entity.WorldBlock
import network.example.PhysicsChannel.PHYSICS_COMMAND
import physics.PhysicsCommand
import physics.SendLocation
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.*
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http.*
import zio.stream.ZStream
import zio.json.*
trait PhysicsChannel {
  def get_queue(): IO[PhysicsChannelError, Queue[PHYSICS_COMMAND]]
  def add_to_queue(cmd: PHYSICS_COMMAND): IO[PhysicsChannelError, Unit]
  def start_queue_stream(): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      queue <- get_queue()
      _ <- ZStream.fromQueue(queue).foreach(cmd => send(cmd)).fork
    } yield ()
//  def register(id: GLOBZ_ID): IO[PhysicsChannelError, Unit]
  def loop(interval: Long): ZIO[WebSocketChannel, PhysicsChannelError, Long]
  def send(
    msg: PHYSICS_COMMAND
  ): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      channel <- ZIO.service[WebSocketChannel]
      _ <- channel
        .send(Read(WebSocketFrame.text(msg)))
        .mapError(err =>
          FailedSend(s"Error while sending to physics server : $err")
        )
    } yield ()
  def get_location(
    id: String
  ): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      _ <- send(s""" {"type":"GET_GLOB_LOCATION", "body":{"id": "$id"}} """)
        .mapError(err =>
          FailedSend(s"Error while sending to physics server : $err")
        )
    } yield ()

    // todo fix bug where internal errors do not cause the socket to restart (possibly due to forking)
    // todo remove interval on loop and see how unthrottled processing handles
  val physics_socket: (WorldBlock.Block) => WebSocketApp[Any] =
    (wb: WorldBlock.Block) =>
      Handler
        .webSocket { channel =>
          channel.receiveAll {
            case Read(WebSocketFrame.Text(txt)) =>
              (for {
                r <- ZIO
                  .fromEither(txt.fromJson[PhysicsCommand])
                  .flatMapError(err =>
                    ZIO.log(s"Could not map $txt due to $err")
                  )
                  .map(_.asInstanceOf[SendLocation])
                _ <- wb.getBlob(r.id).flatMap(ZIO.fromOption(_)).flatMap {
                  case pe: PhysicalEntity =>
                    pe.teleport(r.loc)
                }
              } yield ()).foldZIO(
                err => ZIO.log(s"processing failed on $txt with err $err"),
                x => ZIO.succeed(x)
              )
            case UserEventTriggered(UserEvent.HandshakeComplete) =>
              (for {
                xx <- this
                  .loop(10)
                  .provide(ZLayer.succeed(channel))
                  .flatMapError(err =>
                    ZIO.log(s"Error while processing loop ${err.toString}")
                  )
                  .fork
              } yield ()) *> ZIO.log("Channel Connected")
            case ExceptionCaught(cause) =>
              ZIO.logError(s"Error while handling physics socket $cause")
                *> ZIO.fail(cause)
            //                *> channel.shutdowndd
            case x => ZIO.log(s"other traffic $x")
          }
        }
        .tapErrorZIO(err => ZIO.log("Found Error"))

  val physics_app
    : ZIO[Client & Scope & WorldBlock.Block, PhysicsChannelError, Nothing] =
    ZIO
      .service[WorldBlock.Block]
      .flatMap(wb =>
        PhysicsChannel.get_url
          .flatMap(
            physics_socket(wb).connect(_)
          )
          .mapError(err =>
            GenericPhysicsError(s"Error within physics server app $err")
          ) *> ZIO.never
      )

  val start_socket_app: ZIO[WorldBlock.Block, PhysicsChannelError, Unit] =
    for {
      _ <- ZIO.log("Starting physics socket app")
      wb <- ZIO.service[WorldBlock.Block]
      _ <- physics_app
        .provide(Client.default, Scope.default, ZLayer.succeed(wb))
        .mapError(err =>
          GenericPhysicsError(s"Error starting physics app $err")
        )
        .flatMapError(err => ZIO.log(err.toString))
        .retry(Schedule.spaced(Duration.fromMillis(1000)))
        .fork
    } yield ()

  def start_socket(): IO[PhysicsChannelError, Unit]
}

object PhysicsChannel {
  type PHYSICS_COMMAND = String
  val get_url: IO[PhysicsChannelError, String] =
    System
      .env("PHYSICS_ADDR")
      .flatMap(ZIO.fromOption(_))
      .mapError(err => PhysicsAddrNotFound())
  def make: ZIO[WorldBlock.Block, PhysicsChannelError, PhysicsChannel] =
    for {
//      tracked_ids <- Ref.make(Set.empty[GLOBZ_ID])
      id_queue <- Ref.make(Seq.empty[GLOBZ_ID])
      queue <- Queue.unbounded[PHYSICS_COMMAND]
      wb <- ZIO.service[WorldBlock.Block]
    } yield BasicPhysicsChannel(id_queue, queue, wb)
}
case class BasicPhysicsChannel(
//  tracked_ids: Ref[Set[GLOBZ_ID]],
  id_queue: Ref[Seq[GLOBZ_ID]],
  cmd_queue: Queue[PHYSICS_COMMAND],
  worldBlock: WorldBlock.Block
) extends PhysicsChannel {
//  override def register(id: GLOBZ_ID): IO[PhysicsChannelError, Unit] =
//    for {
//      _ <- tracked_ids.update(_ + id)
//      _ <- id_queue.update(id +: _)
//      _ <- ZIO.log(s"Id registered $id")
//    } yield ()

  def loop(interval: Long): ZIO[WebSocketChannel, PhysicsChannelError, Long] =
    (for {
      q <- id_queue.get
      next_id <- ZIO
        .fromOption(q.headOption)
        .foldZIO(
          err =>
            for {
              ids <- worldBlock.getAllBlobs().map(_.map(g => g.id))
              _ <- id_queue.update(_ => ids.toSeq)
              n <- ZIO.fromOption(ids.headOption).mapError(_ => ???)
            } yield n,
          x => ZIO.succeed(x)
        )
      _ <- get_location(next_id)
      _ <- id_queue.update(_.tail)
    } yield ())
      .repeat(Schedule.spaced(Duration.fromMillis(interval)))
      .mapError(err => FailedSend(s"Error inside loop ${err.toString}"))

  override def get_queue(): IO[PhysicsChannelError, Queue[PHYSICS_COMMAND]] =
    ZIO.succeed(cmd_queue)

  override def add_to_queue(
    cmd: PHYSICS_COMMAND
  ): IO[PhysicsChannelError, Unit] =
    cmd_queue.offer(cmd).unit

  override def start_socket(): IO[PhysicsChannelError, Unit] =
    start_socket_app.provide(ZLayer.succeed(worldBlock))
}
trait PhysicsChannelError
case class FailedSend(msg: String) extends PhysicsChannelError
case class NoIdsToTrack() extends PhysicsChannelError
case class PhysicsAddrNotFound() extends PhysicsChannelError
case class GenericPhysicsError(msg: String) extends PhysicsChannelError
