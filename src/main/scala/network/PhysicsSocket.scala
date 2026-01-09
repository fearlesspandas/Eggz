package network

import entity.Globz.GLOBZ_ID
import zio.*
import zio.json.*
import zio.http.*
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.stream.ZStream
import entity.PhysicalEntity
import entity.WorldBlock
import network.PhysicsChannel.PHYSICS_COMMAND
import physics.PhysicsCommand
import physics.PhysicsData
import physics.PhysicsTeleport
import physics.SetInputLock
trait PhysicsChannel {
  val poll_interval:Long
  def get_queue(): IO[PhysicsChannelError, Queue[PHYSICS_COMMAND]]
  def add_to_queue(cmd: PHYSICS_COMMAND): UIO[Unit]
  def start_queue_stream(): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      queue <- get_queue()
      _ <- ZStream
        .fromQueue(queue)
        .foreach {
          case SetInputLock(id, value) =>
            if (value) { lock_input(id) }
            else unlock_input(id)

          case PhysicsTeleport(id, location) => set_location(id, location)
        }
        .fork
    } yield ()
  def loop(interval: Long): ZIO[WebSocketChannel, PhysicsChannelError, Long]
  def send(
    msg: String
  ): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      channel <- ZIO.service[WebSocketChannel]
      _ <- channel
        .send(Read(WebSocketFrame.text(msg)))
        .mapError(err =>
          FailedSend(s"Error while sending to physics server : $err")
        )
    } yield ()

  def set_location(
    id: String,
    loc: (Double, Double, Double)
  ): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      _ <- send(
        s""" {"type":"SET_LOC", "body":{"id": $id,"vec":[${loc._1},${loc._2},${loc._3}]}} """
      )
        .mapError(err =>
          FailedSend(s"Error while sending to physics server : $err")
        )
    } yield ()

  def get_location(
    id: String
  ): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      _ <- send(s""" {"type":"GET_LOC", "body":{"id": "$id"}} """)
        .mapError(err =>
          FailedSend(s"Error while sending to physics server : $err")
        )
    } yield ()

  def lock_input(id: Int): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    send(s"""{"type":"LOCK_INPUT","body":{"id":$id}}""")

  def unlock_input(
    id: Int
  ): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    send(s"""{"type":"UNLOCK_INPUT","body":{"id":$id}}""")

  def send_noop(): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
    for {
      channel <- ZIO.service[WebSocketChannel]
      _ <- channel
        .send(Read(WebSocketFrame.ping))
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
            case Read(WebSocketFrame.Binary(bytes)) => ZIO.log("bytes_received")
            case Read(WebSocketFrame.Text(txt)) => ZIO.log(s"text received $txt")
            case UserEventTriggered(UserEvent.HandshakeComplete) =>
              (for {
                blobs <- wb.getAllBlobs().mapError(_ => ???)
                _ <- start_queue_stream()
                  .provide(ZLayer.succeed(channel))
                  .foldZIO(
                    err => ZIO.logError(s"Error processing queue stream $err"),
                    ZIO.succeed(_)
                  )
                xx <- this
                  .loop(this.poll_interval)
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
  type PHYSICS_COMMAND = PhysicsCommand
  val get_url: IO[PhysicsChannelError, String] =
    System
      .env("PHYSICS_ADDR")
      .flatMap(ZIO.fromOption(_))
      .mapError(err => PhysicsAddrNotFound())
  def make: ZIO[WorldBlock.Block, PhysicsChannelError, PhysicsChannel] =
    for {
      queue <- Queue.unbounded[PHYSICS_COMMAND]
      wb <- ZIO.service[WorldBlock.Block]
      poll_interval <- System
        .env("PHYSICS_SOCKET_UPDATE_INTERVAL")
        .flatMap(ZIO.fromOption(_))
        .fold(err => 1000,x => x.toInt)
    } yield BasicPhysicsChannel( queue, wb,poll_interval)
}
case class BasicPhysicsChannel(
  cmd_queue: Queue[PHYSICS_COMMAND],
  worldBlock: WorldBlock.Block,
  val poll_interval: Long,
) extends PhysicsChannel {


  def loop(interval: Long): ZIO[WebSocketChannel, PhysicsChannelError, Long] =
    send_noop() 
      .repeat(Schedule.spaced(Duration.fromMillis(interval)))
      .mapError(err => FailedSend(s"Error inside loop ${err.toString}"))

  override def get_queue(): IO[PhysicsChannelError, Queue[PHYSICS_COMMAND]] =
    ZIO.succeed(cmd_queue)

  override def add_to_queue(
    cmd: PHYSICS_COMMAND
  ): UIO[Unit] =
    cmd_queue.offer(cmd).unit

  override def start_socket(): IO[PhysicsChannelError, Unit] =
    start_socket_app.provide(ZLayer.succeed(worldBlock))
}
trait PhysicsChannelError
case class FailedSend(msg: String) extends PhysicsChannelError
case class NoIdsToTrack() extends PhysicsChannelError
case class PhysicsAddrNotFound() extends PhysicsChannelError
case class GenericPhysicsError(msg: String) extends PhysicsChannelError
