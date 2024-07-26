package network

package example

import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.*
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http.*
trait PhysicsChannel {
  def register(id: GLOBZ_ID): IO[PhysicsChannelError, Unit]

  def loop(interval: Long): ZIO[WebSocketChannel, PhysicsChannelError, Long]
  def send(msg: String): ZIO[WebSocketChannel, PhysicsChannelError, Unit] =
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
}
object PhysicsChannel {
  val url = "ws://127.0.0.1:8081"
}
case class BasicPhysicsChannel(
  tracked_ids: Ref[Set[GLOBZ_ID]],
  id_queue: Ref[Seq[GLOBZ_ID]]
) extends PhysicsChannel {

  override def register(id: GLOBZ_ID): IO[PhysicsChannelError, Unit] =
    for {
      _ <- tracked_ids.update(_ + id)
      _ <- id_queue.update(id +: _)
      _ <- ZIO.log(s"Id registered $id")
    } yield ()

  def loop(interval: Long): ZIO[WebSocketChannel, PhysicsChannelError, Long] =
    (for {
      q <- id_queue.get
      next_id <- ZIO
        .fromOption(q.headOption)
        .foldZIO(
          err =>
            for {
              ids <- tracked_ids.get
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
}
trait PhysicsChannelError
case class FailedSend(msg: String) extends PhysicsChannelError
case class NoIdsToTrack() extends PhysicsChannelError
