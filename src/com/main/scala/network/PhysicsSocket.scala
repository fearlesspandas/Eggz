package network

package example

import zio._

import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http._

trait PhysicsSocket{
  val url = "ws://192.168.50.27:8080/subscriptions"

  val socketApp: WebSocketApp[Any] =
    Handler
    // Listen for all websocket channel events
      .webSocket { channel =>
        channel.receiveAll {

          // Send a "foo" message to the server once the connection is established
          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            channel.send(
              Read(WebSocketFrame.text("{\"CREATE_GLOB\":{\"globId\":\"1\",\"location\":[0.0]}}"))
            ) *> channel.send(
              Read(
                WebSocketFrame.text("{\"CREATE_REPAIR_EGG\":{\"eggId\":\"1\",\"globId\":\"1\"}}")
              )
            ) *> channel.send(
              Read(
                WebSocketFrame.text("{\"GET_BLOB\":{\"id\":\"1\"}}")
              )
            )

          // Send a "bar" if the server sends a "foo"
          case Read(WebSocketFrame.Text(text)) =>
//            channel.send(Read(WebSocketFrame.text("bar")))
            Console.printLine("Server:" + text)
          // Close the connection if the server sends a "bar"
          case Read(WebSocketFrame.Text("bar")) =>
            ZIO.succeed(println("Goodbye!")) *> channel.send(Read(WebSocketFrame.close(1000)))

          case _ =>
            ZIO.unit
        }
      }

  val app: ZIO[Client with Scope, Throwable, Response] =
    socketApp.connect(url) *> ZIO.never

  val run: ZIO[Any, Throwable, Any] =
    app.provide(Client.default, Scope.default).fork

}

trait PhysicsChannel{
  def register(id:GlobzId):IO[PhysicsChannelError,Unit]
  private def get_channel():IO[PhysicsChannelError,WebSocketChannel]
  def send(msg:String):IO[PhysicsChannelError,Unit] = 
    for{
      channel <- get_channel()
      _ <- channel.send(Read(WebSocketFrame.text(msg)))
            .mapError(err => FailedSend(s"Error while sending to physics server : $err"))
    }yield ()

  def get_location(id:String):IO[PhysicsChannelError,Unit] = 
    for{
      channel <- get_channel()
      _ <- channel.send(Read(WebSocketFrame.text(s""" "type":"GET_LOCATION", "body":{"id":$id} """ )))
            .mapError(err => FailedSend(s"Error while sending to physics server : $err"))
    }yield()
}
case class BasicPhysicsChannel() extends PhysicsChannel{
  val tracked_ids:Ref[Set[GlobzId]]
  val id_queue:Ref[Seq[GlobzId]]
  def register(id:GlobzId) = 
    for{
      _ <- tracked_ids.update(id +: _)
    } yield ()
  def loop = 
    (for{
      q <- id_queue.get
      next_id <- ZIO.fromOption(q.headOption)
        .flatMapError(err => for{
          _ <- id_queue.update(tracked_ids)
          n <- ZIO.fromOption(id_queue.headOption)
        }yield n)
      _ <- get_location(next_id) 
      _ <- id_queue.update(_.tail)
    } yield ()).repeat(Duration.Inf)
}
trait PhysicsChannelError
case class FailedSend(msg:String) extends PhysicsChannelError
