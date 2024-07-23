package network

package example

import zio._

import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http._

object WebSocketSimpleClient extends ZIOAppDefault {

  val url = "ws://127.0.0.1:8081"

  val socketApp: WebSocketApp[Any] =
    Handler
      // Listen for all websocket channel events
      .webSocket { channel =>
        channel.receiveAll {

          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            ZIO.log("channel connected")

          case Read(WebSocketFrame.Text("bar")) =>
            ZIO.succeed(println("Goodbye!")) *> channel.send(
              Read(WebSocketFrame.close(1000))
            )

          case _ =>
            ZIO.unit
        }
      }

  val app: ZIO[Client with Scope, Throwable, Response] =
    socketApp.connect(url) *> ZIO.never

  val run: ZIO[Any, Throwable, Any] = // ZIO.log("why is this firing")
    app.provide(Client.default, Scope.default)

}
