package network

package example

import zio._

import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http._

object WebSocketSimpleClient extends ZIOAppDefault {

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
    app.provide(Client.default, Scope.default)

}
