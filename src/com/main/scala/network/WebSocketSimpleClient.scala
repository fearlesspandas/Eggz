package network

package example

import zio.*
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http.*
import zio.http.netty.NettyConfig

object WebSocketSimpleClient extends ZIOAppDefault {

  val url = "ws://127.0.0.1:8081"

  val echo_test = (channel: WebSocketChannel) =>
    channel
      .send(Read(WebSocketFrame.text("Echo:hello")))
      .repeat(Schedule.spaced(Duration.fromMillis(1000)))

  val physics_test = (channel: WebSocketChannel) =>
    channel.send(
      Read(
        WebSocketFrame.text(
          """{
            |"type":"SET_GLOB_LOCATION",
            |"body":{
            | "id":"test",
            | "location":[1.0,1.2,0.0]
            | }
            |}""".stripMargin
        )
      )
    ) *> channel.send(
      Read(
        WebSocketFrame.text(
          """ {
            | "type":"GET_GLOB_LOCATION",
            | "body":{"id":"test"}
            |}""".stripMargin
        )
      )
    )
  val socketApp: WebSocketApp[Any] =
    Handler
      // Listen for all websocket channel events
      .webSocket { channel =>
        channel.receiveAll {

          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            echo_test(channel)
              .repeat(Schedule.spaced(Duration.fromMillis(1000)))
              .fork *> ZIO.log("channel connected")
          case ExceptionCaught(cause) => ZIO.log(cause.toString)
          case x =>
            ZIO.log(s"Other traffic $x")
        }
      }

  val app: ZIO[Client with Scope, Throwable, Response] =
    socketApp.connect(url) *> ZIO.never

  val config = (ZLayer.succeed(
    ZClient.Config.default.webSocketConfig(
      WebSocketConfig.default.copy(subprotocols = None)
    )
  ) ++
    DnsResolver.default
    ++ ZLayer.succeed(NettyConfig.defaultWithFastShutdown)) >>> ZClient.live
  val run: ZIO[Any, Throwable, Any] = // ZIO.log("why is this firing")
    app.provide(config, Scope.default)

}
