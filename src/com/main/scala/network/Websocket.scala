package network

import controller.BasicController
import controller.CREATE_GLOB
import controller.Command
import controller.Control
import controller.GET_ALL_GLOBS
import controller.GET_GLOB_LOCATION
import controller.Query
import controller.QueryResponse
import controller.ResponseQuery
import controller.SimpleCommand
import entity.WorldBlock
import network.WebSocketServer.AUTH_ID
import network.WebSocketServer.SECRET
import network.WebSocketServer.SESSION_MAP
import src.com.main.scala.entity.Globz
import zio.Ref
import zio._
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http._
import zio.http.codec.PathCodec.string
import zio.json.DecoderOps
import zio.json.EncoderOps
import zio.prelude.AssociativeBothCovariantOps

trait WebSocketServer {
  def app(): HttpApp[Any]
}
object WebSocketServer {
  type AUTH_ID = String
  type SECRET = Session
  type SESSION_MAP = Map[String, SECRET]

  trait Service {
    def make: IO[Nothing, WebSocketServer]
  }
}

case class WebSocketServerBasic(
  controller: BasicController[Globz.Service with WorldBlock.Block],
  authMap: Ref[SESSION_MAP] // maps id to secret that's expected on first connection
) extends WebSocketServer {
  //def app() = ???
//    HttpApp.collectZIO[Request] {
//
//    case Method.GET -> !! / "authenticate" / id =>
//      for {
//        _ <- authMap.update(_.updated(id, "SECRET"))
//        a <- authMap.get
//      } yield Response.text(s"set secret: $a")
//    case Method.GET -> !! / "connect" / id =>
//      BasicWebSocket
//        .make(id)
//        .flatMap(socketApp => socketApp.socket.toResponse)
//        .provide(ZLayer.succeed(controller) ++ ZLayer.succeed(authMap))
//  }

  def app() =
    Routes(
      Method.GET / "authenticate" / string("id") -> handler { (id: String, request: Request) =>
        authMap
          .update(_.updated(id, BasicSession(id, "SECRET")))
          .flatMap(_ => ZIO.succeed(Response.text(s"updated map:$authMap")))
//        ZIO.succeed(Response.text(id))
      },
      Method.GET / "connect" / string("id") -> handler { (id: String, request: Request) =>
        BasicWebSocket
          .make(id)
          .flatMap(sockerApp => sockerApp.socket(false).toResponse)
          .provide(ZLayer.succeed(controller) ++ ZLayer.succeed(authMap))
      }
    ).toHttpApp

}
object WebSocketServerBasic extends WebSocketServer.Service {
  override def make: IO[Nothing, WebSocketServer] =
    for {
      controller <- BasicController.make.provide(ZLayer.succeed(Control)).mapError(_ => ???)
      sessionmap <- Ref.make(Map.empty[String, SECRET])
    } yield WebSocketServerBasic(controller, sessionmap)
}
object WebSocketAdvanced extends ZIOAppDefault {
  //handles auth map and controller

  val config =
    Server.defaultWith(_.webSocketConfig(WebSocketConfig.default.copy(subprotocols = Some("json"))))
  override val run = program().provide(ZLayer.succeed(WebSocketServerBasic))
  def program(): ZIO[WebSocketServer.Service, Nothing, Unit] =
    (for {
      _ <- Console.printLine(GET_ALL_GLOBS().toJson)
      wss <- ZIO.service[WebSocketServer.Service].flatMap(_.make)
      _ <- Server.serve(wss.app()).provide(config)
    } yield ()).mapError(_ => ???) //.fold(e => Console.printLine(e),x => x)
}

trait WebSocketControlServer[Env] {
  def socket(authenticated: Boolean): WebSocketApp[Env]
}

trait WebsocketError
object WebSocketControlServer {
  trait Service[Env] {

    def make(
      authID: AUTH_ID
    ): ZIO[BasicController[Globz.Service with WorldBlock.Block] with Ref[SESSION_MAP], Nothing, WebSocketControlServer[
      Any
    ]]
  }
}

//A ->

case class BasicWebSocket(
  id: String,
  authMap: Ref[SESSION_MAP],
  controller: BasicController[Globz.Service with WorldBlock.Block],
  authenticated: Ref[Boolean]
) extends WebSocketControlServer[Any] {

  def handleCommand(
    command: Command[Globz.Service with WorldBlock.Block, Unit]
  ): ZIO[Any, Nothing, Unit] =
    (for {
      _ <- controller.runCommand(command.run.mapError(_ => null.asInstanceOf[Nothing]))
    } yield ())

  def handleQuery(
    query: ResponseQuery[Globz.Service with WorldBlock.Block]
  ): ZIO[Any, Nothing, String] =
    (for {
      res <- controller.runQuery(query.run.mapError(_ => null.asInstanceOf[Nothing]))
    } yield res.toJson)

  def handleQueryAsString(
    query: Command[Globz.Service with WorldBlock.Block, Any]
  ): ZIO[Any, Nothing, String] =
    (for {
      res <- controller.runQuery(query.run.mapError(_ => null.asInstanceOf[Nothing]))
    } yield res.toString)

  def recieveAllText(text: String, channel: WebSocketChannel) =
    text match {
      case "end" =>
        channel.shutdown
      case text =>
        (for {
          _ <- Console.printLine("received text:" + text)
          _ <- ZIO
            .fromEither(text.fromJson[Command[_, _]])
            .fold(
              err => println(s"we fucked up:$text:" + err),
              x => x
            )
            .flatMap {
              case c: SimpleCommand[Globz.Service with WorldBlock.Block] =>
                handleCommand(c)
              case rq: ResponseQuery[Globz.Service with WorldBlock.Block] =>
                handleQuery(rq).flatMap(res =>
                  channel.send(Read(WebSocketFrame.text(res))) *> ZIO
                    .succeed(println(s"Query results: $res"))
                )
              case c: Query[Globz.Service with WorldBlock.Block, _] =>
                handleQueryAsString(c).flatMap(res => channel.send(Read(WebSocketFrame.text(res))))
            }
            .flatMapError(err =>
              channel
                .send(Read(WebSocketFrame.text(s"ERROR PERFORMING COMMAND : $text  ERROR: $err")))
                .mapError(_ => ???)
            )
        } yield ())
          .fold(
            err => println(s"we fucked up:$text:" + err),
            x => x
          )

      case _ => ZIO.unit
    }

  def recieveAll(channel: WebSocketChannel, text: String) =
    authenticated.get.flatMap(authd =>
      if (authd) {
        recieveAllText(text, channel)
      } else
        for {
          sentSecret <- ZIO
            .fromEither(text.fromJson[Session])
            .flatMapError(_ => Console.printLine(s"error decoding text: $text").mapError(_ => ???))
          secretOp <- authMap.get.map(_.get(id))
          secret <- ZIO.fromOption(secretOp).fold(_ => BasicSession(id, "SECRET"), x => x)
          _ <- authenticated.update(_ => secret == sentSecret)
          verified <- authenticated.get
        } yield
          if (verified) recieveAllText(text, channel)
          else {
            println(s"Could not authenticate: $secret,$sentSecret, $id"); channel.shutdown
          }
    )

  override def socket(authenticated: Boolean): WebSocketApp[Any] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case Read(WebSocketFrame.Text(text)) if !authenticated =>
          recieveAll(channel, text).mapError(_ => ???)
        //          for {
//            res <- authMap.get.map(_.get(id).map(_ == text))
//          } yield receivAll(true, channel)
        case Read(WebSocketFrame.Text(text)) if authenticated =>
          recieveAll(channel, text).mapError(_ => ???)
        case UserEventTriggered(UserEvent.HandshakeTimeout) =>
          ZIO.succeed(println("handshake timeout"))
        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          channel.send(Read(WebSocketFrame.text("Greetings!")))
        // Log when the channel is getting closed
        case Read(WebSocketFrame.Close(status, reason)) =>
          Console.printLine("Closing channel with status: " + status + " and reason: " + reason)

        // Print the exception if it's not a normal close
        case ExceptionCaught(cause) =>
          Console.printLine(s"Channel error!: ${cause.getMessage}")

      }
    }
}

object BasicWebSocket extends WebSocketControlServer.Service[Any] {

  override def make(authID: AUTH_ID): ZIO[BasicController[
    Globz.Service with WorldBlock.Block
  ] with Ref[SESSION_MAP], Nothing, WebSocketControlServer[Any]] =
    ZIO
      .service[BasicController[Globz.Service with WorldBlock.Block]]
      .flatMap(controller =>
        ZIO
          .service[Ref[SESSION_MAP]]
          .flatMap(sessions =>
            Ref.make(false).map(authd => BasicWebSocket(authID, sessions, controller, authd))
          )
      )
//      .map(controller => )
}
