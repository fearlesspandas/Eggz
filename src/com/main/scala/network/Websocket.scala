package network

import controller.BasicController
import controller.CREATE_GLOB
import controller.Command
import controller.Control
import controller.GET_ALL_GLOBS
import controller.Query
import controller.SimpleCommand
import entity.WorldBlock
import src.com.main.scala.entity.Globz
import zio._
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http._
import zio.http.codec.PathCodec.string
import zio.json.DecoderOps
import zio.json.EncoderOps

object WebSocketAdvanced extends ZIOAppDefault {

  def app(socketApp: String => WebSocketControlServer[Any]) =
    Routes(
      Method.GET / "greet" / string("name") -> handler { (name: String, req: Request) =>
        Response.text(s"Greetings ${name}!")
      },
      Method.GET / "connect" / string("id") -> handler { (id: String, req: Request) =>
        socketApp(id).socket.toResponse
      }
    ).toHttpApp

  val config =
    Server.defaultWith(_.webSocketConfig(WebSocketConfig.default.copy(subprotocols = Some("json"))))
  override val run = program().provide(ZLayer.succeed(BasicWebSocket))
  def program(): ZIO[WebSocketControlServer.Service[Any], Nothing, Unit] =
    (for {
      _ <- Console.printLine(GET_ALL_GLOBS().toJson)
      ws <- ZIO.service[WebSocketControlServer.Service[Any]].flatMap(_.make)
      _ <- Server.serve(app(ws)).provide(config)
    } yield ()).mapError(_ => ???) //.fold(e => Console.printLine(e),x => x)
}

trait WebSocketControlServer[Env] {
  def socket: WebSocketApp[Env]
}

trait WebsocketError
object WebSocketControlServer {
  trait Service[Env] {
    def make: IO[WebsocketError, String => WebSocketControlServer[Env]]
  }
}

case class BasicWebSocket(
  id: String,
  controller: BasicController[Globz.Service with WorldBlock.Block]
) extends WebSocketControlServer[Any] {

  def handleCommand(
    command: Command[Globz.Service with WorldBlock.Block, Unit]
  ): ZIO[Any, Nothing, Unit] =
    (for {
      _ <- controller.runCommand(command.run.mapError(_ => null.asInstanceOf[Nothing]))
    } yield ())

  def handleQuery(
    query: Command[Globz.Service with WorldBlock.Block, Any]
  ): ZIO[Any, Nothing, String] =
    (for {
      res <- controller.runQuery(query.run.mapError(_ => null.asInstanceOf[Nothing]))
    } yield res.toString)

  override def socket: WebSocketApp[Any] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case Read(WebSocketFrame.Text("end")) =>
          channel.shutdown

        case Read(WebSocketFrame.Text(text)) =>
          (for {
            _ <- Console.printLine("received text:" + text)
            _ <- ZIO
              .fromEither(text.fromJson[Command[_, _]])
              .fold(
                err => println(s"we fucked up:$text:" + err)
//                  channel.send(
//                    Read(WebSocketFrame.text(s"ERROR PERFORMING COMMAND : $text  ERROR: $err"))
//                  )
                ,
                x => x
              )
              .flatMap {
                case c: SimpleCommand[Globz.Service with WorldBlock.Block] =>
                  handleCommand(c)
                    .flatMap(_ => channel.send(Read(WebSocketFrame.text(s"FinishedCommand:$text"))))
                    .flatMap(_ => channel.send(Read(WebSocketFrame.text(text))))
                case c: Query[Globz.Service with WorldBlock.Block, _] =>
                  handleQuery(c).flatMap(res => channel.send(Read(WebSocketFrame.text(res))))
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

        case _ =>
          ZIO.unit
      }
    }
}

object BasicWebSocket extends WebSocketControlServer.Service[Any] {
  override def make: IO[WebsocketError, String => WebSocketControlServer[Any]] =
    for {
      controller <- BasicController.make
        .provide(ZLayer.succeed(Control))
        .mapError(_ => ???)
    } yield BasicWebSocket(_, controller)
}
