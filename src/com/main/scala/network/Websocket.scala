package network

import controller.SerializableCommand.CommandError
import controller.AuthCommandService
import controller.BasicController
import controller.Blob
import controller.CREATE_GLOB
import controller.Control
import controller.GET_ALL_GLOBS
import controller.GET_BLOB
import controller.GET_GLOB_LOCATION
import controller.Query
import controller.QueryResponse
import controller.ResponseQuery
import controller.SUBSCRIBE
import controller.SerializableCommand
import controller.SimpleCommand
import controller.SimpleCommandSerializable
import controller.SocketSubscribe
import controller.Subscription
import controller.auth.AUTH
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

object WebSocketAdvanced extends ZIOAppDefault {
  //handles auth map and controller

  val config =
    Server.defaultWith(_.webSocketConfig(WebSocketConfig.default.copy(subprotocols = Some("json"))))

  override val run = program().provide(ZLayer.succeed(WebSocketServerBasic))
  def program(): ZIO[WebSocketServer.Service, Nothing, Unit] =
    (for {
      _ <- Console.printLine(GET_ALL_GLOBS().toJson)
      wss <- ZIO.service[WebSocketServer.Service].flatMap(_.make)
      _ <- Server.serve(wss.app).provide(config)
    } yield ()).mapError(_ => ???)
}

case class BasicWebSocket(
  id: String,
  authMap: Ref[SESSION_MAP],
  controller: BasicController[Globz.Service with WorldBlock.Block],
  authenticated: Ref[Boolean],
  server_keys: Set[String],
  auth: AUTH[String]
) extends WebSocketControlServer[Any] {

  def handleCommand(
    command: SerializableCommand[Globz.Service with WorldBlock.Block, Unit]
  ): ZIO[Any, Nothing, Unit] = controller.runCommand(command.run.mapError(_ => ???)).map(_ => ())

  def handleQuery(
    query: ResponseQuery[Globz.Service with WorldBlock.Block]
  ): ZIO[Any, Nothing, String] =
    (for {
      res <- controller.runQuery(query.run.mapError(_ => ???))
    } yield res.toJson)

  val initialize_session: ZIO[Any, Object, Unit] =
    (for {
      _ <- Console.printLine(s"authenticating session for $id")
      glob <- controller
        .runQuery(GET_BLOB(id).run)
        .map {
          case Blob(blob) => blob
        }
        .fold(_ => None, x => x)
      _ <- (for {
        _ <- Console.printLine(s"No blob found for $id creating new one")
        randx <- Random.nextIntBetween(-10, 10)
        randz <- Random.nextIntBetween(-10, 10)
        _ <- controller.runCommand(CREATE_GLOB(id, Vector(0 + randx, 5, 0 + randz)).run)
        _ <- Console.printLine(s"blob successfully created for $id")
      } yield ()).when(glob.isEmpty && !server_keys.contains(id))
    } yield ())

  val parse_message: (String) => ZIO[Any, Nothing, SerializableCommand[_, _]] =
    text =>
      ZIO
        .fromEither(text.fromJson[SerializableCommand[_, _]])
        .flatMapError(err =>
          Console
            .printLine(s"Error processing command $text, error: $err")
            .fold(_ => ???, x => ???)
        )
  val authenticateMsg: SerializableCommand[_, _] => ZIO[Any, Nothing, Boolean] =
    cmd =>
      auth(cmd)
        .provide(ZLayer.succeed(id))
        .flatMapError(err => Console.printLine(s"bad auth: $err").mapError(_ => ???))
        .fold(_ => true, x => x)

  def handle_request(command: SerializableCommand[_, _]): ZIO[WebSocketChannel, Object, Unit] =
    command match {
      case op: SUBSCRIBE =>
        ZIO
          .serviceWithZIO[WebSocketChannel](channel =>
            controller.runCommand(SocketSubscribe(channel, op).run)
          )
          .map(_ => ())

      case c: SimpleCommandSerializable[Globz.Service with WorldBlock.Block] =>
        handleCommand(c)

      case rq: ResponseQuery[Globz.Service with WorldBlock.Block] =>
        for {
          channel <- ZIO.service[WebSocketChannel]
          res <- handleQuery(rq)
          _ <- channel.send(Read(WebSocketFrame.text(res)))
          //_ <- Console.printLine(s"Query results: $res")
        } yield ()
      case _ => ZIO.unit
    }
  def recieveAllText(
    text: String,
    channel: WebSocketChannel,
    initializing: Boolean = false
  ): ZIO[Any, Object, Unit] = {
    println(s"Received Text ${text}")
    text match {
      case _ if initializing =>
        initialize_session
      case "end" =>
        channel.shutdown
      case text =>
        (for {
          msg <- parse_message(text)
          authorized <- authenticateMsg(msg).debug
          _ <- if (authorized) handle_request(msg).provide(ZLayer.succeed(channel)) else ZIO.unit
          //_ <- handle_request(msg).provide(ZLayer.succeed(channel)).debug
        } yield ()).fold(err => println(s"error processing cmd $text :  $err"), x => x)
    }
  }

  def recieveAll(channel: WebSocketChannel, text: String) =
    authenticated.get.flatMap(authd =>
      if (authd) {
        recieveAllText(text, channel)
      } else
        (for {
          sentSecret <- ZIO
            .fromEither(text.fromJson[Session])
            .flatMapError(_ => Console.printLine(s"error decoding text: $text").mapError(_ => ???))
          secretOp <- authMap.get.map(_.get(id))
          secret <- ZIO
            .fromOption(secretOp)
            .flatMapError(err =>
              Console.printLine(s"Error retrieving secret for $id, cause: $err").mapError(_ => ???)
            )
          _ <- authenticated.update(_ => secret == sentSecret)
          verified <- authenticated.get
          _ <- if (verified) for {
            _ <- Console.printLine("Verified succeeded")
            _ <- authenticated.update(_ => true)
            _ <- recieveAllText(
              text,
              channel,
              true
            )
          } yield ()
          else
            Console
              .printLine(
                s"Could not authenticate: $secret,$sentSecret, $id, $authMap"
              ) *> channel.shutdown
        } yield ()).flatMapError(_ => channel.shutdown)
    )

  override def socket(authenticated: Boolean): WebSocketApp[Any] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case Read(WebSocketFrame.Text(text)) =>
          recieveAll(channel, text).mapError(_ => ???)
        case UserEventTriggered(UserEvent.HandshakeTimeout) =>
          ZIO.succeed(println("handshake timeout"))
        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          channel.send(Read(WebSocketFrame.text("Greetings!")))
        case Read(WebSocketFrame.Close(status, reason)) =>
          Console.printLine("Closing channel with status: " + status + " and reason: " + reason)
        case ExceptionCaught(cause) =>
          Console.printLine(s"Channel error!: ${cause.getMessage}")

      }
    }
}

object BasicWebSocket extends WebSocketControlServer.Service[Any] {

  override def make(authID: AUTH_ID): ZIO[BasicController[
    Globz.Service with WorldBlock.Block
  ] with Ref[SESSION_MAP], Nothing, WebSocketControlServer[Any]] =
    for {
      controller <- ZIO.service[BasicController[Globz.Service with WorldBlock.Block]]
      sessions <- ZIO.service[Ref[SESSION_MAP]]
      authd <- Ref.make(false)
    } yield BasicWebSocket(
      authID,
      sessions,
      controller,
      authd,
      Set("1"),
      AuthCommandService.all(Set("1"))
    )
}
