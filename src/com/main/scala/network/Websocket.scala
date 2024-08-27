package network

import controller.SerializableCommand.CommandError
import controller.AuthCommandService
import controller.AuthenticationService
import controller.BasicController
import controller.Blob
import controller.CONSOLE
import controller.CREATE_GLOB
import controller.Completed
import controller.Control
import controller.Control.CONTROLLER_ENV
import controller.GET_ALL_GLOBS
import controller.GET_BLOB
import controller.GET_GLOB_LOCATION
import controller.MultiResponse
import controller.PaginatedResponse
import controller.Query
import controller.QueryResponse
import controller.QueuedClientMessage
import controller.QueuedPhysicsMessage
import controller.QueuedServerMessage
import controller.ResponseQuery
import controller.SUBSCRIBE
import controller.SerializableCommand
import controller.SimpleCommandSerializable
import controller.SocketSubscribe
import controller.StartPagination
import controller.Subscription
import controller.auth.AUTH
import entity.WorldBlock
import network.WebSocketServer.AUTH_ID
import network.WebSocketServer.SECRET
import network.WebSocketServer.SERVER_IDS
import network.WebSocketServer.SESSION_MAP
import src.com.main.scala.entity.Globz
import zio.Ref
import zio.*
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.Read
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http.*
import zio.http.Status.Continue
import zio.http.WebSocketFrame.Continuation
import zio.http.codec.PathCodec.string
import zio.json.DecoderOps
import zio.json.EncoderOps
import zio.prelude.AssociativeBothCovariantOps
import zio.profiling.sampling.*
import zio.profiling.causal.*
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
object WebSocketAdvanced extends ZIOAppDefault {
  // handles auth map and controller

  val config =
    Server.defaultWith(
      _.webSocketConfig(
        WebSocketConfig.default.copy(subprotocols = Some("json"))
      )
    )

  override val run =
//    CausalProfiler(10)
//      .profile(
    program()
      .provide(ZLayer.succeed(WebSocketServerBasic))
      .map(x => ???)
//      )
//      .flatMap(_.renderToFile("profile.causal"))
  def program(): ZIO[WebSocketServer.Service, WebSocketError, Unit] =
    (for {
      _ <- ZIO.logInfo("server started")
      wss <- ZIO.service[WebSocketServer.Service].flatMap(_.make)
      _ <- Server.serve(wss.app).provide(config)
    } yield ()).mapError(err => GenericWebsocketError(err.toString))
}

case class BasicWebSocket(
  id: String,
  authMap: Ref[SESSION_MAP],
  controller: BasicController[CONTROLLER_ENV, Queue[QueryResponse]],
  authenticated: Ref[Boolean],
  server_keys: Set[String],
  auth: AUTH[String],
  response_queue: Queue[QueryResponse]
) extends WebSocketControlServer[Any] {

  def linkControllerServer(channel: WebSocketChannel) =
    (for {
      q <-
        controller.getQueue
      _ <- ZStream
        .fromQueue(q)
        .foreach {
          case x: QueuedClientMessage => q.offer(x)
          case response =>
            channel.send(Read(WebSocketFrame.text(response.toJson)))
        }
        .fork
    } yield ()).when(server_keys.contains(id))

  def linkControllerClient(channel: WebSocketChannel) =
    (for {
      q <-
        controller.getQueue
      _ <- ZStream
        .fromQueue(q)
        .foreach {
          case QueuedClientMessage(id, responses) if id == this.id =>
            ZIO.foreach(responses) {
              case PaginatedResponse(grouped_responses) =>
                ZIO.foreach(grouped_responses)(send_response =>
                  channel.send(
                    Read(WebSocketFrame.text(send_response.toJson))
                  )
                )
              case response =>
                channel.send(Read(WebSocketFrame.text(response.toJson)))
            }
          case x => q.offer(x)
        }
        .fork
    } yield ()).when(!server_keys.contains(id))

  def handleCommand(
    command: SerializableCommand[CONTROLLER_ENV, Unit]
  ): ZIO[Any, Nothing, Unit] =
    controller
      .runCommand(
        command.run.foldZIO(
          err =>
            ZIO.log(s"Error while processing command $err") *> ZIO.succeed(()),
          x => ZIO.succeed(x)
        )
      )
      .unit

  def handleQuery(
    query: ResponseQuery[CONTROLLER_ENV]
  ): ZIO[Any, Nothing, Seq[String] | QueryResponse] =
    for {
      res <- controller.runQuery(
        query.run.foldZIO(
          err =>
            ZIO.log(s"Error while performing query $err") *> ZIO.succeed(
              QueryResponse.Empty
            ),
          x => ZIO.succeed(x)
        )
      )
    } yield res
//    match {
//      case x: QueuedServerMessage       => x
//      case x: Completed                 => x
//      case PaginatedResponse(responses) => responses.map(_.toJson)
//      case r                            => Seq(r.toJson)
//      case _                            => Seq()
//    }

  val initialize_session: ZIO[Any, Object, Unit] =
    for {
      _ <- Console.printLine(s"authenticating session for $id")
      glob <- controller
        .runQuery(GET_BLOB(id).run)
        .map { case Blob(blob) =>
          blob
        }
        .fold(_ => None, x => x)
      _ <- (for {
        _ <- Console.printLine(s"No blob found for $id creating new one")
        randx <- Random.nextIntBetween(-10, 10)
        randz <- Random.nextIntBetween(-10, 10)
        _ <- controller.runCommand(
          CREATE_GLOB(id, Vector(0 + randx, 5, 0 + randz)).run
        )
        _ <- Console.printLine(s"blob successfully created for $id")
      } yield ()).when(glob.isEmpty && !server_keys.contains(id))
    } yield ()

  val parse_message: (String) => ZIO[Any, Nothing, SerializableCommand[_, _]] =
    text =>
      ZIO
        .fromEither(text.fromJson[SerializableCommand[_, _]])
        .flatMapError(err =>
          Console
            .printLine(s"Error processing command $text, error: $err")
            .fold(_ => ???, x => ???)
        )

  val authorizeMsg: SerializableCommand[_, _] => ZIO[Any, Nothing, Boolean] =
    cmd =>
      auth(cmd)
        .provide(ZLayer.succeed(id))
        .flatMapError(err =>
          Console.printLine(s"bad auth: $err").mapError(_ => ???)
        )
        .fold(_ => false, x => x)

  def handle_query_response(
    channel: WebSocketChannel,
    res: QueryResponse
  ): ZIO[Any, Throwable, Unit] =
    res match {
      case MultiResponse(responses) =>
        ZIO
          .foreach(responses)(response =>
            handle_query_response(channel, response)
          )
          .unit
      case PaginatedResponse(responses) =>
        ZIO
          .foreach(responses)(response =>
            channel.send(Read(WebSocketFrame.text(response.toJson)))
          )
          .unit
      case QueuedPhysicsMessage(messages) =>
        ZIO
          .foreach(messages)(message =>
            controller.queueQuery(ZIO.succeed(message))
          )
          .unit
      case QueuedServerMessage(responses) =>
        ZIO
          .foreach(responses)(resp => controller.queueQuery(ZIO.succeed(resp)))
          .unit
      case x: QueuedClientMessage => controller.queueQuery(ZIO.succeed(x))
      case x: Completed =>
        for {
          next <- response_queue.takeUpTo(1)
          _ <- ZIO.foreach(next)(x =>
            channel.send(Read(WebSocketFrame.text(x.toJson)))
          )
        } yield ()
      case response => channel.send(Read(WebSocketFrame.text(response.toJson)))
    }

  def handle_request(
    command: SerializableCommand[_, _]
  ): ZIO[WebSocketChannel, Object, Unit] =
    command match {
      case op: SUBSCRIBE =>
        ZIO
          .serviceWithZIO[WebSocketChannel](channel =>
            controller.runCommand(SocketSubscribe(channel, op).run)
          )
          .map(_ => ())

      case c: SimpleCommandSerializable[CONTROLLER_ENV] =>
        handleCommand(c)

      case rq: ResponseQuery[CONTROLLER_ENV] =>
        for {
          channel <- ZIO.service[WebSocketChannel]
          res <- handleQuery(rq)
          _ <-
            res match {
              case q: QueryResponse => handle_query_response(channel, q)
              case s: Seq[String] =>
                ZIO.foreach(s)(txt =>
                  channel.send(Read(WebSocketFrame.text(txt)))
                )
            }

        } yield ()
      case _ => ZIO.unit
    }

  def recieveAllText(
    text: String,
    channel: WebSocketChannel,
    initializing: Boolean = false
  ): ZIO[Any, Object, Unit] =
    // println(s"Received Text ${text}")
    text match {
      case _ if initializing =>
        initialize_session
      case "end" =>
        channel.shutdown
      case text =>
        (for {
          msg <- parse_message(text)
          authorized <- authorizeMsg(msg)
          _ <-
            if (authorized)
              handle_request(msg).provide(ZLayer.succeed(channel))
            else ZIO.unit
        } yield ())
          .fold(err => println(s"error processing cmd $text :  $err"), x => x)
    }

  def recieveAll(channel: WebSocketChannel, text: String) =
    authenticated.get.flatMap(authd =>
      if (authd) {
        recieveAllText(text, channel)
      } else
        (for {
          _ <- ZIO.log(s"Authorizing: $text")
          sentSecret <- ZIO
            .fromEither(text.fromJson[Session])
            .flatMapError(_ =>
              Console
                .printLine(s"error decoding text: $text")
                .mapError(_ => ???)
            )
          secretOp <- authMap.get.map(_.get(id))
          secret <- ZIO
            .fromOption(secretOp)
            .flatMapError(err =>
              Console
                .printLine(s"Error retrieving secret for $id, cause: $err")
                .mapError(_ => ???)
            )
          _ <- authenticated.update(_ => secret == sentSecret)
          verified <- authenticated.get
          _ <-
            if (verified) for {
              _ <- Console.printLine("Verified succeeded")
              _ <- authenticated.update(_ => true)
              // initialize response stream when first authenticating
//              _ <- sendResponses(ZStream.fromQueue(response_queue))
//                .provide(ZLayer.succeed(channel))
              _ <- linkControllerServer(channel)
              _ <- linkControllerClient(channel)
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
        // *> CausalProfiler
//            .progressPoint(
//              "rendered command" +
//                ""
//            )

        case UserEventTriggered(UserEvent.HandshakeTimeout) =>
          ZIO.succeed(println("handshake timeout"))
        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          channel.send(Read(WebSocketFrame.text("Greetings!")))
        case Read(WebSocketFrame.Close(status, reason)) =>
          Console.printLine(
            "Closing channel with status: " + status + " and reason: " + reason
          )
        case ExceptionCaught(cause) =>
          // channel.shutdown
          Console.printLine(s"Channel error!: ${cause.getMessage}")

      }
    }
}

object BasicWebSocket extends WebSocketControlServer.Service[Any] {

  override def make(
    authID: AUTH_ID
  ): ZIO[BasicController[CONTROLLER_ENV, Queue[QueryResponse]]
    with Ref[SESSION_MAP]
    with Ref[SERVER_IDS], Nothing, WebSocketControlServer[Any]] =
    for {
      controller <- ZIO
        .service[BasicController[CONTROLLER_ENV, Queue[QueryResponse]]]
      sessions <- ZIO.service[Ref[SESSION_MAP]]
      server_keys <- ZIO.serviceWithZIO[Ref[SERVER_IDS]](_.get)
      authd <- Ref.make(false)
      cachedAuth <- Ref.make(Map.empty[Any, Boolean])
      q <- Queue.unbounded[QueryResponse]
      res = BasicWebSocket(
        authID,
        sessions,
        controller,
        authd,
        server_keys.toSet,
        AuthenticationService(
          cachedAuth,
          AuthCommandService.all_non_par(server_keys.toSet)
        ).verify_with_caching,
        q
      )
    } yield BasicWebSocket(
      authID,
      sessions,
      controller,
      authd,
      Set("1"),
      AuthenticationService(
        cachedAuth,
        AuthCommandService.all_non_par(Set("1"))
      ).verify_with_caching,
      q
    )
}
