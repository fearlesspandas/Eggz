package network

import controller.BasicController
import controller.Control
import entity.WorldBlock
import network.WebSocketServer.PUB_KEY_MAP
import network.WebSocketServer.SECRET
import network.WebSocketServer.SESSION_MAP
import src.com.main.scala.entity.Globz
import zio.http.codec.PathCodec.string
import zio.Console
import zio.IO
import zio.Ref
import zio.ZIO
import zio.ZLayer
import zio.http.HttpApp
import zio.http.Method
import zio.http.Request
import zio.http.Response
import zio.http.Routes
import zio.http.handler
import zio.json.*

trait WebSocketServer {
  val app: HttpApp[Any]
}
object WebSocketServer {
  type AUTH_ID = String
  type SECRET = Session
  type SESSION_MAP = Map[String, SECRET]
  type PUB_KEY_MAP = Map[String, String]

  trait Service {
    def make: IO[WebSocketError, WebSocketServer]
  }
}

case class WebSocketServerBasic(
  controller: BasicController[Globz.Service with WorldBlock.Block],
  authMap: Ref[
    SESSION_MAP
  ], // maps id to secret that's expected on first connection
  pubKeys: Ref[PUB_KEY_MAP]
) extends WebSocketServer {

  val app =
    Routes(
      Method.GET / "authenticate" / string("id") -> handler {
        (id: String, request: Request) =>
          (for {
            _ <- ZIO.log("Attempting to update map")
            pubKey <- pubKeys.get
              .map(_.get(id))
              .flatMap {
                case None =>
                  for {
                    _ <- ZIO.log(s"adding new key to registry for id: $id")
                    key <- request.body.asString
                    _ <- pubKeys.update(_.updated(id, key))
                  } yield key
                case Some(k) =>
                  for {
                    _ <- ZIO.log(s"Key already found for id $id")
                  } yield k
              }
            st = CryptoUtils.create_session_token(pubKey)
            _ <- authMap.update(_.updated(id, BasicSession(id, st._1)))
            res <- authMap.get.map(_.get(id)).flatMap(ZIO.fromOption(_))
            _ <- ZIO.log("map updated successfully")
          } yield Response.text(
            BasicSession(id, st._2).asInstanceOf[Session].toJson
          )) // Response.text(res.toJson))
            .fold(_ => Response.badRequest, x => x)

      },
      Method.GET / "connect" / string("id") -> handler {
        (id: String, request: Request) =>
          BasicWebSocket
            .make(id)
            .flatMap(sockerApp => sockerApp.socket(false).toResponse)
            .provide(ZLayer.succeed(controller) ++ ZLayer.succeed(authMap))
      }
    ).toHttpApp

}
trait WebSocketError
case class GenericWebsocketError(msg: String) extends WebSocketError
object WebSocketServerBasic extends WebSocketServer.Service {
  override def make: IO[WebSocketError, WebSocketServer] =
    for {
      controller <- BasicController.make
        .provide(ZLayer.succeed(Control))
        .mapError(err => GenericWebsocketError(err.toString))
      sessionmap <- Ref.make(Map.empty[String, SECRET])
      pubkeyMap <- Ref.make(Map.empty[String, String])
    } yield WebSocketServerBasic(controller, sessionmap, pubkeyMap)
}
