package network

import controller.BasicController
import controller.Control
import entity.WorldBlock
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
import zio.json.EncoderOps

trait WebSocketServer {
  val app: HttpApp[Any]
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

  val app =
    Routes(
      Method.GET / "authenticate" / string("id") -> handler { (id: String, request: Request) =>
        (for {
          _ <- Console.printLine("Attempting to update map")
          _ <- authMap.update(_.updated(id, BasicSession(id, "SECRET")))
          res <- authMap.get.map(_.get(id)).flatMap(ZIO.fromOption(_))
          _ <- Console.printLine("map updated successfully")
        } yield Response.text(res.toJson)).fold(_ => Response.badRequest, x => x)

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
