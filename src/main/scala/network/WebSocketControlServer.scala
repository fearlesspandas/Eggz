package network

import controller.Control.CONTROLLER_ENV
import controller.BasicController
import controller.QueryResponse
import network.WebSocketServer.AUTH_ID
import network.WebSocketServer.SESSION_MAP
import zio.http.WebSocketApp
import zio.*
trait WebSocketControlServer[Env] {
  def socket(authenticated: Boolean): WebSocketApp[Env]
}

object WebSocketControlServer {
  trait Service[Env] {

    def make(
      authID: AUTH_ID
    ): ZIO[BasicController[CONTROLLER_ENV, Queue[
      QueryResponse
    ]]
      with Ref[SESSION_MAP]
      with Ref[Chunk[String]], Nothing, WebSocketControlServer[
      Any
    ]]
  }
}
