package network

import controller.BasicController
import controller.Control.CONTROLLER_ENV
import controller.QueryResponse
import entity.WorldBlock
import network.WebSocketServer.AUTH_ID
import network.WebSocketServer.SESSION_MAP
import src.com.main.scala.entity.Globz
import zio.Chunk
import zio.Queue
import zio.Ref
import zio.ZIO
import zio.http.WebSocketApp

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
