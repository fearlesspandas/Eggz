package controller

import entity.WorldBlock
import physics.{Destination, TELEPORT, WAYPOINT}
import zio.*
import zio.http.WebSocketChannel
import zio.stream.ZStream
trait LinkStreams {

  def streamDestinations(stream: ZStream[Any, Nothing, Chunk[Destination]]) =
    for {
      channel <- ZIO.service[WebSocketChannel]
      wb <- ZIO.service[WorldBlock.Block]
      _ <- stream
        .foreach(ch =>
          ZIO
            .fromOption(ch.headOption)
            .flatMap(dest =>
              dest.dest_type match {
                case TELEPORT => ???
                case WAYPOINT => ???
              }
            )
        )
        .fork
    } yield ()
}
