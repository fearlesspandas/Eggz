package controller

import entity.{LivingEntity, WorldBlock}
import physics.DESTINATION_TYPE.{TELEPORT, WAYPOINT}
import physics.Destination
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.http.WebSocketChannel
import zio.{Chunk, ZIO}
import zio.stream.ZStream

trait LinkStreams {

  def streamDestinations(stream:ZStream[Any,Nothing,Chunk[Destination]]) = for{
    channel <- ZIO.service[WebSocketChannel]
    wb <- ZIO.service[WorldBlock.Block]
    _ <- stream.foreach(ch => ZIO.fromOption(ch.headOption).flatMap(dest => dest.dest_type match {
      case TELEPORT => ???
      case WAYPOINT => ??? 
    })).fork
  } yield ()
}
