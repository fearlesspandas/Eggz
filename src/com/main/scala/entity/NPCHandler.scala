package entity

import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_IN
import zio.*
trait NPCHandler {
  var glob: Globz
  def add_entity_as_npc(egg: GLOBZ_IN): IO[NPCHandlerError, Unit] =
    glob
      .update(egg)
      .provide(ZLayer.succeed(glob))
      .mapError(_ => ???)
      .map(_ => ())
  def remove_entity_as_npc(id: ID): IO[NPCHandlerError, Unit] = ???

  def scheduleEgg(
    egg: GLOBZ_IN,
    op: ZIO[GLOBZ_IN, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] = glob.scheduleEgg(egg, op)
}

trait NPCHandlerError
object NPCHandler {
  def make(): IO[NPCHandlerError, NPCHandler] = for {
    glob <- Globz
      .create("NPCHandler")
      .provide(ZLayer.succeed(GlobzInMem))
      .mapError(_ => ???)
  } yield BasicNPCHandler(glob)
}

case class BasicNPCHandler(var glob: Globz) extends NPCHandler
