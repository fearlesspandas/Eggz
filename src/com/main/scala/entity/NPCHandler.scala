package entity

import src.com.main.scala.entity.EggzOps.ID
import zio._
trait NPCHandler {
  def add_entity_as_npc(id: ID): IO[NPCHandlerError, Unit]
  def remove_entity_as_npc(id: ID): IO[NPCHandlerError, Unit]
  def tick_npc(id: ID): IO[NPCHandlerError, Unit]
}

trait NPCHandlerError
object NPCHandler {}
