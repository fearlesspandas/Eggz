package controller

import controller.SerializableCommand.CommandError

case class NotLivingEntity(msg:String) extends CommandError
case class NoEntityFound(msg:String) extends CommandError
