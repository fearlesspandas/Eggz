package src.com.main.scala.entity

import zio.console.{Console, getStrLn, putStr, putStrLn}
import zio.{&, ExitCode, Has, IO, Ref, URIO, ZIO, ZLayer}
import java.io.IOException
import java.util

import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.{GLOBZ_ERR, GLOBZ_IN, GLOBZ_OUT, Globz}





trait Eggz extends EggzOps.Service {

  val id:ID
  val health:Double
  val top:Option[ID]
  val bottom:Option[ID]
  val right:Option[ID]
  val left:Option[ID]
  //def op:ZIO[Globz,String,ExitCode]
  val inventory:Option[storage] // replace with no default
  def setHealth(health:Double):Eggz
  def setEnergy(value:Double):Eggz
}




//storage => pipe => process ==> combination


