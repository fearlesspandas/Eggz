package src.com.main.scala.entity

import zio.console.{Console, getStrLn, putStr, putStrLn}
import zio.{&, ExitCode, Has, IO, Ref, URIO, ZIO, ZLayer}
import java.io.IOException
import java.util

import src.com.main.scala.entity.EggzOps.{EggzOps, ID}
import src.com.main.scala.entity.Globz.{GLOBZ_ERR, GLOBZ_IN, GLOBZ_OUT, Globz}
import src.com.main.scala.entity.Storage.Storage




//
//trait Eggz extends EggzOps.Service {
//
//  val id:ID
//  val health:Double
//  val top:Option[ID]
//  val bottom:Option[ID]
//  val right:Option[ID]
//  val left:Option[ID]
//  //def op:ZIO[Globz,String,ExitCode]
//   // replace with no default
//  def setHealth(health:Double):IO[Eggz
//  def setEnergy(value:Double):IO[Eggz]
//}


object Eggz {
  type Eggz = Has[Eggz.Service]

  trait Service extends EggzOps.Service {
    val id:ID
    val health:Double
    val top:Option[ID]
    val bottom:Option[ID]
    val right:Option[ID]
    val left:Option[ID]
    //def op:ZIO[Globz,String,ExitCode]
    // replace with no default
    def setHealth(health:Double):IO[EggzError,Eggz.Service]
    def setEnergy(value:Double):IO[EggzError,Eggz.Service]
  }

  trait EggzError
  case class GenericEggzError(msg:String) extends EggzError
}
trait StorageEgg[I] extends Eggz.Service with Storage.Service[I]{
  val inventory:Option[Storage.Service[I]]
}


//storage => pipe => process ==> combination


