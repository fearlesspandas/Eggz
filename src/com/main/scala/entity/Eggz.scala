package src.com.main.scala.entity

import entity.EggzModel
import src.com.main.scala.entity.EggzOps.ID
import zio.Ref
//import zio.Has
import zio.IO

object Eggz {

  trait Service extends EggzOps.Service {
    val id: ID
    def serializeEgg: IO[EggzError, EggzModel]
  }

  trait EggzError
  case class GenericEggzError(msg: String) extends EggzError
}
trait StorageEgg[I] extends Eggz.Service with Storage.Service[I] {
  val inventory: Ref[Storage.Service[I]]
}
