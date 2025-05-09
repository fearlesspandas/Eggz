package entity

import entity.EggzOps.ID
//import zio.Has

object Eggz {

  trait Service extends EggzOps.Service {
    val id: ID
  }

  trait EggzError
  case class GenericEggzError(msg: String) extends EggzError
}
trait StorageEgg[I] extends Eggz.Service with Storage.Service[I] {
  val inventory: Storage.Service[I]
}
