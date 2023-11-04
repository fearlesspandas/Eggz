package src.com.main.scala.entity

import src.com.main.scala.entity.Storage.GenericServiceError
import src.com.main.scala.entity.Storage.REF_STORE
import zio.ZIO
//import zio.Has
import zio.IO

trait storage {
  val sizeLimit: Int
  val weightLimit: Double
  val contents: Set[Any]
}

object Storage {
//  type Storage[I] = Has[Storage.Service[I]]
  type REF_STORE[I] = Set[I]

  trait Service[I] {
    //val refs: REF_STORE[I]

    def add(item: I*): IO[ServiceError, Storage.Service[I]]

    def remove(item: I*): IO[ServiceError, Storage.Service[I]]

    def getAll(): IO[ServiceError, Set[I]]
  }

//    def add[I](item:I*):ZIO[Storage[I],ServiceError,Storage.Service[I]] = ZIO.accessM(_.get.add(item:_*))
//    def remove[I](item:I*):ZIO[Storage[I],ServiceError,Storage.Service[I]] = ZIO.accessM(_.get.remove(item:_*))

  trait ServiceError extends Error

  case class GenericServiceError(msg: String) extends ServiceError

}

case class basicStorage[I](refs: REF_STORE[I] = Set()) extends Storage.Service[I] {
  // val refs:REF_STORE[I] = HashSet()
  override def add(item: I*): IO[Storage.ServiceError, Storage.Service[I]] =
    ZIO
      .succeed {
        basicStorage(refs ++ item)
        //.mapError( GenericServiceError("whoopsie"))
      }
  //.mapError(_ => GenericServiceError("whoopsie"))

  override def remove(item: I*): IO[Storage.ServiceError, Storage.Service[I]] =
    ZIO
      .succeed {
        basicStorage(refs.filter(i => !item.toSet.contains(i)))
      }
  //.mapError(_ => GenericServiceError("whoopsie"))

  override def getAll(): IO[Storage.ServiceError, Set[I]] =
    ZIO
      .succeed {
        this.refs.toSet[I]
      }
  //.mapError(_ => GenericServiceError("error fetching inventory"))
}

object basicStorage {
  //  def apply()
}

case class BasicStorage(sizeLimit: Int, weightLimit: Double, contents: Set[Any] = Set())
    extends storage {
  def setContents(value: Set[Any]): storage = this.copy(contents = value)

  def addContents(value: Set[Any]): storage =
    this.copy(contents = this.contents ++ value) //definitely fix this to be true addition (currently overwrites dups)
  def removeContents() = ???
}
