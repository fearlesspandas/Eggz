package src.com.main.scala.entity

import src.com.main.scala.entity.Storage.GenericServiceError
import src.com.main.scala.entity.Storage.REF_STORE
import zio.Chunk
import zio.Ref
import zio.UIO
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
  type REF_STORE[I] = Ref[Chunk[I]]

  trait Service[I] {
    // val refs: REF_STORE[I]

    def add(item: I*): IO[ServiceError, Storage.Service[I]]

    def remove(item: I*): IO[ServiceError, Storage.Service[I]]

    def getInventory(): IO[ServiceError, Set[I]]
  }

  def make[A]: UIO[Service[A]] =
    Ref.make(Chunk.empty[A]).map(r => basicStorage[A](r))

  trait ServiceError extends Error

  case class GenericServiceError(msg: String) extends ServiceError

}

case class basicStorage[I](refs: REF_STORE[I]) extends Storage.Service[I] {
  // val refs:REF_STORE[I] = HashSet()
  override def add(item: I*): IO[Storage.ServiceError, Storage.Service[I]] =
    refs.update(_ ++ item).as(this)

  override def remove(item: I*): IO[Storage.ServiceError, Storage.Service[I]] =
    refs.update(_.filter(i => !item.contains(i))).as(this)

  override def getInventory(): IO[Storage.ServiceError, Set[I]] =
    refs.get.map(_.toSet)
  // .mapError(_ => GenericServiceError("error fetching inventory"))
}

object basicStorage {
  //  def apply()
}

case class BasicStorage(
  sizeLimit: Int,
  weightLimit: Double,
  contents: Set[Any] = Set()
) extends storage {
  def setContents(value: Set[Any]): storage = this.copy(contents = value)

  def addContents(value: Set[Any]): storage =
    this.copy(contents =
      this.contents ++ value
    ) // definitely fix this to be true addition (currently overwrites dups)
  def removeContents() = ???
}
