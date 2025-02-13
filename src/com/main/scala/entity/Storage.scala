package src.com.main.scala.entity

import src.com.main.scala.entity.Storage.GenericServiceError
import src.com.main.scala.entity.Storage.REF_STORE
import zio.Chunk
import zio.Ref
import zio.UIO
import zio.ZIO
//import zio.Has
import zio.IO

object Storage {
//  type Storage[I] = Has[Storage.Service[I]]
  type REF_STORE[I] = Ref[Map[I, Int]]

  def make[I]: UIO[REF_STORE[I]] = Ref.make(Map.empty[I, Int])

  trait Service[I] {
    val storage: REF_STORE[I]

    def add(item: I*): IO[ServiceError, Storage.Service[I]] =
      ZIO
        .foreach(item)(i =>
          storage
            .update(stor => stor.updated(i, stor.getOrElse(i, 0) + 1))
        )
        .as(this)

    def remove(item: I*): IO[ServiceError, Storage.Service[I]] =
      ZIO
        .foreach(item)(i => storage.update(_.filterNot((k, v) => k == i)))
        .as(this)

    def getInventory(): IO[ServiceError, Map[I, Int]] = storage.get
  }

  trait ServiceError extends Error

  case class GenericServiceError(msg: String) extends ServiceError

}
