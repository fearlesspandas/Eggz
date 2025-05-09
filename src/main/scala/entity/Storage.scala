package entity

import zio.*
object Storage {
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

    def getInventoryCount(id: I) =
      storage.get
        .map(_.get(id))
        .flatMap(ZIO.fromOption(_))
        .orElseFail(CouldNotGetInventoryCount)
  }

  trait ServiceError extends Error

  case class GenericServiceError(msg: String) extends ServiceError
  case object CouldNotGetInventoryCount extends ServiceError

}
