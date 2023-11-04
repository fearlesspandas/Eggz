package entity

import entity.WorldBlock.GenericWorldBlockError
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ID
//import src.com.main.scala.entity.Globz.Globz
import zio.ExitCode
//import zio.Has
import zio.IO
import zio.ZIO

object WorldBlock {

//  type WorldBlock = Has[WorldBlock.Service]

  trait Service {
    def spawnBlob(
      blob: Globz.Service,
      coords: Vector[Double]
    ): IO[WorldBlockError, WorldBlock.Service]
    def getAllBlobs(): ZIO[Globz.Service, WorldBlockError, Set[Globz.Service]]
    def removeBlob(blob: Globz.Service): IO[WorldBlockError, WorldBlock.Service]
    def tickAllBlobs(): ZIO[Globz.Service, WorldBlockError, ExitCode]
  }

  def spawnBlob(
    blob: Globz.Service,
    coords: Vector[Double]
  ): ZIO[WorldBlock.Service, WorldBlockError, WorldBlock.Service] =
    ZIO.environmentWithZIO(_.get.spawnBlob(blob, coords))

  def getAllBlobs()
    : ZIO[WorldBlock.Service with Globz.Service, WorldBlockError, Set[Globz.Service]] =
    ZIO.environmentWithZIO[WorldBlock.Service](_.get.getAllBlobs())

  def removeBlob(
    blob: Globz.Service
  ): ZIO[WorldBlock.Service, WorldBlockError, WorldBlock.Service] =
    ZIO.environmentWithZIO(_.get.removeBlob(blob))

  def tickAllBlobs(): ZIO[WorldBlock.Service with Globz.Service, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO[WorldBlock.Service](_.get.tickAllBlobs())

  trait WorldBlockError

  case class GenericWorldBlockError(msg: String) extends WorldBlockError

}

case class WorldBlockInMem(db: Map[GLOBZ_ID, Vector[Double]] = Map()) extends WorldBlock.Service {

  def spawnBlobInternal(blobl: Globz.Service, coords: Vector[Double]): WorldBlock.Service =
    WorldBlockInMem(db.updated(blobl.id, coords))
  def removeBlobInternal(blob: Globz.Service): WorldBlock.Service =
    WorldBlockInMem(db.removed(blob.id))

  override def spawnBlob(
    blob: Globz.Service,
    coords: Vector[Double]
  ): IO[WorldBlock.WorldBlockError, WorldBlock.Service] =
    ZIO
      .succeed {
        this.spawnBlobInternal(blob, coords)
      }
  //.mapError(_ => GenericWorldBlockError("error spawning blob"))

  override def getAllBlobs(): ZIO[Globz.Service, WorldBlock.WorldBlockError, Set[Globz.Service]] =
    (for {
      r <- ZIO.collectAllPar(db.keys.map(Globz.get(_)))
    } yield r.collect { case Some(g: Globz.Service) => g }.toSet[Globz.Service])
      .mapError(_ => GenericWorldBlockError("Error fetching blobs"))

  override def removeBlob(blob: Globz.Service): IO[WorldBlock.WorldBlockError, WorldBlock.Service] =
    ZIO
      .succeed {
        this.removeBlobInternal(blob)
      }
  //.mapError(_ => GenericWorldBlockError("error removing blob"))

  override def tickAllBlobs(): ZIO[Globz.Service, WorldBlock.WorldBlockError, ExitCode] =
    (for {
      all <- getAllBlobs()
      r <- ZIO.collectAllPar(all.map(_.tickAll()))
      fail = r.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)).mapError(_ => GenericWorldBlockError("error tick blobs"))
}
