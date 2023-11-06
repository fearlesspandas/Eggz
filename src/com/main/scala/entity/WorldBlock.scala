package entity

import entity.WorldBlock.GenericWorldBlockError
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.Ref
import zio.ZLayer
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
    ): IO[WorldBlockError, ExitCode]
    def getAllBlobs(): ZIO[Any, WorldBlockError, Set[Globz.Service]]
    def removeBlob(blob: Globz.Service): IO[WorldBlockError, ExitCode]
    def tickAllBlobs(): ZIO[Globz.Service, WorldBlockError, ExitCode]
  }

  def spawnBlob(
    blob: Globz.Service,
    coords: Vector[Double]
  ): ZIO[WorldBlock.Service, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.spawnBlob(blob, coords))

  def getAllBlobs(): ZIO[WorldBlock.Service, WorldBlockError, Set[Globz.Service]] =
    ZIO.environmentWithZIO[WorldBlock.Service](_.get.getAllBlobs())

  def removeBlob(
    blob: Globz.Service
  ): ZIO[WorldBlock.Service, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.removeBlob(blob))

  def tickAllBlobs(): ZIO[WorldBlock.Service with Globz.Service, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO[WorldBlock.Service](_.get.tickAllBlobs())

  trait WorldBlockError

  case class GenericWorldBlockError(msg: String) extends WorldBlockError

}

case class WorldBlockInMem(
  coordsRef: Ref[Map[GLOBZ_ID, Vector[Double]]],
  dbRef: Ref[Map[GLOBZ_ID, Globz.Service]]
) extends WorldBlock.Service {

  override def spawnBlob(
    blob: Globz.Service,
    coords: Vector[Double]
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    (for {
      _ <- coordsRef.update(_.updated(blob.id, coords))
      _ <- dbRef.update(_.updated(blob.id, blob))
    } yield ExitCode.success) //.mapError(_ => GenericWorldBlockError("error spawning blob"))
  //.mapError(_ => GenericWorldBlockError("error spawning blob"))

  override def getAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, Set[Globz.Service]] =
    for {
      db <- dbRef.get
    } yield db.values.toSet

  override def removeBlob(blob: Globz.Service): IO[WorldBlock.WorldBlockError, ExitCode] =
    for {
      _ <- coordsRef.update(_.removed(blob.id))
      _ <- dbRef.update(_.removed(blob.id))
    } yield ExitCode.success
  //.mapError(_ => GenericWorldBlockError("error removing blob"))

  override def tickAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, ExitCode] =
    (for {
      all <- getAllBlobs()
      r <- ZIO.collectAllPar(all.map(g => g.tickAll().provide(ZLayer { ZIO.succeed(g) })))
      fail = r.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)).mapError(_ => GenericWorldBlockError("error tick blobs"))
}

object WorldBlockEnvironment {

  val worldblock = ZLayer {
    ZIO
      .service[Ref[Map[GLOBZ_ID, Vector[Double]]]]
      .flatMap(s => ZIO.service[Ref[Map[GLOBZ_ID, Globz.Service]]].map(t => WorldBlockInMem(s, t)))
  }

}
