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
      blob: Globz.Glob,
      coords: Vector[Double]
    ): IO[WorldBlockError, ExitCode]
    def spawnFreshBlob(coords: Vector[Double]): ZIO[Globz.Glob, WorldBlockError, ExitCode]
    def getAllBlobs(): ZIO[Any, WorldBlockError, Set[Globz.Glob]]
    def removeBlob(blob: Globz.Glob): IO[WorldBlockError, ExitCode]
    def tickAllBlobs(): ZIO[Any, WorldBlockError, ExitCode]
    def getBlob(id: GLOBZ_ID): IO[WorldBlockError, Option[Globz.Glob]]
  }

  def spawnBlob(
    blob: Globz.Glob,
    coords: Vector[Double]
  ): ZIO[WorldBlock.Service, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.spawnBlob(blob, coords))
  def spawnFreshBlob(
    coords: Vector[Double]
  ): ZIO[WorldBlock.Service with Globz.Glob, WorldBlockError, ExitCode] =
    ZIO.service[WorldBlock.Service].flatMap(_.spawnFreshBlob(coords))
  def getAllBlobs(): ZIO[WorldBlock.Service, WorldBlockError, Set[Globz.Glob]] =
    ZIO.environmentWithZIO[WorldBlock.Service](_.get.getAllBlobs())

  def removeBlob(
    blob: Globz.Glob
  ): ZIO[WorldBlock.Service, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.removeBlob(blob))

  def tickAllBlobs(): ZIO[WorldBlock.Service, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO[WorldBlock.Service](_.get.tickAllBlobs())
  def getBlob(id: GLOBZ_ID): ZIO[WorldBlock.Service, WorldBlockError, Option[Globz.Glob]] =
    ZIO.service[WorldBlock.Service].flatMap(_.getBlob(id))
  trait WorldBlockError

  case class GenericWorldBlockError(msg: String) extends WorldBlockError

}

case class WorldBlockInMem(
  coordsRef: Ref[Map[GLOBZ_ID, Vector[Double]]],
  dbRef: Ref[Map[GLOBZ_ID, Globz.Glob]]
) extends WorldBlock.Service {

  override def spawnBlob(
    blob: Globz.Glob,
    coords: Vector[Double]
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    (for {
      _ <- coordsRef.update(_.updated(blob.id, coords))
      _ <- dbRef.update(_.updated(blob.id, blob))
    } yield ExitCode.success) //.mapError(_ => GenericWorldBlockError("error spawning blob"))
  //.mapError(_ => GenericWorldBlockError("error spawning blob"))

  override def getAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, Set[Globz.Glob]] =
    for {
      db <- dbRef.get
    } yield db.values.toSet

  override def removeBlob(blob: Globz.Glob): IO[WorldBlock.WorldBlockError, ExitCode] =
    for {
      _ <- coordsRef.update(_.removed(blob.id))
      _ <- dbRef.update(_.removed(blob.id))
    } yield ExitCode.success
  //.mapError(_ => GenericWorldBlockError("error removing blob"))

  override def tickAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, ExitCode] =
    (for {
      all <- getAllBlobs()
      r <- ZIO.collectAllPar(all.map(g => g.tickAll()))
      fail = r.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)).mapError(_ => GenericWorldBlockError("error tick blobs"))

  override def spawnFreshBlob(
    coords: Vector[Double]
  ): ZIO[Globz.Glob, WorldBlock.WorldBlockError, ExitCode] =
    ZIO.service[Globz.Glob].flatMap(spawnBlob(_, coords))

  override def getBlob(id: GLOBZ_ID): IO[WorldBlock.WorldBlockError, Option[Globz.Glob]] =
    for {
      res <- dbRef.get.map(_.get(id))
    } yield res
}

object WorldBlockEnvironment {

  val worldblock =
    ZLayer[Ref[Map[GLOBZ_ID, Vector[Double]]] with Ref[Map[GLOBZ_ID, Globz.Glob]], Nothing, WorldBlock.Service] {
      ZIO
        .service[Ref[Map[GLOBZ_ID, Vector[Double]]]]
        .flatMap(s => ZIO.service[Ref[Map[GLOBZ_ID, Globz.Glob]]].map(t => WorldBlockInMem(s, t)))
    }
  val anyref = ZLayer[Any, Nothing, Ref[Map[GLOBZ_ID, Vector[Double]]]] {
    for {
      r <- Ref.make(Map.empty[GLOBZ_ID, Vector[Double]])
    } yield r
  } ++ ZLayer[Any, Nothing, Ref[Map[GLOBZ_ID, Globz.Glob]]] {
    for {
      r <- Ref.make(Map.empty[GLOBZ_ID, Globz.Glob])
    } yield r
  }

}
