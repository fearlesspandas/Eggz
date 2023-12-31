package entity

import entity.WorldBlock.GenericWorldBlockError
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz
import zio.Ref
import zio.ZLayer
//import src.com.main.scala.entity.Globz
import zio.ExitCode
//import zio.Has
import zio.IO
import zio.ZIO

object WorldBlock {

//  type WorldBlock = Has[WorldBlock.Service]

  trait Block {
    def spawnBlob(
                   blob: Globz,
                   coords: Vector[Double]
    ): IO[WorldBlockError, ExitCode]
    def spawnFreshBlob(coords: Vector[Double]): ZIO[Globz, WorldBlockError, ExitCode]
    def getAllBlobs(): ZIO[Any, WorldBlockError, Set[Globz]]
    def removeBlob(blob: Globz): IO[WorldBlockError, ExitCode]
    def tickAllBlobs(): ZIO[Any, WorldBlockError, ExitCode]
    def getBlob(id: GLOBZ_ID): IO[WorldBlockError, Option[Globz]]

    def updateBlob(blob: Globz): IO[WorldBlockError, ExitCode]

  }

  trait Service {
    def make: IO[WorldBlockError, WorldBlock.Block]
  }
  def make: ZIO[WorldBlock.Service, WorldBlockError, WorldBlock.Block] =
    ZIO.service[WorldBlock.Service].flatMap(_.make)

  def spawnBlob(
                 blob: Globz,
                 coords: Vector[Double]
  ): ZIO[WorldBlock.Block, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.spawnBlob(blob, coords))
  def spawnFreshBlob(
    coords: Vector[Double]
  ): ZIO[WorldBlock.Block with Globz, WorldBlockError, ExitCode] =
    ZIO.service[WorldBlock.Block].flatMap(_.spawnFreshBlob(coords))
  def getAllBlobs(): ZIO[WorldBlock.Block, WorldBlockError, Set[Globz]] =
    ZIO.environmentWithZIO[WorldBlock.Block](_.get.getAllBlobs())

  def removeBlob(
    blob: Globz
  ): ZIO[WorldBlock.Block, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO(_.get.removeBlob(blob))

  def tickAllBlobs(): ZIO[WorldBlock.Block, WorldBlockError, ExitCode] =
    ZIO.environmentWithZIO[WorldBlock.Block](_.get.tickAllBlobs())
  def getBlob(id: GLOBZ_ID): ZIO[WorldBlock.Block, WorldBlockError, Option[Globz]] =
    ZIO.service[WorldBlock.Block].flatMap(_.getBlob(id))
  trait WorldBlockError

  case class GenericWorldBlockError(msg: String) extends WorldBlockError

}

case class WorldBlockInMem(
  coordsRef: Ref[Map[GLOBZ_ID, Vector[Double]]],
  dbRef: Ref[Map[GLOBZ_ID, Globz]]
) extends WorldBlock.Block {

  override def spawnBlob(
                          blob: Globz,
                          coords: Vector[Double]
  ): IO[WorldBlock.WorldBlockError, ExitCode] =
    (for {
      _ <- coordsRef.update(_.updated(blob.id, coords))
      _ <- dbRef.update(_.updated(blob.id, blob))
    } yield ExitCode.success) //.mapError(_ => GenericWorldBlockError("error spawning blob"))
  //.mapError(_ => GenericWorldBlockError("error spawning blob"))

  override def getAllBlobs(): ZIO[Any, WorldBlock.WorldBlockError, Set[Globz]] =
    for {
      db <- dbRef.get
    } yield db.values.toSet

  override def removeBlob(blob: Globz): IO[WorldBlock.WorldBlockError, ExitCode] =
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
  ): ZIO[Globz, WorldBlock.WorldBlockError, ExitCode] =
    ZIO.service[Globz].flatMap(spawnBlob(_, coords))

  override def getBlob(id: GLOBZ_ID): IO[WorldBlock.WorldBlockError, Option[Globz]] =
    for {
      res <- dbRef.get.map(_.get(id))
    } yield res

  override def updateBlob(blob: Globz): IO[WorldBlock.WorldBlockError, ExitCode] = ???
}
object WorldBlockInMem extends WorldBlock.Service {
  override def make: IO[WorldBlock.WorldBlockError, WorldBlock.Block] =
    for {
      s <- Ref.make(Map.empty[GLOBZ_ID, Vector[Double]])
      t <- Ref.make(Map.empty[GLOBZ_ID, Globz])
    } yield WorldBlockInMem(s, t)
}
object WorldBlockEnvironment {

  val worldblock =
    ZLayer[Ref[Map[GLOBZ_ID, Vector[Double]]] with Ref[Map[GLOBZ_ID, Globz]], Nothing, WorldBlock.Block] {
      ZIO
        .service[Ref[Map[GLOBZ_ID, Vector[Double]]]]
        .flatMap(s => ZIO.service[Ref[Map[GLOBZ_ID, Globz]]].map(t => WorldBlockInMem(s, t)))
    }
  val anyref = ZLayer[Any, Nothing, Ref[Map[GLOBZ_ID, Vector[Double]]]] {
    for {
      r <- Ref.make(Map.empty[GLOBZ_ID, Vector[Double]])
    } yield r
  } ++ ZLayer[Any, Nothing, Ref[Map[GLOBZ_ID, Globz]]] {
    for {
      r <- Ref.make(Map.empty[GLOBZ_ID, Globz])
    } yield r
  }

}
