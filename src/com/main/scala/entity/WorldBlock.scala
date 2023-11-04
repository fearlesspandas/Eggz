package entity

import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.Globz
import zio.ExitCode
import zio.Has
import zio.IO
import zio.ZIO

object WorldBlock {

  type WorldBlock = Has[WorldBlock.Service]

  trait Service {
    def spawnBlob(blob: Globz.Service, coords: Vector[Double]): IO[WorldBlockError, ExitCode]
    def getAllBlobs(): IO[WorldBlockError, Set[Globz.Service]]
    def removeBlob(blob: Globz.Service): IO[WorldBlockError, ExitCode]
    def tickAllBlobs(): IO[WorldBlockError, ExitCode]
  }

  def spawnBlob(
    blob: Globz.Service,
    coords: Vector[Double]
  ): ZIO[WorldBlock, WorldBlockError, ExitCode] = ZIO.accessM(_.get.spawnBlob(blob, coords))

  def getAllBlobs(): ZIO[WorldBlock, WorldBlockError, Set[Globz.Service]] =
    ZIO.accessM(_.get.getAllBlobs())

  def removeBlob(blob: Globz.Service): ZIO[WorldBlock, WorldBlockError, ExitCode] =
    ZIO.accessM(_.get.removeBlob(blob))

  def tickAllBlobs(): ZIO[WorldBlock, WorldBlockError, ExitCode] = ZIO.accessM(_.get.tickAllBlobs())

  trait WorldBlockError

}
