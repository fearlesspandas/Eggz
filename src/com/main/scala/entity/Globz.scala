package src.com.main.scala.entity

import src.com.main.scala.entity.EggzOps.ID
import zio.ExitCode
import zio.Has
import zio.IO
import zio.ZIO

object Globz {
  type Globz = Has[Globz.Service]
  type GLOBZ_ERR = String
  type GLOBZ_OUT = Eggz.Service
  type GLOBZ_IN = Eggz.Service

  //define operations without service impl
  def update(eggz: GLOBZ_IN): ZIO[Globz, GLOBZ_ERR, GLOBZ_OUT] =
    ZIO.accessM[Globz](_.get.update(eggz))

  def remove(id: ID): ZIO[Globz, GLOBZ_ERR, Unit] = ZIO.accessM(_.get.remove(id))

  def get(id: ID): ZIO[Globz, GLOBZ_ERR, Option[GLOBZ_OUT]] = ZIO.accessM(_.get.get(id))

  def getAll(): ZIO[Globz, GLOBZ_ERR, Set[GLOBZ_OUT]] = ZIO.accessM(_.get.getAll())

  def tickAll(): ZIO[Globz, GLOBZ_ERR, ExitCode] = ZIO.accessM(_.get.tickAll())

  trait Service {
    def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT]

    def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_OUT]]

    def remove(id: ID): IO[GLOBZ_ERR, Unit]

    def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_OUT]]

    def tickAll(): ZIO[Globz, GLOBZ_ERR, ExitCode]
  }
}
