package src.com.main.scala.entity

import src.com.main.scala.entity.EggzOps.ID
import zio.ExitCode
//import zio.Has
import zio.IO
import zio.ZIO

object Globz {
//  type Globz = Has[Globz.Service]
  type GLOBZ_ERR = String
  type GLOBZ_OUT = Globz.Service
  type GLOBZ_IN = Eggz.Service
  type GLOBZ_ID = String
  //define operations without service impl
  def update(eggz: GLOBZ_IN): ZIO[Globz.Service, GLOBZ_ERR, GLOBZ_OUT] =
    ZIO.environmentWithZIO[Globz.Service](_.get.update(eggz))
//    ZIO.environmentWithZIO[Globz](_.get.update(eggz))

  def remove(id: ID): ZIO[Globz.Service, GLOBZ_ERR, Unit] = ZIO.environmentWithZIO(_.get.remove(id))

  def get(id: ID): ZIO[Globz.Service, GLOBZ_ERR, Option[GLOBZ_IN]] =
    ZIO.environmentWithZIO(_.get.get(id))

  def getAll(): ZIO[Globz.Service, GLOBZ_ERR, Set[GLOBZ_IN]] =
    ZIO.environmentWithZIO(_.get.getAll())

  def tickAll(): ZIO[Globz.Service, GLOBZ_ERR, ExitCode] = ZIO.environmentWithZIO(_.get.tickAll())
  def create(id: GLOBZ_ID): ZIO[Globz.Service, GLOBZ_ERR, Globz.Service] =
    ZIO.environmentWithZIO(_.get.create(id))
  trait Service {
    val id: GLOBZ_ID
    def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT]

    def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_IN]]

    def remove(id: ID): IO[GLOBZ_ERR, Unit]

    def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]]

    def tickAll(): ZIO[Globz.Service, GLOBZ_ERR, ExitCode]

    def create(id: GLOBZ_ID): IO[GLOBZ_ERR, Globz.Service]
  }
}
