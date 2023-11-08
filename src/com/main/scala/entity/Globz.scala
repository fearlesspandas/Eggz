package src.com.main.scala.entity

import src.com.main.scala.entity.EggzOps.ID
import zio.ExitCode
import zio.Schedule
//import zio.Has
import zio.IO
import zio.ZIO

object Globz {
//  type Globz = Has[Globz.Service]
  type GLOBZ_ERR = String
  type GLOBZ_OUT = Globz.Glob
  type GLOBZ_IN = Eggz.Service
  type GLOBZ_ID = String
  def create(id: GLOBZ_ID): ZIO[Globz.Service, GLOBZ_ERR, Globz.Glob] =
    ZIO.environmentWithZIO(_.get.make(id))

  trait Glob {
    val id: GLOBZ_ID
    //def getId : IO[GLOBZ_ERR,GLOBZ_ID]
    //def setId : IO[GLOBZ_ERR, UNIT]
    def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT]

    def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_IN]]

    def remove(id: ID): IO[GLOBZ_ERR, Unit]

    def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]]

    def tickAll(): ZIO[Any, GLOBZ_ERR, ExitCode]

    def relate(egg1: Eggz.Service, egg2: Eggz.Service): IO[GLOBZ_ERR, Unit]

    def neighbors(egg: Eggz.Service): IO[GLOBZ_ERR, Vector[Eggz.Service]]

    def scheduleEgg(
      id: Eggz.Service
    ): IO[GLOBZ_ERR, Unit]
  }

  trait Service {
    def make(id: GLOBZ_ID): IO[GLOBZ_ERR, Globz.Glob]
  }
}
