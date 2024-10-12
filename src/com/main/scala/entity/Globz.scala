package src.com.main.scala.entity

import entity.GlobzModel
import src.com.main.scala.entity
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz.GLOBZ_OUT
import zio.ExitCode
import zio.Fiber
import zio.Schedule
//import zio.Has
import zio.IO
import zio.ZIO

trait Globz {
  val id: GLOBZ_ID

  // def getId : IO[GLOBZ_ERR,GLOBZ_ID]
  // def setId : IO[GLOBZ_ERR, UNIT]
  def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT]

  def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_IN]]

  def remove(id: ID): IO[GLOBZ_ERR, Unit]

  def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]]

  def tickAll(): ZIO[Any, GLOBZ_ERR, ExitCode]

  def relate(
    egg1: GLOBZ_ID,
    egg2: GLOBZ_ID,
    bidirectional: Boolean,
    process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit]
  def unrelate(
    egg1: GLOBZ_ID,
    egg2: GLOBZ_ID,
    bidirectional: Boolean,
    cleanup_process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit]

  def unrelateAll(
    egg: GLOBZ_ID,
    direction: Int,
    cleanup_process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit]
  // direction indicates whether we want nodes pointing to, from or either relative to our glob
  def neighbors(
    egg: GLOBZ_ID,
    direction: Int
  ): IO[GLOBZ_ERR, Vector[GLOBZ_ID]]
  def serializeGlob: IO[GLOBZ_ERR, GlobzModel]
  def scheduleEgg(
    egg: GLOBZ_IN,
    op: ZIO[GLOBZ_IN, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit]
}
object Globz {
//  type Globz = Has[Globz.Service]
  type GLOBZ_ERR = String
  type GLOBZ_OUT = Globz
  type GLOBZ_IN = Eggz.Service
  type GLOBZ_ID = String
  def create(id: GLOBZ_ID): ZIO[Globz.Service, GLOBZ_ERR, Globz] =
    ZIO.environmentWithZIO(_.get.make(id))
//  def serialize(glob: Globz): ZIO[Globz.Service, GLOBZ_ERR, GlobzModel] =
//    ZIO.service[Globz.Service].flatMap(_.serialize(glob))
  trait Service {
    def make(id: GLOBZ_ID): IO[GLOBZ_ERR, Globz]

  }
}
