package controller

import entity.WorldBlock
import entity.WorldBlockInMem
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.GlobzInMem
import zio.IO
import zio.Ref
import zio.Tag
import zio.ZIO
import zio.ZLayer

import scala.reflect.runtime.universe.TypeTag
trait BasicController[Env] {
  def runCommand[E](comm: ZIO[Env, E, Unit]): ZIO[Any, E, BasicController[Env]]
  def runQuery[Q, E](query: ZIO[Env, E, Q]): ZIO[Any, E, Q]

}

trait ControllerError

object BasicController {
  type COMMAND = String
  type QUERY = String

  trait Service[Env] {
    def make: IO[ControllerError, BasicController[Env]]
  }
  type DEFINED = Globz.Service with WorldBlock.Block
  def make: ZIO[Service[DEFINED], ControllerError, BasicController[DEFINED]] =
    ZIO.service[Service[DEFINED]].flatMap(_.make)

}

case class Control(glob: Globz.Service, worldBlock: Ref[WorldBlock.Block])
    extends BasicController[Globz.Service with WorldBlock.Block] {

  override def runCommand[E](
    comm: ZIO[Globz.Service with WorldBlock.Block, E, Unit]
  ): ZIO[Any, E, BasicController[Globz.Service with WorldBlock.Block]] =
    for {
      wb <- worldBlock.get
      _ <- comm.provide(ZLayer { ZIO.succeed { glob } } ++ ZLayer { ZIO.succeed(wb) })
    } yield this

  override def runQuery[Q, E](
    query: ZIO[Globz.Service with WorldBlock.Block, E, Q]
  ): ZIO[Any, E, Q] =
    worldBlock.get.flatMap(wb =>
      query.provide(ZLayer { ZIO.succeed { glob } } ++ ZLayer { ZIO.succeed(wb) })
    )
}
object Control extends BasicController.Service[Globz.Service with WorldBlock.Block] {
  override def make: IO[ControllerError, BasicController[Globz.Service with WorldBlock.Block]] =
    for {
      w <- WorldBlock.make.provide(ZLayer.succeed(WorldBlockInMem)).mapError(_ => ???)
      r <- Ref.make(w)
    } yield Control(GlobzInMem, r)
}
