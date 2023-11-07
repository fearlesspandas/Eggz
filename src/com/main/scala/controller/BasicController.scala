package controller

import controller.BasicController.COMMAND
import controller.BasicController.QUERY
import entity.WorldBlock
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.GlobzInMem
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.ZIO
import zio.ZLayer

trait BasicController[Env] {
  def runCommand[E](comm: ZIO[Env, E, Unit]): ZIO[Any, E, Unit]
  def runQuery[Q, E](query: ZIO[Env, E, Q]): ZIO[Any, E, Q]

}

object BasicController {
  type COMMAND = String
  type QUERY = String
  def make: ZIO[Globz.Service with WorldBlock.Service, Nothing, BasicController[
    Globz.Service with WorldBlock.Service
  ]] =
    ZIO
      .service[Globz.Service]
      .flatMap(glob => ZIO.service[WorldBlock.Service].map(Control(glob, _)))
}

case class Control(glob: Globz.Service, worldBlock: WorldBlock.Service)
    extends BasicController[Globz.Service with WorldBlock.Service] {

  override def runCommand[E](
    comm: ZIO[Globz.Service with WorldBlock.Service, E, Unit]
  ): ZIO[Any, E, Unit] =
    comm.provide(ZLayer { ZIO.succeed { glob } } ++ ZLayer { ZIO.succeed(worldBlock) })

  override def runQuery[Q, E](
    query: ZIO[Globz.Service with WorldBlock.Service, E, Q]
  ): ZIO[Any, E, Q] =
    query.provide(ZLayer { ZIO.succeed { glob } } ++ ZLayer { ZIO.succeed(worldBlock) })

}
