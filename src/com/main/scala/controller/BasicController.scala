package controller

import entity.WorldBlock
import src.com.main.scala.entity.Globz
import zio.{Ref, ZIO, ZLayer}

trait BasicController[Env] {
  def runCommand[E](comm: ZIO[Env, E, Unit]): ZIO[Any, E, BasicController[Env]]
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
      .flatMap(glob =>
        ZIO
          .service[WorldBlock.Service]
          .flatMap(worldblock =>
            for {
              r <- Ref.make(worldblock)
            } yield Control(glob, r)
          )
      )
}

case class Control(glob: Globz.Service, worldBlock: Ref[WorldBlock.Service])
    extends BasicController[Globz.Service with WorldBlock.Service] {

  override def runCommand[E](
    comm: ZIO[Globz.Service with WorldBlock.Service, E, Unit]
  ): ZIO[Any, E, BasicController[Globz.Service with WorldBlock.Service]] =
    for {
      wb <- worldBlock.get
      _ <- comm.provide(ZLayer { ZIO.succeed { glob } } ++ ZLayer { ZIO.succeed(wb) })
    } yield this

  override def runQuery[Q, E](
    query: ZIO[Globz.Service with WorldBlock.Service, E, Q]
  ): ZIO[Any, E, Q] =
    worldBlock.get.flatMap(wb =>
      query.provide(ZLayer { ZIO.succeed { glob } } ++ ZLayer { ZIO.succeed(wb) })
    )
}
