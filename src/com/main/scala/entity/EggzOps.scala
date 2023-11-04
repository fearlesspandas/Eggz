package src.com.main.scala.entity

import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.Globz
import zio.ExitCode
import zio.Has
import zio.ZIO

object EggzOps {
  type ID = String
  type EggzOps = Has[EggzOps.Service]

  trait Service {
    def op: ZIO[Globz, GLOBZ_ERR, ExitCode]
  }

}
