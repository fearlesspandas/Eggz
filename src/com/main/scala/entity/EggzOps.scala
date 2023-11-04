package src.com.main.scala.entity

import src.com.main.scala.entity.Globz.{GLOBZ_ERR, Globz}
import zio.{ExitCode, Has, ZIO}

object EggzOps {
  type ID = String
  type EggzOps = Has[EggzOps.Service]

  trait Service{
    def op: ZIO[Globz,GLOBZ_ERR,ExitCode]
  }

}