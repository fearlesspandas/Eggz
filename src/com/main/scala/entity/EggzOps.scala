package src.com.main.scala.entity

import src.com.main.scala.entity.Globz.GLOBZ_ERR
import zio.Ref
//import src.com.main.scala.entity.Globz
import zio.ExitCode
//import zio.Has
import zio.ZIO

object EggzOps {
  type ID = String

  trait Service {

    def op: ZIO[Globz, GLOBZ_ERR, ExitCode]

  }

}
