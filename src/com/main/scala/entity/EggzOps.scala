package src.com.main.scala.entity

import src.com.main.scala.entity.Globz.GLOBZ_ERR
import zio.Ref
//import src.com.main.scala.entity.Globz.Globz
import zio.ExitCode
//import zio.Has
import zio.ZIO

object EggzOps {
  type ID = String

  trait Service {

    //def withGlob(globz: Ref[Globz.Service])
    def op: ZIO[Globz.Service, GLOBZ_ERR, ExitCode]
//
//    def top: ZIO[Globz.Service, GLOBZ_ERR, ExitCode]
//    def bop: ZIO[Globz.Service, GLOBZ_ERR, ExitCode]
//    def lop: ZIO[Globz.Service, GLOBZ_ERR, ExitCode]
//    def rop: ZIO[Globz.Service, GLOBZ_ERR, ExitCode]

    def op2(glob: Ref[Globz.Service]): ZIO[Ref[Globz.Service], GLOBZ_ERR, ExitCode]
  }

}
