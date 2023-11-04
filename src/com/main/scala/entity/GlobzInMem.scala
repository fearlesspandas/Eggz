package src.com.main.scala.entity

import java.util

import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz.GLOBZ_OUT
//import src.com.main.scala.entity.Globz.Globz
import zio.ExitCode
//import zio.Has
import zio.IO
import zio.ZIO
import zio.ZLayer

case class GlobzInMem(val id: GLOBZ_ID) extends Globz.Service {
  private val db = new util.HashMap[String, Eggz.Service]()

  override def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT] = {
    db.put(eggz.id, eggz)
    ZIO.succeed(eggz)
  }

  override def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_OUT]] =
    ZIO.succeed(Option(db.get(id)))

  override def remove(id: ID): IO[GLOBZ_ERR, Unit] = {
    db.remove(id)
    ZIO.succeed(())
  }

  override def tickAll(): ZIO[Globz.Service, GLOBZ_ERR, ExitCode] =
    for {
      all <- getAll()
      e <- ZIO.collectAllPar(all.map(_.op.fold(_ => ExitCode.failure, x => x)))
      fail = e.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)

  override def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_OUT]] =
    ZIO.succeed {
      db.values().toArray().toSet.asInstanceOf[Set[Eggz.Service]]
    }

  override def create(id: GLOBZ_ID): IO[GLOBZ_ERR, Globz.Service] =
    ZIO.succeed {
      GlobzInMem(id)
    }
}

object GlobzEnvironment {
  val inMemory: ZLayer[Any, Nothing, Globz.Service] = ZLayer.succeed(GlobzInMem("1"))
}
