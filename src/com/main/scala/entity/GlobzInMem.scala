package src.com.main.scala.entity

import java.util

import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.{GLOBZ_ERR, GLOBZ_IN, GLOBZ_OUT, Globz}
import zio.{ExitCode, Has, IO, ZIO, ZLayer}


case class GlobzInMem() extends Globz.Service {
  private val db = new util.HashMap[String, Eggz]()

  override def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT] = IO {
    db.put(eggz.id, eggz)
    eggz
  }.mapError(e => e.getMessage)

  override def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_OUT]] = IO {
    Option(db.get(id))
  }.mapError(e => e.getMessage)

  override def remove(id: ID): IO[GLOBZ_ERR, Unit] = IO {
    db.remove(id)
    ()
  }.mapError(e => e.getMessage)

  override def tickAll(): ZIO[Globz, GLOBZ_ERR, ExitCode] =
    for {
      all <- getAll()
      e <- ZIO.collectAllPar(all.map(_.op.fold(_ => ExitCode.failure, x => x)))
      fail = e.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)

  override def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_OUT]] = IO {
    db.values().toArray().toSet.asInstanceOf[Set[Eggz]]
  }.mapError(e => e.getMessage)
}


object GlobzEnvironment {
  val inMemory: ZLayer[Any, Nothing, Has[Globz.Service]] = ZLayer.succeed(GlobzInMem())
}

