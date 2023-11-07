package src.com.main.scala.entity

import java.util

import src.com.main.scala.entity
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz.GLOBZ_OUT
import src.com.main.scala.entity.GlobzEnvironment.EggMap
import src.com.main.scala.entity.GlobzEnvironment.RelationMap
import zio.Ref
//import src.com.main.scala.entity.Globz.Globz
import zio.ExitCode
//import zio.Has
import zio.IO
import zio.ZIO
import zio.ZLayer
// move to this model : https://github.com/psisoyev/release-pager/blob/master/storage/src/main/scala/io/pager/subscription/chat/ChatStorage.scala#L20
case class GlobzInMem(val id: GLOBZ_ID, dbref: EggMap, relationRef: RelationMap)
    extends Globz.Service {
  private val db = new util.HashMap[String, Eggz.Service]()

  override def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT] =
    (for {
      _ <- dbref.update(_.updated(eggz.id, eggz))
      udtd <- dbref.get
    } yield this)
  override def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_IN]] =
    for {
      r <- dbref.get
    } yield r.get(id)

  override def remove(id: ID): IO[GLOBZ_ERR, Unit] =
    for {
      _ <- dbref.update(_.removed(id))
    } yield ()

  override def tickAll(): ZIO[Any, GLOBZ_ERR, ExitCode] =
    for {
      all <- getAll()
      e <- ZIO.collectAllPar(
        all.map(_.op.fold(_ => ExitCode.failure, x => x).provide(ZLayer { ZIO.succeed(this) }))
      )
      fail = e.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)

  override def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]] =
    for {
      ref <- dbref.get
    } yield ref.values.toSet

  override def create(id: GLOBZ_ID): IO[GLOBZ_ERR, Globz.Service] =
    for {
      r <- Ref.make(Map.empty[GLOBZ_ID, Eggz.Service])
      k <- Ref.make(Map.empty[(GLOBZ_ID, GLOBZ_ID), Boolean])
    } yield GlobzInMem(id, r, k)

  override def relate(
    egg1: entity.Globz.GLOBZ_IN,
    egg2: entity.Globz.GLOBZ_IN
  ): IO[GLOBZ_ERR, Unit] =
    for {
      _ <- relationRef.update(_.updated((egg1.id, egg2.id), true))
    } yield ()

  override def neighbors(egg: entity.Globz.GLOBZ_IN): IO[GLOBZ_ERR, Vector[entity.Globz.GLOBZ_IN]] =
    for {
      rels <- relationRef.get
      tasks = rels.toSeq
        .collect {
          case ((id1, id2), related) if id1 == egg.id && related => id2
          case ((id1, id2), related) if id2 == egg.id && related => id1
        }
        .map(x => get(x))
      k <- ZIO.collectAllPar(tasks)
    } yield ???
}

object GlobzEnvironment {
  type EggMap = Ref[Map[GLOBZ_ID, Eggz.Service]]
  type RelationMap = Ref[Map[(GLOBZ_ID, GLOBZ_ID), Boolean]]
  val inMemory = ZLayer {
    ZIO.service[Ref[Map[GLOBZ_ID, Eggz.Service]]].map(GlobzInMem("1", _))
  }
  val anyRef: ZLayer[Any, Nothing, Ref[Map[GLOBZ_ID, Eggz.Service]]] = ZLayer {
    for {
      m <- Ref.make(Map.empty[GLOBZ_ID, Eggz.Service])
    } yield m
  }
}
