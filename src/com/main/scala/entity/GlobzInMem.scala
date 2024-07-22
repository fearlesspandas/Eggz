package entity

import controller.Stats
import entity.GlobzInMem.EggMap
import entity.GlobzInMem.RelationMap
import src.com.main.scala
import src.com.main.scala.entity

import java.util
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Eggz
import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
import src.com.main.scala.entity.Globz.GLOBZ_IN
import src.com.main.scala.entity.Globz.GLOBZ_OUT
import zio.Ref
import zio.Schedule
import zio.Duration.fromMillis
import zio.Duration.*
import zio.ExitCode
import zio.IO
import zio.ZIO
import zio.ZLayer
case class GlobzInMem(val id: GLOBZ_ID, dbref: EggMap, relationRef: RelationMap)
    extends Globz {
  private val db = new util.HashMap[String, Eggz.Service]()

  override def update(eggz: GLOBZ_IN): IO[GLOBZ_ERR, GLOBZ_OUT] =
    for {
      _ <- dbref.update(_.updated(eggz.id, eggz))
      udtd <- dbref.get
    } yield this
  override def get(id: ID): IO[GLOBZ_ERR, Option[GLOBZ_IN]] =
    for {
      r <- dbref.get
    } yield r.get(id)

  override def remove(id: ID): IO[GLOBZ_ERR, Unit] =
    for {

      egg <- dbref.get
        .map(_.get(id))
        .flatMap(x => ZIO.fromOption(x))
        .flatMapError(_ => ???)
      _ <- unrelateAll(egg, 0)
      _ <- dbref.update(_.removed(id))

    } yield ()

  override def tickAll(): ZIO[Any, GLOBZ_ERR, ExitCode] =
    for {
      all <- getAll()
      e <- ZIO.foreachPar(all)(
        _.op
          .fold(_ => ExitCode.failure, x => x)
          .provide(ZLayer {
            ZIO.succeed(this)
          })
      )
      fail = e.filter(_ != ExitCode.success).size
    } yield ExitCode.apply(fail)

  override def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]] =
    for {
      ref <- dbref.get
    } yield ref.values.toSet

  override def relate(
    egg1: GLOBZ_IN,
    egg2: GLOBZ_IN,
    bidirectional: Boolean
  ): IO[GLOBZ_ERR, Unit] =
    for {
      _ <- {
        relationRef
          .update(_.updated((egg1.id, egg2.id), true))
      } *> {
        (for {
          _ <- relationRef.update(_.updated((egg2.id, egg1.id), true))
        } yield ()).when(bidirectional)
      }
    } yield ()

  override def unrelate(
    egg1: scala.entity.Globz.GLOBZ_IN,
    egg2: scala.entity.Globz.GLOBZ_IN,
    bidirectional: Boolean
  ): IO[GLOBZ_ERR, Unit] =
    for {
      _ <- relationRef.update(_.updated((egg1.id, egg2.id), false))
    } yield ()

  override def unrelateAll(
    egg: entity.Globz.GLOBZ_IN,
    direction: Int
  ): IO[GLOBZ_ERR, Unit] =
    for {
      neighbors <- neighbors(egg, direction)
      _ <- ZIO.foreachPar(neighbors) { n =>
        if (direction > 0) {
          unrelate(egg, n, false)
        } else if (direction < 0) {
          unrelate(n, egg, false)
        } else unrelate(egg, n, true)
      }
    } yield ()
  override def neighbors(
    egg: Eggz.Service,
    direction: Int
  ): IO[GLOBZ_ERR, Vector[GLOBZ_IN]] =
    (for {
      rels <- relationRef.get
      tasks = rels.toVector
        .collect {
          case ((id1, id2), related) if id1 == egg.id && related => id2
          case ((id1, id2), related) if id2 == egg.id && related => id1
        }
        .map(x => get(x))
      k <- ZIO.collectAllPar(tasks)
    } yield k.collect { case Some(g) => g }).mapError(_ => "whoops")

  override def scheduleEgg(
    egg: GLOBZ_IN,
    op: ZIO[GLOBZ_IN, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] =
    (for {
      _ <- get(egg.id)
        .flatMap(
          _.map(eg => op.provide(ZLayer.succeed(egg)))
            .getOrElse(ZIO.succeed(ExitCode.failure))
        )
        .flatMapError(err => ZIO.unit)
    } yield ())
      .fold(
        err => (),
        x => x
      )
      .repeat(Schedule.spaced(fromMillis(1000)))
      // .provide(ZLayer.succeed(this))
      .fork
      .map(_ => ())

  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] =
    for {
      eggs <- this.dbref.get
      mapped <- ZIO
        .collectAllPar(eggs.map { case (id, egg) =>
          egg.serializeEgg
        })
        .mapError(_ => s"Error while trying to serialize glob ${this.id}")
      rels <- this.relationRef.get
    } yield GlobInMemory(
      this.id,
      mapped.toSet,
      rels.toSet.collect[(ID, ID)] {
        case (ids: (ID, ID), related: Boolean) if related => ids
      }
    )

}
object GlobzInMem extends Globz.Service {
  type EggMap = Ref[Map[GLOBZ_ID, Eggz.Service]]
  type RelationMap = Ref[Map[(GLOBZ_ID, GLOBZ_ID), Boolean]]
  override def make(id: GLOBZ_ID): IO[GLOBZ_ERR, Globz] =
    for {
      r <- Ref.make(Map.empty[GLOBZ_ID, Eggz.Service])
      k <- Ref.make(Map.empty[(GLOBZ_ID, GLOBZ_ID), Boolean])
    } yield GlobzInMem(id, r, k)

}
