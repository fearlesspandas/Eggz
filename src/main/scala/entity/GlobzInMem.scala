package entity
import zio.*
import entity.EggzOps.ID
import entity.Globz.GLOBZ_ERR
import entity.Globz.GLOBZ_ID
import entity.Globz.GLOBZ_IN
import entity.Globz.GLOBZ_OUT
import entity.GlobzInMem.EggMap
import entity.GlobzInMem.RelationProcessMap
import zio.Duration.fromMillis
case class GlobzInMem(
  id: GLOBZ_ID,
  dbref: EggMap,
  relationRef: RelationProcessMap
) extends Globz {

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
      _ <- unrelateAll(id, 0, ZIO.unit)
      _ <- dbref.update(_.removed(id))
    } yield ()

  override def getAll(): IO[GLOBZ_ERR, Set[GLOBZ_IN]] =
    for {
      ref <- dbref.get
    } yield ref.values.toSet

  override def relate(
    egg1: GLOBZ_ID,
    egg2: GLOBZ_ID,
    bidirectional: Boolean,
    process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] =
    // curently we dont restart a process for a key pair that's already present
    // instead we just keep the existing one
    // may change this in the future
    (for {
      f <- process.fork
      _ <- {
        relationRef
          .update(_.updated((egg1, egg2), f))
      } *> {
        (for {
          _ <- relationRef.update(_.updated((egg2, egg1), f))
        } yield ()).when(bidirectional)
      }
    } yield ()).whenZIO(relationRef.get.map(!_.contains((egg1, egg2)))).unit

  override def unrelate(
    egg1: GLOBZ_ID,
    egg2: GLOBZ_ID,
    bidirectional: Boolean,
    cleanup_process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] =
    for {
      f <- relationRef.get
        .map(_.get((egg1, egg2)))
      _ <- ZIO.fromOption(f).foldZIO(_ => ZIO.unit, _.interrupt)
      _ <- relationRef.update(
        _.removed((egg1, egg2))
      ) *> relationRef
        .update(_.removed((egg2, egg1)))
    } yield ()

  override def unrelateAll(
    egg: GLOBZ_ID,
    direction: Int,
    cleanup_process: ZIO[Any, GLOBZ_ERR, Unit]
  ): IO[GLOBZ_ERR, Unit] =
    for {
      neighbors <- neighbors(egg, direction)
      _ <- ZIO.foreachParDiscard(neighbors) { n =>
        if (direction > 0) {
          unrelate(egg, n, false, cleanup_process)
        } else if (direction < 0) {
          unrelate(n, egg, false, cleanup_process)
        } else unrelate(egg, n, true, cleanup_process)
      }
    } yield ()

  override def neighbors(
    egg: GLOBZ_ID,
    direction: Int
  ): IO[GLOBZ_ERR, Vector[GLOBZ_ID]] =
    for {
      rels <- relationRef.get
      tasks = rels.toVector
        .collect {
          case ((id1, id2), related) if id1 == egg => id2
          case ((id1, id2), related) if id2 == egg => id1
        }
    } yield tasks

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
      .repeat(Schedule.spaced(fromMillis(300)))
      .unit
    // .provide(ZLayer.succeed(this))
//      .fork
  override def serializeGlob: IO[GLOBZ_ERR, GlobzModel] = ???
}
object GlobzInMem extends Globz.Service {
  type EggMap = Ref[Map[GLOBZ_ID, Eggz.Service]]
  type RelationMap = Ref[Map[(GLOBZ_ID, GLOBZ_ID), Boolean]]
  type RelationProcessMap =
    Ref[Map[(GLOBZ_ID, GLOBZ_ID), Fiber.Runtime[GLOBZ_ERR, Unit]]]
  override def make(id: GLOBZ_ID): IO[GLOBZ_ERR, Globz] =
    for {
      r <- Ref.make(Map.empty[GLOBZ_ID, Eggz.Service])
      k <- Ref.make(
        Map.empty[(GLOBZ_ID, GLOBZ_ID), Fiber.Runtime[GLOBZ_ERR, Unit]]
      )
    } yield GlobzInMem(id, r, k)

}
