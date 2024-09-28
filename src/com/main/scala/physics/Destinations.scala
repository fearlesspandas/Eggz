package physics

import entity.PhysicsError
import physics.Mode.FORWARD
import physics.Mode.POP
import physics.Mode.REVERSE
import zio.Chunk
import zio.IO
import zio.Ref
import zio.ZIO
import zio.json.DeriveJsonDecoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder
import zio.json.JsonEncoder
import zio.stream.SubscriptionRef

import java.util.UUID

trait Destinations {
  def addDestination(dest: Destination): IO[DestinationsError, Unit]
  def getNextDestination(): IO[DestinationsError, Option[Destination]]
  def getAllDestinations(): IO[DestinationsError, Seq[Destination]]
  def popNextDestination(): IO[DestinationsError, Option[Destination]]
  def clearDestinations(): IO[DestinationsError, Unit]
  def toggleDestinations(): IO[DestinationsError, Unit]
  def setIsActive(value: Boolean): IO[DestinationsError, Unit]
  def isActive(): IO[DestinationsError, Boolean]
  def toggleGravitate(): IO[DestinationsError, Unit]
  def setGravitate(value: Boolean): IO[DestinationsError, Unit]
  def isGravitating(): IO[DestinationsError, Boolean]
  def setMode(mode: Mode): IO[DestinationsError, Unit]
  def getMode(): IO[DestinationsError, Mode]
  def getIndex(): IO[DestinationsError, Int]
  def setIndex(index: Int): IO[DestinationsError, Unit]
  def setActiveDest(id: UUID): IO[DestinationsError, Unit]
  def getDestAtIndex(ind: Int): IO[DestinationsError, Option[Destination]]
  def getDestAtCurrentIndex(): IO[DestinationsError, Option[Destination]]
  def deleteDest(uuid: UUID): IO[DestinationsError, Unit]
  def increment(): IO[DestinationsError, Unit]
  def decrement(): IO[DestinationsError, Unit]
}
trait DestinationsError extends PhysicsError
object Destinations {
  trait Service {
    def make(): IO[DestinationsError, Destinations]
  }
  def make(): ZIO[Destinations.Service, DestinationsError, Destinations] =
    ZIO.service[Destinations.Service].flatMap(_.make())
}
sealed trait Mode
object Mode {
  case object POP extends Mode
  case object FORWARD extends Mode
  case object REVERSE extends Mode
  implicit val encoder: JsonEncoder[Mode] = DeriveJsonEncoder.gen[Mode]

  implicit val decoder: JsonDecoder[Mode] = DeriveJsonDecoder.gen[Mode]
}
case class BasicDestinations(
  destinations: Ref[Chunk[Destination]],
  active: Ref[Boolean],
  gravitating: Ref[Boolean],
  mode: Ref[Mode],
  index: Ref[Int]
) extends Destinations {
  override def addDestination(dest: Destination): IO[DestinationsError, Unit] =
    destinations.update(_.appended(dest))

  override def getNextDestination()
    : IO[DestinationsError, Option[Destination]] =
    mode.get.flatMap {
      case POP =>
        destinations.get
          .map(_.headOption)
      case FORWARD =>
        getDestAtCurrentIndex()
      case REVERSE =>
        getDestAtCurrentIndex()
    }

  override def getAllDestinations(): IO[DestinationsError, Seq[Destination]] =
    destinations.get

  override def popNextDestination()
    : IO[DestinationsError, Option[Destination]] =
    mode.get.flatMap {
      case POP =>
        destinations.get.flatMap(dests =>
          for {
            _ <- destinations.update(_.tail)
          } yield dests.headOption
        )
      case FORWARD =>
        increment() *> getDestAtCurrentIndex()
      case REVERSE =>
        decrement() *> getDestAtCurrentIndex()
    }
  override def clearDestinations(): IO[DestinationsError, Unit] =
    destinations.update(_ => Chunk.empty)

  override def toggleDestinations(): IO[DestinationsError, Unit] =
    active.update(!_)

  override def isActive(): IO[DestinationsError, Boolean] = active.get

  override def toggleGravitate(): IO[DestinationsError, Unit] =
    gravitating.update(!_)

  override def isGravitating(): IO[DestinationsError, Boolean] = gravitating.get

  override def setMode(mode: Mode): IO[DestinationsError, Unit] =
    this.mode.update(_ => mode)

  override def getMode(): IO[DestinationsError, Mode] = mode.get

  override def getIndex(): IO[DestinationsError, Int] = index.get

  override def increment(): IO[DestinationsError, Unit] =
    destinations.get.flatMap(dests => index.update(x => (x + 1) % dests.size))

  override def decrement(): IO[DestinationsError, Unit] =
    destinations.get.flatMap(dests =>
      index.update(x => if (x == 0) dests.size - 1 else x - 1)
    )

  override def getDestAtIndex(
    ind: Int
  ): IO[DestinationsError, Option[Destination]] =
    destinations.get.map(_.lift(ind))

  override def getDestAtCurrentIndex()
    : IO[DestinationsError, Option[Destination]] =
    index.get.flatMap(getDestAtIndex)

  override def setActiveDest(id: UUID): IO[DestinationsError, Unit] =
    destinations.get.flatMap(ch =>
      for {
        newindex <- ZIO
          .fromOption(ch.find(_.uuid == id).map(ch.indexOf(_)))
          .mapError(_ => ???)
        _ <- index.update(_ => newindex)
      } yield ()
    )

  override def deleteDest(uuid: UUID): IO[DestinationsError, Unit] =
    for {
      _ <- destinations.update(ch => ch.filterNot(_.uuid == uuid))
      _ <- destinations.get.flatMap(dests =>
        index.update(math.min(_, dests.size))
      )
    } yield ()

  override def setIsActive(value: Boolean): IO[DestinationsError, Unit] =
    active.update(_ => value)

  override def setGravitate(value: Boolean): IO[DestinationsError, Unit] =
    gravitating.update(_ => value)

  override def setIndex(index: Int): IO[DestinationsError, Unit] =
    destinations.get.flatMap(dests =>
      this.index.update(_ => index).when(index < dests.size).unit
    )
}
object BasicDestinations extends Destinations.Service {
  override def make(): IO[Nothing, Destinations] =
    for {
      destinations <- Ref.make(Chunk.empty[Destination])
      isactive <- Ref.make(false)
      gravitating <- Ref.make(false)
      mode <- Ref.make[Mode](Mode.POP)
      index <- Ref.make(0)
    } yield BasicDestinations(destinations, isactive, gravitating, mode, index)
}
