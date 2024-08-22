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

trait Destinations {
  def addDestination(dest: Destination): IO[DestinationsError, Unit]
  def getNextDestination(): IO[DestinationsError, Option[Destination]]
  def getAllDestinations(): IO[DestinationsError, Seq[Destination]]
  def popNextDestination(): IO[DestinationsError, Option[Destination]]
  def clearDestinations(): IO[DestinationsError, Unit]
  def toggleDestinations(): IO[DestinationsError, Unit]
  def isActive(): IO[DestinationsError, Boolean]
  def toggleGravitate(): IO[DestinationsError, Unit]
  def isGravitating(): IO[DestinationsError, Boolean]
  def setMode(mode: Mode): IO[DestinationsError, Unit]
  def getMode(): IO[DestinationsError, Mode]
  def getIndex(): IO[DestinationsError, Int]
  def setIndex(value: Int): IO[DestinationsError, Unit]
  def getDestAtIndex(): IO[DestinationsError, Option[Destination]]
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
        getDestAtIndex()
      case REVERSE =>
        getDestAtIndex()
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
        increment() *> getDestAtIndex()
      case REVERSE =>
        decrement() *> getDestAtIndex()
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

  override def getDestAtIndex(): IO[DestinationsError, Option[Destination]] =
    for {
      dest_ch <- destinations.get
      ind <- index.get
      res = dest_ch.lift(ind)
    } yield res

  override def setIndex(value: Int): IO[DestinationsError, Unit] =
    destinations.get.flatMap(dests =>
      index.update(_ => value).when(value < dests.size).unit
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
