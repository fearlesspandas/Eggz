package physics

import entity.PhysicsError
import zio.Chunk
import zio.IO
import zio.Ref
import zio.ZIO
import zio.stream.SubscriptionRef

trait Destinations {
  def addDestination(dest: Destination): IO[DestinationsError, Unit]
  def getNextDestination(): IO[DestinationsError, Option[Destination]]
  def getAllDestinations(): IO[DestinationsError, Seq[Destination]]
  def popNextDestination(): IO[DestinationsError, Option[Destination]]
  def clearDestinations(): IO[DestinationsError, Unit]
  def toggleDestinations(): IO[Destinations, Unit]
  def isActive(): IO[DestinationsError, Boolean]
}
trait DestinationsError extends PhysicsError
object Destinations {
  trait Service {
    def make(): IO[DestinationsError, Destinations]
  }
  def make(): ZIO[Destinations.Service, DestinationsError, Destinations] =
    ZIO.service[Destinations.Service].flatMap(_.make())
}

case class BasicDestinations(
  destinations: SubscriptionRef[Chunk[Destination]],
  active: Ref[Boolean]
) extends Destinations {
  override def addDestination(dest: Destination): IO[DestinationsError, Unit] =
    destinations.update(_.appended(dest))

  override def getNextDestination()
    : IO[DestinationsError, Option[Destination]] =
    destinations.get
      .map(_.headOption)

  override def getAllDestinations(): IO[DestinationsError, Seq[Destination]] =
    destinations.get

  override def popNextDestination()
    : IO[DestinationsError, Option[Destination]] =
    destinations.get.flatMap(dests =>
      for {
        _ <- destinations.update(_.tail)
      } yield dests.headOption
    )
  override def clearDestinations(): IO[DestinationsError, Unit] =
    destinations.update(_ => Chunk.empty)

  override def toggleDestinations(): IO[Destinations, Unit] = active.update(!_)

  override def isActive(): IO[DestinationsError, Boolean] = active.get
}
object BasicDestinations extends Destinations.Service {
  override def make(): IO[Nothing, Destinations] =
    for {
      ref <- SubscriptionRef.make(Chunk.empty[Destination])
      isactive <- Ref.make(false)
    } yield BasicDestinations(ref, isactive)
}
