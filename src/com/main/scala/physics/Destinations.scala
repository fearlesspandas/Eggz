package physics

import entity.PhysicsError
import zio.IO
import zio.Ref
import zio.ZIO

trait Destinations {
  def addDestination(location: Vector[Double]): IO[DestinationError, Unit]
  def getNextDestination(): IO[DestinationError, Option[Vector[Double]]]
  def getAllDestinations(): IO[DestinationError, Seq[Vector[Double]]]
  def popNextDestination(): IO[DestinationError, Option[Vector[Double]]]
  def clearDestinations(): IO[DestinationError, Unit]
}
trait DestinationError extends PhysicsError
object Destinations {
  trait Service {
    def make(): IO[DestinationError, Destinations]
  }
  def make(): ZIO[Destinations.Service, DestinationError, Destinations] =
    ZIO.service[Destinations.Service].flatMap(_.make())
}

case class BasicDestinations(
  destinations: Ref[Seq[Vector[Double]]]
) extends Destinations {
  override def addDestination(location: Vector[Double]): IO[DestinationError, Unit] =
    destinations.update(_.appended(location))

  override def getNextDestination(): IO[DestinationError, Option[Vector[Double]]] =
    destinations.get
      .map(_.headOption)

  override def getAllDestinations(): IO[DestinationError, Seq[Vector[Double]]] = destinations.get

  override def popNextDestination(): IO[DestinationError, Option[Vector[Double]]] =
    destinations.get.flatMap(dests =>
      for {
        _ <- destinations.update(_.tail)
      } yield dests.headOption
    )

  override def clearDestinations(): IO[DestinationError, Unit] = destinations.update(_ => Seq())
}
object BasicDestinations extends Destinations.Service {
  override def make(): IO[Nothing, Destinations] =
    for {
      ref <- Ref.make(Seq.empty[Vector[Double]])
    } yield BasicDestinations(ref)
}
