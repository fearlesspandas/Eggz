package entity

import zio.ExitCode
import zio.IO
import zio.Ref
import zio.ZIO

trait PhysicalEntity {
  def getLocation: IO[PhysicsError, Vector[Double]]
  def getDestination: IO[PhysicsError, Option[Vector[Double]]]
  def setDestination(dest: Vector[Double]): IO[PhysicsError, Unit]
  def getVelocity: IO[PhysicsError, Vector[Double]]
  def setVelocity(velocity: Vector[Double]): IO[PhysicsError, Unit]
  def move(location: Vector[Double]): IO[PhysicsError, Unit]
  def teleport(location: Vector[Double]): IO[PhysicsError, Unit]
  def setInputVec(vec: Vector[Double]): IO[PhysicsError, Unit]
  def getInputVec(): IO[PhysicsError, Option[Vector[Double]]]
}
trait PhysicsError
object PhysicalEntity {
  trait Service {
    def make: IO[Nothing, PhysicalEntity]
  }
}

case class BasicPhysicalEntity(
  location: Ref[Vector[Double]],
  destination: Ref[Option[Vector[Double]]],
  velocity: Ref[Vector[Double]],
  input: Ref[Option[Vector[Double]]]
) extends PhysicalEntity {
  override def getLocation: IO[PhysicsError, Vector[Double]] = location.get

  override def getDestination: IO[PhysicsError, Option[Vector[Double]]] = destination.get

  override def setDestination(dest: Vector[Double]): IO[PhysicsError, Unit] =
    destination.update(_ => Some(dest))

  override def getVelocity: IO[PhysicsError, Vector[Double]] = velocity.get

  override def setVelocity(vel: Vector[Double]): IO[PhysicsError, Unit] =
    velocity.update(_ => vel)

  override def move(location: Vector[Double]): IO[PhysicsError, Unit] = ???
  //todo make this also remove destination based on internal dest epsilon
  override def teleport(loc: Vector[Double]): IO[PhysicsError, Unit] =
    location.update(_ => loc)

  override def setInputVec(vec: Vector[Double]): IO[PhysicsError, Unit] =
    input.update(_ => Some(vec))

  override def getInputVec(): IO[PhysicsError, Option[Vector[Double]]] = input.get
}

object BasicPhysicalEntity extends PhysicalEntity.Service {
  override def make: IO[Nothing, PhysicalEntity] =
    for {
      loc <- Ref.make(Vector(0.0, 10, 0))
      dest <- Ref.make(Option.empty[Vector[Double]])
      vel <- Ref.make(Vector(0.0, 0, 0))
      inpt <- Ref.make[Option[Vector[Double]]](None)
    } yield BasicPhysicalEntity(loc, dest, vel, inpt)
}
