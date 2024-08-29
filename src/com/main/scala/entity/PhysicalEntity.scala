package entity

import entity.BasicPhysicalEntity.make
import zio.ExitCode
import zio.IO
import zio.Ref
import zio.ZIO

trait PhysicalEntity {
  def getLocation: IO[PhysicsError, Vector[Double]]
  def getVelocity: IO[PhysicsError, Vector[Double]]
  def setVelocity(velocity: Vector[Double]): IO[PhysicsError, Unit]
  def teleport(location: Vector[Double]): IO[PhysicsError, Unit]
  @deprecated
  def setInputVec(vec: Vector[Double]): IO[PhysicsError, Unit]
  @deprecated
  def getInputVec: IO[PhysicsError, Option[Vector[Double]]]
  def adjustMaxSpeed(delta: Double): IO[PhysicsError, Unit]
  def getMaxSpeed: IO[PhysicsError, Double]
  def adjustSpeed(delta: Double): IO[PhysicsError, Unit]
  def getSpeed: IO[PhysicsError, Double]
}
trait PhysicsError
object PhysicalEntity {
  trait Service {
    def make: IO[Nothing, PhysicalEntity]
  }
}

case class BasicPhysicalEntity(
  location: Ref[Vector[Double]],
  velocity: Ref[Vector[Double]],
  input: Ref[Option[Vector[Double]]],
  max_speed: Ref[Double],
  speed: Ref[Double]
) extends PhysicalEntity {
  override def getLocation: IO[PhysicsError, Vector[Double]] = location.get

  override def getVelocity: IO[PhysicsError, Vector[Double]] = velocity.get

  override def setVelocity(vel: Vector[Double]): IO[PhysicsError, Unit] =
    velocity.update(_ => vel)

  override def teleport(loc: Vector[Double]): IO[PhysicsError, Unit] =
    location.update(_ => loc)

  override def setInputVec(vec: Vector[Double]): IO[PhysicsError, Unit] =
    input.update {
      case _ if vec.find(_ != 0).nonEmpty => Some(vec); case _ => None
    }

  override def getInputVec: IO[PhysicsError, Option[Vector[Double]]] =
    input.get

  override def adjustMaxSpeed(delta: Double): IO[PhysicsError, Unit] =
    max_speed.update(curr => Math.max(curr + delta, 0))

  override def getMaxSpeed: IO[PhysicsError, Double] = max_speed.get

  override def adjustSpeed(delta: Double): IO[PhysicsError, Unit] =
    for {
      ms <- max_speed.get
      _ <- speed.update(s => math.max(0, math.min(s + delta, ms)))
    } yield ()

  override def getSpeed: IO[PhysicsError, Double] = speed.get
}

object BasicPhysicalEntity extends PhysicalEntity.Service {
  override def make: IO[Nothing, PhysicalEntity] =
    for {
      loc <- Ref.make(Vector(0.0, 10, 0))
      vel <- Ref.make(Vector(0.0, 0, 0))
      inpt <- Ref.make[Option[Vector[Double]]](None)
      max_speed <- Ref.make(500.0)
      speed <- Ref.make(0.0)
    } yield BasicPhysicalEntity(loc, vel, inpt, max_speed, speed)
}
