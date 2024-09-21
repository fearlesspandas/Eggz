package entity

import zio.IO

trait Health {
  def setHealth(health: Double): IO[HealthError, Health]

  def setEnergy(value: Double): IO[HealthError, Health]

  def health: IO[HealthError, Double]

  def energy: IO[HealthError, Double]

}

trait HealthError
