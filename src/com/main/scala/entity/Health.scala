package entity

import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.IO

trait Health {
  def setHealth(health: Double): IO[HealthError, Health]

  def setEnergy(value: Double): IO[HealthError, Health]

  def health: IO[HealthError, Double]

  def energy: IO[HealthError, Double]

}

trait InstanceHealth {
  def setHealth(
    player_id: GLOBZ_ID,
    value: Double
  ): IO[HealthError, InstanceHealth]

  def setEnergy(
    player_id: GLOBZ_ID,
    value: Double
  ): IO[HealthError, InstanceHealth]

  def health(player_id: GLOBZ_ID): IO[HealthError, Double]

  def energy(player_id: GLOBZ_ID): IO[HealthError, Double]

}
trait HealthError
