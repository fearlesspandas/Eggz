package entity
//import src.com.main.scala.entity.Globz
import entity.Eggz.EggzError
import entity.EggzOps.ID
import entity.Storage.REF_STORE
import zio.*

case class RepairEgg(
  val id: ID,
  override val storage: REF_STORE[String],
  healthRef: Ref[Double],
  repairValue: Double,
  energyRef: Ref[Double],
  cost: Double
) extends Storage.Service[String]
    with Health {

  override def setHealth(health: Double): IO[HealthError, Health] =
    for {
      _ <- this.healthRef.update(_ => health)
    } yield this

  override def setEnergy(value: Double): IO[HealthError, Health] =
    for {
      _ <- this.energyRef.update(_ => value)
    } yield this

  override def health: IO[HealthError, Double] = healthRef.get

  override def energy: IO[HealthError, Double] = energyRef.get

}

object RepairEgg {
  def make(
    id: ID,
    health: Double,
    repairValue: Double
  ): IO[Nothing, RepairEgg] =
    for {
      h <- Ref.make(health)
      e <- Ref.make(10000.0)
      inventory <- Storage.make[String]
    } yield RepairEgg(
      id,
      inventory,
      h,
      repairValue,
      e,
      20
    )

}
case object RepairEggStatsNotFound extends EggzError
class processingEgg() // proocesses resource to next stage
class combinationEgg() //combines materials from adjacent eggs to create new material
class storageEgg() // stores items and materials in egg
class pipeEgg() //transport invetory from egg to egg
class energyCreateEgg() // manifests energy from external resource
