package src.com.main.scala.entity

import controller.Stats
import entity.EggzModel
import entity.Health
import entity.HealthError
import entity.REPAIR_EGG
import src.com.main.scala.entity.Eggz.EggzError
import src.com.main.scala.entity.Eggz.GenericEggzError
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
//import src.com.main.scala.entity.Globz
import src.com.main.scala.entity.Storage.GenericServiceError
import zio.ExitCode
import zio.IO
import zio.Ref

import zio.ZIO

case class RepairEgg(
  val id: ID,
  healthRef: Ref[Double],
  repairValue: Double,
  energyRef: Ref[Double],
  cost: Double,
  inventory: Storage.Service[String]
) extends StorageEgg[String]
    with Health {
  override def op: ZIO[Globz, String, ExitCode] = ???

  override def setHealth(health: Double): IO[HealthError, Health] =
    for {
      _ <- this.healthRef.update(_ => health)
    } yield this

  override def setEnergy(value: Double): IO[HealthError, Health] =
    for {
      _ <- this.energyRef.update(_ => value)
    } yield this

  override def add(
    item: String*
  ): IO[Storage.ServiceError, Storage.Service[String]] =
    inventory.add(item: _*)

  override def remove(
    item: String*
  ): IO[Storage.ServiceError, Storage.Service[String]] =
    inventory.remove(item: _*)

  override def getInventory(): IO[Storage.ServiceError, Set[String]] =
    inventory.getInventory()

  override def health: IO[HealthError, Double] = healthRef.get

  override def energy: IO[HealthError, Double] = energyRef.get

  override def serializeEgg: IO[Eggz.EggzError, EggzModel] =
    for {
      health <- health.orElseFail(RepairEggStatsNotFound)
      energy <- energy.orElseFail(RepairEggStatsNotFound)
      stats = Stats(id, health, energy)
    } yield REPAIR_EGG(id, stats, cost, repairValue)
}

object RepairEgg {
  def make(
    id: ID,
    health: Double,
    repairValue: Double
  ): IO[Nothing, Eggz.Service] =
    for {
      h <- Ref.make(health)
      e <- Ref.make(10000.0)
      bs <- Storage.make[String]
    } yield RepairEgg(
      id,
      h,
      repairValue,
      e,
      20,
      bs
    )

  def op(egg: Eggz.Service): ZIO[Globz, GLOBZ_ERR, ExitCode] = egg.op
}
case object RepairEggStatsNotFound extends EggzError
class processingEgg() // proocesses resource to next stage
class combinationEgg() //combines materials from adjacent eggs to create new material
class storageEgg() // stores items and materials in egg
class pipeEgg() //transport invetory from egg to egg
class energyCreateEgg() // manifests energy from external resource
