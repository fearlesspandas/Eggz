package src.com.main.scala.entity

import controller.Stats
import entity.EggzModel
import entity.REPAIR_EGG
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
  inventory: Ref[Storage.Service[String]]
) extends StorageEgg[String] {
  override def op: ZIO[Globz, String, ExitCode] =
    this.energyRef.get.flatMap(e =>
      this.healthRef.get.flatMap { h =>
        if (e <= cost) {
          ZIO.succeed(ExitCode.success)
        } else
          for {
            // repairs self
            _ <- this
              .setHealth(h + repairValue)
              .flatMap(_.setEnergy(e - cost))
              .flatMap { case stor: Storage.Service[String] =>
                stor.add(
                  s"Set health and energy values: ${stor.health}:${stor.energy}"
                )
              }
              .fold[Eggz.Service](_ => this, { case x: Eggz.Service => x })
            ud <- ZIO
              .serviceWithZIO[Globz](glob => glob.neighbors(this, 0))
              .flatMap { neighbors =>
                ZIO.foreachPar(neighbors) { egg =>
                  for {
                    h_curr <- egg.health
                    up <- egg.setHealth(h_curr + repairValue)
                  } yield up
                }
              }
              .mapError(_ => "")

          } yield ExitCode.success
      }
    )

  override def setHealth(health: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- this.healthRef.update(_ => health)
    } yield this

  override def setEnergy(value: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- this.energyRef.update(_ => value)
    } yield this

  override def add(
    item: String*
  ): IO[Storage.ServiceError, Storage.Service[String]] =
    (for {
      i <- inventory.get
      iUP <- i.add(item: _*)
      r <- inventory.update(_ => iUP)
    } yield this).mapError(_ => GenericServiceError(""))

  override def remove(
    item: String*
  ): IO[Storage.ServiceError, Storage.Service[String]] =
    (for {
      i <- inventory.get
      iUp <- i.remove(item: _*)
      r <- inventory.update(_ => iUp)
    } yield this).mapError(_ => GenericServiceError(""))

  override def getInventory(): IO[Storage.ServiceError, Set[String]] =
    (for {
      i <- inventory.get
      inv <- i.getInventory()
    } yield inv).mapError(_ => GenericServiceError("error fetching inventory"))

  override def health: IO[Eggz.EggzError, Double] = healthRef.get

  override def energy: IO[Eggz.EggzError, Double] = energyRef.get

  override def serializeEgg: IO[Eggz.EggzError, EggzModel] =
    for {
      health <- health
      energy <- energy
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
      bs <- Ref.make(basicStorage[String](Set()))
    } yield RepairEgg(
      id,
      h,
      repairValue,
      e,
      20,
      bs.asInstanceOf[Ref[Storage.Service[String]]]
    )

  def op(egg: Eggz.Service): ZIO[Globz, GLOBZ_ERR, ExitCode] = egg.op
}

class processingEgg() // proocesses resource to next stage
class combinationEgg() //combines materials from adjacent eggs to create new material
class storageEgg() // stores items and materials in egg
class pipeEgg() //transport invetory from egg to egg
class energyCreateEgg() // manifests energy from external resource
