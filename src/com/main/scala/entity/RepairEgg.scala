package src.com.main.scala.entity

import src.com.main.scala.entity.Eggz.GenericEggzError
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.Globz
import src.com.main.scala.entity.Storage.GenericServiceError
import zio.ExitCode
import zio.IO
import zio.ZIO

case class RepairEgg(
  val id: ID,
  health: Double,
  repairValue: Double,
  energy: Double,
  cost: Double,
  top: Option[ID] = None,
  bottom: Option[ID] = None,
  right: Option[ID] = None,
  left: Option[ID] = None,
  inventory: Option[Storage.Service[String]] = Some(basicStorage[String](Set()))
) extends StorageEgg[String] {

  override def op: ZIO[Globz, String, ExitCode] =
    if (energy <= cost) {
      ZIO.succeed(ExitCode.success)
    } else
      for {
        updatedSelf <- this
          .setHealth(health + repairValue)
          .flatMap(_.setEnergy(this.energy - cost))
          .flatMap {
            case stor: Storage.Service[String] =>
              stor.add(s"Set health and energy values: ${stor.health}:${stor.energy}")
          }
          .fold[Eggz.Service](_ => this, { case x: Eggz.Service => x })
        ud <- Globz.update(updatedSelf)
        t <- ZIO
          .fromOption(top)
          .flatMap(Globz.get(_))
          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
          .mapError(_ => "no value for top")
        b <- ZIO
          .fromOption(bottom)
          .flatMap(Globz.get(_))
          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
          .mapError(_ => "error getting right")
        l <- ZIO
          .fromOption(left)
          .flatMap(Globz.get(_))
          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
          .mapError(_ => "error")
        r <- ZIO
          .fromOption(right)
          .flatMap(Globz.get(_))
          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
          .mapError(_ => "error")

      } yield ExitCode.success

  override def setHealth(health: Double): IO[Eggz.EggzError, Eggz.Service] =
    IO {
      this.copy(health = health)
    }.mapError(_ => GenericEggzError("failed set health"))

  override def setEnergy(value: Double): IO[Eggz.EggzError, Eggz.Service] =
    IO {
      this.copy(energy = value)
    }.mapError(_ => GenericEggzError("failed set energy"))

  override def add(item: String*): IO[Storage.ServiceError, Storage.Service[String]] =
    (for {
      r <- ZIO.fromOption(inventory).flatMap(_.add(item: _*))
    } yield this.copy(inventory = Some(r))).mapError(_ => GenericServiceError(""))

  override def remove(item: String*): IO[Storage.ServiceError, Storage.Service[String]] =
    (for {
      r <- ZIO.fromOption(inventory).flatMap(_.remove(item: _*))
    } yield this.copy(inventory = Some(r))).mapError(_ => GenericServiceError(""))

  override def getAll(): IO[Storage.ServiceError, Set[String]] =
    (for {
      i <- ZIO.fromOption(inventory)
      inv <- i.getAll()
    } yield inv).mapError(_ => GenericServiceError("error fetching inventory"))
}

object RepairEgg {
  def apply(id: ID, health: Double, repairValue: Double): Eggz.Service =
    RepairEgg(id, health, repairValue, 1000, 20, None, None, None, None)

  def op(egg: Eggz.Service): ZIO[Globz, GLOBZ_ERR, ExitCode] = egg.op
}

class processingEgg() // proocesses resource to next stage
class combinationEgg() //combines materials from adjacent eggs to create new material
class storageEgg() // stores items and materials in egg
class pipeEgg() //transport invetory from egg to egg
class energyCreateEgg() // manifests energy from external resource
