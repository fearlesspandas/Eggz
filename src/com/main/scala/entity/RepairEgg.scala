package src.com.main.scala.entity

import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.{GLOBZ_ERR, GLOBZ_IN, Globz}
import zio.{ExitCode, ZIO}

case class RepairEgg(
                      val id: ID,
                      health: Double,
                      repairValue: Double,
                      energy: Double,
                      cost: Double,
                      top: Option[ID],
                      bottom: Option[ID],
                      right: Option[ID],
                      left: Option[ID]
                    ) extends Eggz {
  val inventory = None

  override def op: ZIO[Globz, String, ExitCode] =
    if (energy <= cost) {
      ZIO.succeed(ExitCode.success)
    } else
      for {
        ud <- Globz.update(this.setHealth(health + repairValue))
        e <- Globz.update(this.setEnergy(energy - cost))
        t <- ZIO.fromOption(top).flatMap(Globz.get(_)).mapError(_ => "no value for top")
        b <- ZIO.fromOption(bottom).flatMap(Globz.get(_)).mapError(_ => "error getting right")
        l <- ZIO.fromOption(left).flatMap(Globz.get(_)).mapError(_ => "error")
        r <- ZIO.fromOption(right).flatMap(Globz.get(_)).mapError(_ => "error")
        ut <- ZIO.fromOption(t).flatMap(egg => Globz.update(egg.setHealth(egg.health + repairValue))).mapError(_ => "error")
        _ <- ZIO.fromOption(b).flatMap(egg => Globz.update(egg.setHealth(egg.health + repairValue))).mapError(_ => "error")
        _ <- ZIO.fromOption(l).flatMap(egg => Globz.update(egg.setHealth(egg.health + repairValue))).mapError(_ => "error")
        _ <- ZIO.fromOption(r).flatMap(egg => Globz.update(egg.setHealth(egg.health + repairValue))).mapError(_ => "error")
      } yield ExitCode.success

  override def setHealth(health: Double): GLOBZ_IN = this.copy(health = health)

  override def setEnergy(value: Double): GLOBZ_IN = this.copy(energy = value)
}

object RepairEgg {
  def apply(id: ID, health: Double, repairValue: Double): Eggz = RepairEgg(id, health, repairValue, 0, 20, None, None, None, None)

  def op(egg: Eggz): ZIO[Globz, GLOBZ_ERR, ExitCode] = egg.op
}


class processingEgg() // proocesses resource to next stage
class combinationEgg() //combines materials from adjacent eggs to create new material
class storageEgg() // stores items and materials in egg
class pipeEgg() //transport invetory from egg to egg
class energyCreateEgg() // manifests energy from external resource
