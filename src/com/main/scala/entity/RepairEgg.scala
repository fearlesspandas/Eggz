package src.com.main.scala.entity

import src.com.main.scala.entity.Eggz.GenericEggzError
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
import src.com.main.scala.entity.Globz.GLOBZ_ID
//import src.com.main.scala.entity.Globz.Globz
import src.com.main.scala.entity.Storage.GenericServiceError
import zio.ExitCode
import zio.IO
import zio.Ref

import zio.ZIO

case class RepairEgg(
  val id: ID,
  health: Ref[Double],
  repairValue: Double,
  energy: Ref[Double],
  cost: Double,
  inventory: Ref[Storage.Service[String]]
) extends StorageEgg[String] {
  def handleAdjacents(
    adj: Option[GLOBZ_ID],
    default: Eggz.Service
  ): ZIO[Globz.Service, Nothing, Eggz.Service] =
    ZIO.service[Globz.Service].flatMap { glob =>
      for {
        e <- ZIO
          .fromOption(adj)
          .flatMap(glob.get(_))
          .fold[Eggz.Service](_ => default, x => x.getOrElse(default))
      } yield e
    }
  override def op: ZIO[Globz.Service, String, ExitCode] =
    this.energy.get.flatMap(e =>
      this.health.get.flatMap { h =>
        if (e <= cost) {
          ZIO.succeed(ExitCode.success)
        } else
          for {
            //repairs self
            _ <- this
              .setHealth(h + repairValue)
              .flatMap(_.setEnergy(e - cost))
              .flatMap {
                case stor: Storage.Service[String] =>
                  stor.add(s"Set health and energy values: ${stor.health}:${stor.energy}")
              }
              .fold[Eggz.Service](_ => this, { case x: Eggz.Service => x })
            ud <- ZIO
              .service[Globz.Service]
              .flatMap(glob => glob.neighbors(this))
              .flatMap { neighbors =>
                ZIO.collectAllPar(
                  neighbors.map(egg =>
                    for {
                      h_curr <- egg.health.get
                      up <- egg.setHealth(h_curr + repairValue)
                    } yield up
                  )
                )
              }
              .mapError(_ => "")

          } yield ExitCode.success
      }
    )

  override def setHealth(health: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- this.health.update(_ => health)
    } yield this
  //.mapError(_ => GenericEggzError("failed set health"))

  override def setEnergy(value: Double): IO[Eggz.EggzError, Eggz.Service] =
    for {
      _ <- this.energy.update(_ => value)
    } yield this
  //.mapError(_ => GenericEggzError("failed set energy"))

  override def add(item: String*): IO[Storage.ServiceError, Storage.Service[String]] =
    (for {
      i <- inventory.get
      iUP <- i.add(item: _*)
      r <- inventory.update(_ => iUP)
    } yield this).mapError(_ => GenericServiceError(""))

  override def remove(item: String*): IO[Storage.ServiceError, Storage.Service[String]] =
    (for {
      i <- inventory.get
      iUp <- i.remove(item: _*)
      r <- inventory.update(_ => iUp)
    } yield this).mapError(_ => GenericServiceError(""))

  override def getAll(): IO[Storage.ServiceError, Set[String]] =
    (for {
      i <- inventory.get
      inv <- i.getAll()
    } yield inv).mapError(_ => GenericServiceError("error fetching inventory"))
//
//    ZIO.environmentWithZIO[Ref[Globz.Service]] { ref =>
//      val r = ref.get
//      for {
//        orig <- r.get
//        updated <- this
//          .setHealth(this.health + this.repairValue)
//          .flatMap(_.setEnergy(this.energy - this.cost))
//          .flatMap {
//            case stor: Storage.Service[String] => stor.add("set health")
//          }
//      } yield ExitCode.success
//
//    }

}

object RepairEgg {
  def make(id: ID, health: Double, repairValue: Double): IO[Nothing, Eggz.Service] =
    for {
      h <- Ref.make(health)
      e <- Ref.make(1000.0)
      bs <- Ref.make(basicStorage[String](Set()))
    } yield RepairEgg(id, h, repairValue, e, 20, bs.asInstanceOf[Ref[Storage.Service[String]]])

  def op(egg: Eggz.Service): ZIO[Globz.Service, GLOBZ_ERR, ExitCode] = egg.op

//  def adjacentOps(value:Option[Eggz.Service]) =
//    ZIO
//      .fromOption(value)
//      .flatMap(g.get(_))
//      .flatMap { case Some(egg) => egg.setHealth(egg.health + repairValue); }
//      .mapError(_ => "")
//      .fold(_ => updated, x => x) //this is hacky but should work if adjacents are None
}

class processingEgg() // proocesses resource to next stage
class combinationEgg() //combines materials from adjacent eggs to create new material
class storageEgg() // stores items and materials in egg
class pipeEgg() //transport invetory from egg to egg
class energyCreateEgg() // manifests energy from external resource
