package src.com.main.scala.entity

import src.com.main.scala.entity.Eggz.GenericEggzError
import src.com.main.scala.entity.EggzOps.ID
import src.com.main.scala.entity.Globz.GLOBZ_ERR
//import src.com.main.scala.entity.Globz.Globz
import src.com.main.scala.entity.Storage.GenericServiceError
import zio.ExitCode
import zio.IO
import zio.Ref

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

  override def op: ZIO[Globz.Service, String, ExitCode] =
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
        ud <- ZIO.service[Globz.Service].flatMap(_.update(updatedSelf))
//        t <- ZIO
//          .fromOption(top)
//          .flatMap(Globz.get(_))
//          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
//          .mapError(_ => "no value for top")
//        b <- ZIO
//          .fromOption(bottom)
//          .flatMap(Globz.get(_))
//          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
//          .mapError(_ => "error getting right")
//        l <- ZIO
//          .fromOption(left)
//          .flatMap(Globz.get(_))
//          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
//          .mapError(_ => "error")
//        r <- ZIO
//          .fromOption(right)
//          .flatMap(Globz.get(_))
//          .flatMap(x => ZIO.fromOption(x.map(e => e.setHealth(e.health + repairValue))))
//          .mapError(_ => "error")

      } yield ExitCode.success

  override def setHealth(health: Double): IO[Eggz.EggzError, Eggz.Service] =
    ZIO
      .succeed {
        this.copy(health = health)
      }
  //.mapError(_ => GenericEggzError("failed set health"))

  override def setEnergy(value: Double): IO[Eggz.EggzError, Eggz.Service] =
    ZIO
      .succeed {
        this.copy(energy = value)
      }
  //.mapError(_ => GenericEggzError("failed set energy"))

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

  override def op2(glob: Ref[Globz.Service]): ZIO[Ref[Globz.Service], GLOBZ_ERR, ExitCode] =
    (for {
      g <- glob.get
      updated <- this
        .setHealth(health + repairValue)
        .flatMap(_.setEnergy(energy - cost))
        .flatMap {
          case stor: Storage.Service[String] => stor.add("added stuff")
        }
        .fold[Eggz.Service](_ => this, { case x: Eggz.Service => x })

      //fix so that None's don't affect flow
      t <- ZIO
        .fromOption(top)
        .flatMap(g.get(_))
        .flatMap { case Some(egg) => egg.setHealth(egg.health + repairValue); }
        .mapError(_ => "")
        .fold(_ => updated, x => x) //this is hacky but should work if adjacents are None
      b <- ZIO
        .fromOption(top)
        .flatMap(g.get(_))
        .flatMap { case Some(egg) => egg.setHealth(egg.health + repairValue); }
        .mapError(_ => "")
        .fold(_ => updated, x => x) //this is hacky but should work if adjacents are None
      l <- ZIO
        .fromOption(top)
        .flatMap(g.get(_))
        .flatMap { case Some(egg) => egg.setHealth(egg.health + repairValue); }
        .mapError(_ => "")
        .fold(_ => updated, x => x) //this is hacky but should work if adjacents are None
      r <- ZIO
        .fromOption(top)
        .flatMap(g.get(_))
        .flatMap { case Some(egg) => egg.setHealth(egg.health + repairValue); }
        .mapError(_ => "")
        .fold(_ => updated, x => x) //this is hacky but should work if adjacents are None
      gup <- g
        .update(updated)
        .flatMap(_.update(t))
        .flatMap(_.update(b))
        .flatMap(_.update(r))
        .flatMap(_.update(l))
      _ <- glob.update(_ => gup)
    } yield ExitCode.success)
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
  def apply(id: ID, health: Double, repairValue: Double): Eggz.Service =
    RepairEgg(id, health, repairValue, 1000, 20, None, None, None, None)

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
