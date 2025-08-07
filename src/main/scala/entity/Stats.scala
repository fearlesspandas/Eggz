package entity
import zio.*

enum StatType{
  case health;
  case speed;
}
object StatType{
  def to_int(typ:StatType):Int = {
    typ match {
      case StatType.health => 0
      case StatType.speed => 1
    }
  }
  def from_int(item:Int):Option[StatType] = {
    item match{
      case 0 => Some(StatType.health)
      case 1 => Some(StatType.speed)
      case _ => None
    }
  }
}

trait Stats{
  def setStat(typ:StatType,value:Double):UIO[Unit]
  def getStat(typ:StatType):UIO[Option[Double]]
}

trait StatsError
