package entity
import zio.*
import controller.Statsd

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
trait StatsNotifier{
  //StatTypes that will notify all relevant players
  //of a 'this' stat change
  val global_stat_notifiers : Set[StatType]
  //StatTypes that will notify any clients associated with 'this'
  val client_stat_notifiers : Set[StatType] = global_stat_notifiers

  def statsGlobalNotification[I](
    id:I,
    typ:StatType
  ):ZIO[Stats,StatsNotifierError,Option[Statsd]] = 
    ZIO.service[Stats]
      .flatMap(stats => stats.getStat(typ))
      .map( statOp => 
          statOp.map(stat => Statsd(
            id.toString(),
            Map((StatType.to_int(typ),stat)))
          )
      ).when(global_stat_notifiers.contains(typ))
      .flatMap(ZIO.fromOption(_))
      .orElseFail(NotGlobalNotifier(s"add $typ to global notifiers for ${this.toString()}"))

  def statsGlobalNotifications[I](
    id:I,
    types:Set[StatType]
  ):ZIO[Stats,StatsNotifierError,Option[Statsd]] = 
    ZIO.service[Stats].flatMap(stats => 
      ZIO.foreach(types)( typ => 
        for{
          stat <- stats.getStat(typ)
            .when(global_stat_notifiers.contains(typ))
            .flatMap(ZIO.fromOption(_))
            .orElseFail(NotGlobalNotifier(s"add $typ to global notifiers for ${this.toString()}"))
            .flatMap(ZIO.fromOption(_))
            .orElseFail(StatNotFound)
        }yield (StatType.to_int(typ),stat)
      )
  ).map(stats => 
    if (stats.size > 0) 
      Some(Statsd( id.toString(), stats.toMap[Int,Double])) 
    else None
  )
  def statsClientNotification[I](
    id:I,
    typ:StatType
  ):ZIO[Stats,StatsNotifierError,Option[Statsd]] = 
    ZIO.service[Stats]
      .flatMap(stats => stats.getStat(typ))
      .map( statOp => 
          statOp.map(stat => Statsd(
            id.toString(),
            Map((StatType.to_int(typ),stat)))
          )
      ).when(client_stat_notifiers.contains(typ))
      .flatMap(ZIO.fromOption(_))
      .orElseFail(NotGlobalNotifier(s"add $typ to global notifiers for ${this.toString()}"))

  def statsClientNotifications[I](
    id:I,
    types:Set[StatType]
  ):ZIO[Stats,StatsNotifierError,Option[Statsd]] = 
    ZIO.service[Stats].flatMap(stats => 
      ZIO.foreach(types)( typ => 
        for{
          stat <- stats.getStat(typ)
            .when(client_stat_notifiers.contains(typ))
            .flatMap(ZIO.fromOption(_))
            .orElseFail(NotClientNotifier(s"add $typ to global notifiers for ${this.toString()}"))
            .flatMap(ZIO.fromOption(_))
            .orElseFail(StatNotFound)
        }yield (StatType.to_int(typ),stat)
      )
  ).map(stats => 
    if (stats.size > 0) 
      Some(Statsd( id.toString(), stats.toMap[Int,Double])) 
    else None
  )

}
trait StatsNotifierError
case object StatNotFound extends StatsNotifierError
case class NotGlobalNotifier(msg:String) extends StatsNotifierError
case class NotClientNotifier(msg:String) extends StatsNotifierError

