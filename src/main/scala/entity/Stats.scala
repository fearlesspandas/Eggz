package entity

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
}

