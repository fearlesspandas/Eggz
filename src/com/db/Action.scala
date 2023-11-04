package src.com.db



object Action{
  def fromString(raw:String,id:String):Option[Action]  = raw match {
    case "w" => Some(UpdateUserDirection(id,(FORWARD,FORWARD)))
    case "a" => Some(UpdateUserDirection(id,(LEFT,LEFT)))
    case "s" => Some(UpdateUserDirection(id,(RIGHT,RIGHT)))
    case "d" => Some(UpdateUserDirection(id,(BACKWARD,BACKWARD)))
    case _ => None
  }
}
sealed trait Action {

}

//input ->

sealed trait Direction{
  val normal:Vector3
}
case object FORWARD extends Direction{
  val normal = Vector3(0,0,1)
}
case object BACKWARD extends Direction{
  val normal = Vector3(0,0,-1)
}
case object LEFT extends Direction{
  val normal = Vector3(-1,0,0)
}
case object RIGHT extends Direction{
  val normal = Vector3(1,0,0)
}
case object UP extends Direction{
  val normal = Vector3(0,1,0)
}
case object DOWN extends Direction{
  val normal = Vector3(0,-1,0)
}
case class UpdateUserDirection(key: String, value: (Direction,Direction)) extends Action