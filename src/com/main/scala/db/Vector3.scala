package src.com.db

import zio.json._
case class Vector3 (x:Float,y:Float,z:Float){
  def +(v:Vector3):Vector3 = Vector3(x + v.x,y + v.y,z + v.z)
  def *(scalar:Double):Vector3 = Vector3(x * scalar.toFloat , y * scalar.toFloat,z * scalar.toFloat)
  def /(scalar:Double):Vector3 = Vector3(x / scalar.toFloat , y / scalar.toFloat,z / scalar.toFloat)

  def length : Double = scala.math.sqrt(x * x + y * y + z * z)
  def normalize:Vector3 = this/length
}
object Vector3{
  implicit val decoder:JsonDecoder[Vector3] = DeriveJsonDecoder.gen[Vector3]
  implicit val encoder:JsonEncoder[Vector3]= DeriveJsonEncoder.gen[Vector3]
}
