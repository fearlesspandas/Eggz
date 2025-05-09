package physics

trait Vector3[A] {
  def +(x: Vector3[A]): Vector3[A]
  def -(x: Vector3[A]): Vector3[A]
  def dot(x: Vector3[A]): A
  def cross(x: Vector3[A]): Vector3[A]
  def distance(x: Vector3[A]): A
}
object Vector3 {}
