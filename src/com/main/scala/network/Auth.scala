package network

import controller.Command
import controller.GET_GLOB_LOCATION
import controller.SET_GLOB_LOCATION
import network.Auth.Authorized
import zio.IO
import zio.ZIO

trait Auth[-OP, +SENDER, +OUT] { self =>
  def validate[S >: SENDER](id: S, op: OP): ZIO[Any, Nothing, OUT]

  def flatMap[R1 <: OP, S >: SENDER, B](f: OUT => Auth[R1, S, B]): Auth[R1, S, B] =
    Authorized(self, f)

  def map[R1 <: OP, B](f: OUT => B): Auth[R1, SENDER, B] = flatMap(out => Auth.succeed(f(out)))
}
object Auth {
  case class Root[A](eval: () => A) extends Auth[Any, Nothing, A] {
    override def validate[S >: Nothing](id: S, op: Any): ZIO[Any, Nothing, A] = ZIO.succeed(eval())
  }
  case class Authorized[OP, SENDER, A, B](
    first: Auth[OP, SENDER, A],
    success: A => Auth[OP, SENDER, B]
  ) extends Auth[OP, SENDER, B] {
    override def validate[S >: SENDER](id: S, op: OP): ZIO[Any, Nothing, B] =
      for {
        firstP <- first.validate(id, op)
        b <- success(firstP).validate(id, op)
      } yield b
  }

  def succeed[A](a: A): Auth[Any, Nothing, A] = Root(() => a)
}

case class AuthorizeCommand() extends Auth[Command[Any, Any], String, Boolean] {
  override def validate[S >: String](id: S, op: Command[Any, Any]): ZIO[Any, Nothing, Boolean] = ???
}
