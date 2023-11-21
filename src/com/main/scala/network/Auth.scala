package network

import network.Auth.Authorized
import zio._
import zio.prelude.Validation

trait Auth[+OP, +E, -SENDER, +OUT] { self =>

  def validation[O >: OP]: O => ZIO[SENDER, E, OUT]

  def flatMap[R1 >: OP, E1 >: E, S <: SENDER, B](f: OUT => Auth[R1, E1, S, B]): Auth[R1, E1, S, B] =
    Authorized(self, f)

  def map[R1 >: OP, E1 >: E, S <: SENDER, B](f: OUT => B): Auth[R1, E1, S, B] =
    flatMap(out => Auth.succeed(f(out)))
}

object Auth {

  def fromFunction[A: Tag, B, S, E](
    f: ZIO[S, E, B],
    default: B
  ): Auth[A, E, S, B] =
    new Auth[A, E, S, B] { self =>
      override def validation[O >: A]: O => ZIO[S, E, B] =
        op => f
    }
  case class Root[A](eval: () => A) extends Auth[Nothing, Nothing, Any, A] {
    override def validation[O >: Nothing]: O => ZIO[Any, Nothing, A] = _ => ZIO.succeed(eval())

  }
  case class Authorized[OP, E, SENDER, A, B](
    first: Auth[OP, E, SENDER, A],
    success: A => Auth[OP, E, SENDER, B]
  ) extends Auth[OP, E, SENDER, B] {
    override def validation[O >: OP]: O => ZIO[SENDER, E, B] =
      op => first.validation(op).flatMap(success(_).validation(op))
  }

  def succeed[A](a: A): Auth[Nothing, Nothing, Any, A] = Root(() => a)
}
