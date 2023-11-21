package network

import network.Auth.Authorized
import zio._
import zio.prelude.Validation

trait Auth[+OP] { self =>

  def validation[R1 >: OP]: PartialFunction[R1, Validation[Boolean, Boolean]]

  def flatMap[R1 >: OP](f: Boolean => Auth[R1]): Auth[R1] =
    Authorized(self, f)

  def map[R1 >: OP](f: Boolean => Boolean): Auth[R1] =
    flatMap[R1](out => Auth.succeed(f(out)))
}

object Auth {
  case class Root(eval: () => Boolean) extends Auth[Nothing] {

    override def validation[R1 >: Nothing]: PartialFunction[R1, Validation[Boolean, Boolean]] = {
      case _ => Validation.succeed(eval())
    }
  }
  case class Authorized[OP](
    first: Auth[OP],
    success: Boolean => Auth[OP]
  ) extends Auth[OP] {

    override def validation[R1 >: OP]: PartialFunction[R1, Validation[Boolean, Boolean]] = {
      case op =>
        first.validation
          .orElse(
            success(false).validation
          )
          .orElse[R1, Validation[Boolean, Boolean]]({
            case _: R1 => Validation.succeed(false)
          })(op)
    }

  }

  def succeed(a: Boolean): Auth[Nothing] = Root(() => a)
}
