package network

import network.Auth.Authorized
import zio._
import zio.prelude.Validation

trait Auth[+OP, -SENDER] { self =>

  def validation[R1 >: OP]: PartialFunction[R1, Validation[Boolean, Boolean]]

  def flatMap[R1 >: OP, S <: SENDER: Tag](f: Boolean => Auth[R1, S]): Auth[R1, S] =
    Authorized(self, f)

  def map[R1 >: OP, S <: SENDER: Tag, B](f: Boolean => Boolean): Auth[R1, S] =
    flatMap[R1, S](out => Auth.succeed(f(out)))
}

trait AuthError
object Auth {
  type Authorizer[E, A] = Validation[E, A]
  case class Root(eval: () => Boolean) extends Auth[Nothing, Any] {

    override def validation[R1 >: Nothing]: PartialFunction[R1, Validation[Boolean, Boolean]] = {
      case _ => Validation.succeed(eval())
    }
  }
  case class Authorized[OP, SENDER: Tag, A, B](
    first: Auth[OP, SENDER],
    success: Boolean => Auth[OP, SENDER]
  ) extends Auth[OP, SENDER] {

    override def validation[R1 >: OP]: PartialFunction[R1, Validation[Boolean, Boolean]] = {

      case op if first.validation.isDefinedAt(op) =>
        first
          .validation(op)
          .fold(_ => Validation.succeed(false), x => Validation.succeed(x))
          .map(res => (res, success(res)))
          .flatMap {
            case (res, next) if next.validation.isDefinedAt(op) =>
              next
                .validation(op)
                .fold(_ => Validation.succeed(res), x => Validation.succeed(res || x))
            case (res, next) => Validation.succeed(res)
          }
      case op =>
        Validation
          .succeed(false)
          .map(res => (res, success(res)))
          .flatMap {
            case (res, next) if next.validation.isDefinedAt(op) =>
              next
                .validation(op)
                .fold(_ => Validation.succeed(res), x => Validation.succeed(x))
            case (res, next) => Validation.succeed(res)
          }
    }

  }

  def succeed(a: Boolean): Auth[Nothing, Any] = Root(() => a)
}
