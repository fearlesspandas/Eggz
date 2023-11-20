package network

import controller.Command
import controller.GET_GLOB_LOCATION
import controller.SET_GLOB_LOCATION
import controller.SerializableCommand
import entity.WorldBlock
import network.Auth.Authorized
import src.com.main.scala.entity.Globz
import zio.IO
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault

trait Auth[+OP, -SENDER, +OUT] { self =>
  def validate[S <: SENDER, R1 >: OP](id: S, op: R1): ZIO[Any, Nothing, OUT]

  def flatMap[R1 >: OP, S <: SENDER, B](f: OUT => Auth[R1, S, B]): Auth[R1, S, B] =
    Authorized(self, f)

  def map[R1 >: OP, S <: SENDER, B](f: OUT => B): Auth[R1, S, B] =
    flatMap(out => Auth.succeed(f(out)))
}
object Auth {
  case class Root[A](eval: () => A) extends Auth[Nothing, Any, A] {
    override def validate[S <: Any, R1 >: Nothing](id: S, op: R1): ZIO[Any, Nothing, A] =
      ZIO.succeed(eval())
  }
  case class Authorized[OP, SENDER, A, B](
    first: Auth[OP, SENDER, A],
    success: A => Auth[OP, SENDER, B]
  ) extends Auth[OP, SENDER, B] {
    override def validate[S <: SENDER, R1 >: OP](id: S, op: R1): ZIO[Any, Nothing, B] =
      for {
        firstP <- first.validate(id, op)
        b <- success(firstP).validate(id, op)
      } yield b
  }

  def succeed[A](a: A): Auth[Nothing, Any, A] = Root(() => a)
}

case class GET_GLOB_LOCATION_AUTH(validId: String)
    extends Auth[GET_GLOB_LOCATION, String, Boolean] {

  override def validate[S <: String, R1 >: GET_GLOB_LOCATION](
    id: S,
    op: R1
  ): ZIO[Any, Nothing, Boolean] = ZIO.succeed(true)
}
case class SET_GLOB_LOCATION_AUTH(validId: String)
    extends Auth[SET_GLOB_LOCATION, String, Boolean] {
  override def validate[S <: String, R1 >: SET_GLOB_LOCATION](
    id: S,
    op: R1
  ): ZIO[Any, Nothing, Boolean] =
    for {
      _ <- ZIO.unit
    } yield true
}

object AuthTest extends ZIOAppDefault {
  import zio._
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      res <- auth.validate("1", SET_GLOB_LOCATION("1", Vector(0)))
      _ <- Console.printLine(res)
    } yield ()

  type Env = SerializableCommand[Globz.Service with WorldBlock.Block, Any]
  type AuthEnv = Auth[Env, String, Boolean]

  var auth =
    for {
      setglob <- SET_GLOB_LOCATION_AUTH("1")
      getglob <- GET_GLOB_LOCATION_AUTH("1")
    } yield setglob && getglob

}
