package validation

import controller.Command
import controller.GET_GLOB_LOCATION
import controller.SET_GLOB_LOCATION
import controller.SerializableCommand
import controller.auth.GET_GLOB_LOCATION_AUTH
import controller.auth.SET_GLOB_LOCATION_AUTH
import controller.auth.get_glob_auth
import controller.auth.set_glob_location_auth
import entity.WorldBlock
import network.Auth
import src.com.main.scala.entity.Globz
import zio.ZIOAppDefault
import zio.prelude.Validation

object AuthTest extends ZIOAppDefault {
  import zio._
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      res <- auth
        .validation(SET_GLOB_LOCATION("", Vector(0)))
        .toZIO //t(SET_GLOB_LOCATION("1", Vector(0)))("21").toZIO
      res2 <- auth
        .validation(GET_GLOB_LOCATION("1"))
        .toZIO //.fold(c => c.foldLeft("")(_ + _), x => x)
      _ <- Console.printLine(res)
      _ <- Console.printLine(res2)
    } yield ()

  val auth: Auth[SerializableCommand[Globz.Service with WorldBlock.Block, Any], String] =
    for {
      setglob <- SET_GLOB_LOCATION_AUTH(Set("2"), "1")
      getglob <- GET_GLOB_LOCATION_AUTH("2")
    } yield setglob || getglob
  val t = (op: Command[Nothing, Any]) =>
    (senderId: String) =>
      Validation.validate(
        set_glob_location_auth(Set("2"), senderId)(op)
          .fold(_ => Validation.succeed(false), x => Validation.succeed(x)),
        get_glob_auth(op).fold(_ => Validation.succeed(false), x => Validation.succeed(x))
      )
}
