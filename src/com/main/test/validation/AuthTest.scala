package validation

import controller.SET_GLOB_LOCATION
import controller.SerializableCommand
import controller.auth.get_glob_location
import controller.auth.set_glob_location
import entity.WorldBlock
import src.com.main.scala.entity.Globz
import zio._

object MainApp extends ZIOAppDefault {
  val z = (server_keys: Set[String]) =>
    (op: Any) => {
      val op1 = set_glob_location(server_keys)(op)
      val op2 = get_glob_location(op)
      ZIO
        .validateFirstPar(Seq(op1, op2)) { x =>
          x
        }
    }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      _ <- Console.printLine("")
      //res <- t.validation(SET_GLOB_LOCATION("1", Vector(0))).provide(ZLayer.succeed("1"))
      res2 <- z(Set("1"))(SET_GLOB_LOCATION("1", Vector(0))).provide(ZLayer.succeed("1"))
      //_ <- Console.printLine(res)
      _ <- Console.printLine(res2)
    } yield ()
}
