package validation

import controller.SET_GLOB_LOCATION
import controller.SerializableCommand
import controller.auth.get_glob_location
import controller.auth.set_glob_location
import entity.WorldBlock
import src.com.main.scala.entity.Globz
import zio._

object MainApp extends ZIOAppDefault {
//  val z
//    : ZIO[Any, String, String] = Console.printLine("").fold(_ => "", _ => "") //.mapError(_ => "")
//  val f1 = ZIO.succeed(1).debug
//  val f2 = ZIO.succeed(2).debug *> ZIO.fail("Oh uh!")
//  val f3 = ZIO.succeed(3).debug
//  val f4 = ZIO.succeed(4).debug *> z
//  val f5 = ZIO.succeed(5).debug
//
//  val myApp: ZIO[Any, String, (Int, Int, Int)] =
//    f1 validate f2 validate f3 validate f4 validate f5
//  val otherapp = f1 validate f2 validate f3 validate f4 validate f5
//
  // def run = myApp.cause.debug.uncause
//
//  val t = for {
//    g <- get_glob_auth
//    s <- set_glob_auth(Set("1"))
//
//  } yield s || g

  val z = (server_keys: Set[String]) =>
    (op: Any) => {
      val op1 = set_glob_location(server_keys)(op)
      val op2 = get_glob_location(op)
      ZIO
        .validateFirst(Seq(op1, op2)) { x =>
          x
        }
      //.map(_.filter(x => x).size > 0)
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
