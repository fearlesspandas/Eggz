package validation

import zio._

object MainApp extends ZIOAppDefault {
  val z
    : ZIO[Any, String, String] = Console.printLine("").fold(_ => "", _ => "") //.mapError(_ => "")
  val f1 = ZIO.succeed(1).debug
  val f2 = ZIO.succeed(2).debug *> ZIO.fail("Oh uh!")
  val f3 = ZIO.succeed(3).debug
  val f4 = ZIO.succeed(4).debug *> z
  val f5 = ZIO.succeed(5).debug

  val myApp: ZIO[Any, String, (Int, Int, Int)] =
    f1 validate f2 validate f3 validate f4 validate f5
  val otherapp = f1 validate f2 validate f3 validate f4 validate f5
  def run = myApp.cause.debug.uncause
}
