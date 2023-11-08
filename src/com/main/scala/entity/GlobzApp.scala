package src.com.main.scala.entity

//import src.com.main.scala.entity.Globz.Globz
import controller.{BasicController, CREATE_EGG, CREATE_GLOB, GET_ALL_GLOBS, GET_BLOB, START_EGG, TICK_WORLD}
import controller.Command.CommandError
import entity.WorldBlock
import entity.WorldBlockEnvironment
import zio.{Console, Duration, ExitCode, Random, Ref, Runtime, Schedule, Scope, URIO, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, ZLayer}

object GlobzApp extends ZIOAppDefault {

  type ApplicationEnvironment = Globz.Service with WorldBlock.Service
  val addSimpleLogger: ZLayer[Any, Nothing, Unit] =
    Runtime.addLogger((_, _, _, message: () => Any, _, _, _, _) => println(message()))
  val localApplicationEnvironment =
    addSimpleLogger ++ GlobzEnvironment.inMemory ++ WorldBlockEnvironment.worldblock // ++ GlobzEnvironment.anyRef

  def run =
    program()
      .provideLayer(localApplicationEnvironment)
      .provideLayer(WorldBlockEnvironment.anyref)

  def program(): ZIO[Globz.Service with WorldBlock.Service, Nothing, ExitCode] =
    (for {
      controller <- BasicController.make
      egg <- RepairEgg.make("1", 100, 10)
      _ <- controller
        .runCommand(CREATE_GLOB("1", Vector(0)).run)
        .flatMap(_.runCommand(CREATE_GLOB("2", Vector(1)).run))
//      _ <- controller.runCommand(CREATE_GLOB("2", Vector(1)).run)
      g <- controller.runQuery(GET_BLOB("1").run)
      _ <- controller.runCommand(CREATE_EGG(egg, g.get).run)
      _ <- controller.runCommand(TICK_WORLD().run)
      _ <- controller.runCommand(START_EGG("1","1").run)
      res2 <- controller
        .runQuery[Set[Globz.Glob], CommandError](GET_ALL_GLOBS().run)
        .flatMap(s => Console.printLine(s"RESULTS;$s")).repeat(Schedule.spaced(Duration.fromMillis(1000)))
      //_ <- Console.printLine(s"RESULTS: $res2")
    } yield ExitCode.success).mapError(_ => null.asInstanceOf[Nothing])

}
