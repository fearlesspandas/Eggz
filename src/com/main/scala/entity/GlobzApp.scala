package src.com.main.scala.entity

//import src.com.main.scala.entity.Globz.Globz
import controller.Command.CommandError
import controller._
import entity.BasicPlayer
import entity.WorldBlock
import entity.WorldBlockEnvironment
import zio.Console
import zio.Duration
import zio.ExitCode
import zio.Runtime
import zio.Schedule
import zio.ZIO
import zio.ZIOAppDefault
import zio.ZLayer

object GlobzApp extends ZIOAppDefault {

  type ApplicationEnvironment = Globz.Service with WorldBlock.Service
  val addSimpleLogger: ZLayer[Any, Nothing, Unit] =
    Runtime.addLogger((_, _, _, message: () => Any, _, _, _, _) => println(message()))
  val localApplicationEnvironment =
    addSimpleLogger ++ WorldBlockEnvironment.worldblock ++ ZLayer.succeed(BasicPlayer)

  def run =
    program()
      .provideLayer(localApplicationEnvironment)
      .provideLayer(WorldBlockEnvironment.anyref)

  def program(): ZIO[Globz.Service with WorldBlock.Service, Nothing, ExitCode] =
    (for {
      controller <- BasicController.make
      egg1 <- RepairEgg.make("1", 100, 10)
      egg2 <- RepairEgg.make("2", 100, 5)
      _ <- controller
        .runCommand(CREATE_GLOB("1", Vector(0)).run)
        .flatMap(_.runCommand(CREATE_GLOB("2", Vector(1)).run))
      //      _ <- controller.runCommand(CREATE_GLOB("2", Vector(1)).run)
      g <- controller.runQuery(GET_BLOB("1").run)
      _ <- controller.runCommand(CREATE_EGG(egg1, g.get).run)
      _ <- controller.runCommand(CREATE_EGG(egg2, g.get).run)

      _ <- controller.runCommand(RELATE_EGGS("1", "2")("1").run)
//      _ <- controller.runCommand(TICK_WORLD().run)
      _ <- controller.runCommand(START_EGG("1", "1").run)
      _ <- controller.runCommand(START_EGG("2", "1").run)
      res2 <- controller
        .runQuery[Set[Globz.Glob], CommandError](GET_ALL_GLOBS().run)
        .flatMap(s => { ZIO.collectAllPar(s.map(_.getAll())) })
        .map(_.flatMap(x => x).toSeq)
        .flatMap(s =>
          ZIO.collectAllPar(
            s.map(gl =>
              for {
                h <- gl.health()
                e <- gl.energy()
              } yield s"health: $h  energy: $e"
            )
          )
        )
        .flatMap(s => Console.printLine(s"${s}"))
        .repeat(Schedule.spaced(Duration.fromMillis(1000)))
      //_ <- Console.printLine(s"RESULTS: $res2")
    } yield ExitCode.success).mapError(_ => null.asInstanceOf[Nothing])

}
