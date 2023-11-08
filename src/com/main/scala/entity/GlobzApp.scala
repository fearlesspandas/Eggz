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
    program2()
    //      .repeat(Schedule.recurs(1))
      .provideLayer(localApplicationEnvironment)
      .provideLayer(WorldBlockEnvironment.anyref)
//      .provideLayer(WorldBlockEnvironment.anyref)
//  program2()
//    .provideLayer(GlobzEnvironment.anyRef)
//    .provideLayer(WorldBlockEnvironment.anyref)
//    .provideLayer(localApplicationEnvironment) *> ZIO
//    .succeed(
//      ExitCode.success
//    )

  def program2(): ZIO[Globz.Service with WorldBlock.Service, Nothing, ExitCode] =
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

  def program(): ZIO[ApplicationEnvironment, Nothing, ExitCode] = (
    (for {
      _ <- Console.printLine("Welcome")
      controller <- BasicController.make
      g <- Globz.create("1")
      ref <- Ref.make[Int](0)
      _ <- (Console.readLine.orDie
        .repeatWhileZIO {
          case "q" => ZIO.succeed(false)
          case st =>
            (for {
              _ <- Console.printLine("Enter health amount:")
              id <- ref.get
              egg <- RepairEgg.make(id.toString, st.toInt, 10)
              e <- g.update(egg) //.mapError(null)
              _ <- ref.update(_ + 1)
              all <- g.getAll()
              _ <- g
                .tickAll()
                .zipPar(Console.printLine(s"all eggs current: ${all}").fold(e => (), x => x))
              t <- ZIO
                .collectAllPar(
                  all.collect { case x: Storage.Service[String] => x.getInventory() }
                )
              //.flatMap(_)
              _ <- Console.printLine(s"Inventories: ${t.flatMap(x => x)}")
              //.mapError(_ => null.asInstanceOf[Nothing])
              //            _ <- putStrLn(s"all eggs current: ${all}").fold(e => (),x=>x)
            } yield true).fold(_ => true, x => x)
        })
        .map(x => x)
      //      g <- Globz.update(Eggz("1",v.toInt))
      //      all <- Globz.getAll()
      //      _ <- putStrLn(s"all eggs current: ${all}").fold(e => (),x=>x)
      //      _ <- program()
    } yield ExitCode.success).fold(
      e => {
        println(e.toString); null.asInstanceOf[Nothing]
      },
      x => x
    ) //.tapError( e => putStrLn(s"Unexpected failure $e")).fold(_ => null.asInstanceOf[Nothing],x => x)
    //      .flatMapError(e =>
    //      (putStrLn("try again") *> program *> ZIO.succeed(null.asInstanceOf[Nothing]))
    //        .mapError(err => null.asInstanceOf[Nothing])
    //    )
  )

}
