package src.com.main.scala.entity

//import src.com.main.scala.entity.Globz.Globz
import zio.Console
import zio.ExitCode
import zio.Ref
import zio.Scope
import zio.URIO
import zio.ZIO
import zio.ZIOApp
import zio.ZIOAppArgs
import zio.Runtime
import zio.ZIOAppDefault
import zio.ZLayer

object GlobzApp extends ZIOAppDefault {

  type ApplicationEnvironment = Globz.Service
  val addSimpleLogger: ZLayer[Any, Nothing, Unit] =
    Runtime.addLogger((_, _, _, message: () => Any, _, _, _, _) => println(message()))
  val localApplicationEnvironment = addSimpleLogger ++ GlobzEnvironment.inMemory // ++ GlobzEnvironment.anyRef

  def run =
    program().provideLayer(localApplicationEnvironment).provideLayer(GlobzEnvironment.anyRef) *> ZIO
      .succeed(
        ExitCode.success
      )

  def program(): ZIO[ApplicationEnvironment, Nothing, ExitCode] = (
    (for {
      _ <- Console.printLine("Welcome")
      g <- Globz.create("1")
      ref <- Ref.make[Int](0)
      _ <- (Console.readLine.orDie
        .repeatWhileZIO {
          case "q" => ZIO.succeed(false)
          case st =>
            (for {
              _ <- Console.printLine("Enter health amount:")
              id <- ref.get
              e <- g.update(RepairEgg(id.toString, st.toInt, 10)) //.mapError(null)
              _ <- ref.update(_ + 1)
              all <- g.getAll()
              _ <- g
                .tickAll()
                .zipPar(Console.printLine(s"all eggs current: ${all}").fold(e => (), x => x))
              t <- ZIO
                .collectAllPar(
                  all.collect { case x: Storage.Service[String] => x.getAll() }
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
