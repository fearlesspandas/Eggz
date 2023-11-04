package src.com.main.scala.entity

import src.com.main.scala.entity.Globz.Globz
import zio.console.Console
import zio.console.getStrLn
import zio.console.putStrLn
import zio.ExitCode
import zio.Ref
import zio.URIO
import zio.ZIO

object GlobzApp extends zio.App {

  type ApplicationEnvironment = Console with Globz

  val localApplicationEnvironment = Console.live ++ GlobzEnvironment.inMemory

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program().provideLayer(localApplicationEnvironment) *> ZIO.succeed(ExitCode.success)

  def program(): ZIO[ApplicationEnvironment, Nothing, ExitCode] = (
    (for {
      _ <- putStrLn("Welcome")
      ref <- Ref.make[Int](0)
      _ <- (getStrLn.orDie
        .repeatWhileM {
          case "q" => ZIO.succeed(false)
          case st =>
            (for {
              _ <- putStrLn("Enter health amount:")
              id <- ref.get
              e <- Globz.update(RepairEgg(id.toString, st.toInt, 10)) //.mapError(null)
              _ <- ref.update(_ + 1)
              all <- Globz.getAll()
              _ <- Globz
                .tickAll()
                .zipPar(putStrLn(s"all eggs current: ${all}").fold(e => (), x => x))
              t <- ZIO
                .collectAllPar(
                  all.collect { case x: Storage.Service[String] => x.getAll() }
                )
              //.flatMap(_)
              _ <- putStrLn(s"Inventories: ${t.flatMap(x => x)}")
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
