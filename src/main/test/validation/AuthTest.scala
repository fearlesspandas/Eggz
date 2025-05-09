package validation

import java.util.concurrent.TimeUnit

import controller.AuthCommandService
import controller.GET_GLOB_LOCATION
import controller.SET_GLOB_LOCATION
import controller.SUBSCRIBE
import controller.auth.get_glob_location
import controller.auth.set_glob_location
import zio._

object MainApp extends ZIOAppDefault {
  val z1 = (server_keys: Set[String]) =>
    (op: Any) => {
      val op1 = set_glob_location(server_keys)(op)
      val op2 = get_glob_location(op)
      ZIO
        .validateFirstPar(Seq(op1, op2)) { x =>
          x
        }
    }.fold(_ => false, x => x)
  val z2 = (server_keys: Set[String]) =>
    (op: Any) => {
      val op1 = set_glob_location(server_keys)(op)
      val op2 = get_glob_location(op)
      ZIO
        .validateFirst(Seq(op1, op2)) { x =>
          x
        }
    }.fold(_ => false, x => x)
  val z3 = AuthCommandService.all
  val z4 = AuthCommandService.all_non_par
  import scala.util.Random
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      _ <- Console.printLine("")
      test_data_base = Seq(
        SET_GLOB_LOCATION("1", Vector(0)),
        GET_GLOB_LOCATION("1"),
        SUBSCRIBE(GET_GLOB_LOCATION("1"))
      )
      test_data = (0 to 500000).map(_ => test_data_base(Random.nextInt(test_data_base.size)))

      start1 <- Clock.currentTime(TimeUnit.MICROSECONDS)
      _ <- ZIO.collectAll(test_data.map(dat => z1(Set("1"))(dat))).provide(ZLayer.succeed("1"))
      end1 <- Clock.currentTime(TimeUnit.MICROSECONDS)
      _ <- ZIO.collectAll(test_data.map(dat => z2(Set("1"))(dat))).provide(ZLayer.succeed("1"))
      end2 <- Clock.currentTime(TimeUnit.MICROSECONDS)
      _ <- ZIO.collectAll(test_data.map(dat => z3(Set("1"))(dat))).provide(ZLayer.succeed("1"))
      end3 <- Clock.currentTime(TimeUnit.MICROSECONDS)
      _ <- ZIO.collectAll(test_data.map(dat => z2(Set("1"))(dat))).provide(ZLayer.succeed("1"))
      end4 <- Clock.currentTime(TimeUnit.MICROSECONDS)
      _ <- ZIO.collectAllPar(test_data.map(dat => z2(Set("1"))(dat))).provide(ZLayer.succeed("1"))
      end5 <- Clock.currentTime(TimeUnit.MICROSECONDS)
      _ <- Console.printLine(
        s"tests 1 results (par validation): ${end1 - start1}, test 2 results (no par val): ${end2 - end1}, test3 results (par val ): ${end3 - end2}, test4 results (no par val) : ${end4 - end3} collectPar test ${end5 - end4}"
      )
    } yield ()
}
