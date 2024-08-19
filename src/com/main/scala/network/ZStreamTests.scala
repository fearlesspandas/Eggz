package network

import zio.Duration
import zio.Queue
import zio.Ref
import zio.Schedule
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZIOAppDefault
import zio.http.WebSocketChannel
import zio.stream.ZChannel
import zio.stream.ZSink
import zio.stream.ZStream

object ZStreamTests extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      queue: Queue[String] <- Queue.unbounded[String]
      in_queue: Queue[String] <- Queue.unbounded[String]
      queue2: Queue[String] <- Queue.unbounded[String]

      _ <- ZStream
        .fromQueue(queue2)
        .filter(_.toInt % 2 == 1)
        .foreach(s => ZIO.log(s"test 1 $s"))
        .fork

      _ <- ZStream
        .fromQueue(queue2)
        .filter(_.toInt % 2 == 0)
        .foreach(s => ZIO.log(s"test 2 $s"))
        .fork

      n <- Ref.make(0)
      _ <- n.get
        .flatMap(ind =>
          queue2
            .offer(ind.toString)
            .flatMap(x => n.update(_ + 1))
            .flatMap(res => ZIO.log(s"sent ${ind}"))
        )
        .repeat(Schedule.spaced(Duration.fromMillis(1000)))
        .fork
      _ <- ZIO.sleep(Duration.fromMillis(30000))
//      _ <- f.join
    } yield ()
}
