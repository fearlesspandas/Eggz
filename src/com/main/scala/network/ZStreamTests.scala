package network

import zio.Duration
import zio.Queue
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
//      _ <- queue.offer("hello")
      stream = ZStream.fromQueue(queue) // .run(ZSink.log("found")).fork
      stream2 = ZStream.fromQueue(in_queue)
      _ <- stream2
        .foreach(s =>
          for {
            // _ <- queue.takeUpTo(1)
            _ <- queue.offer(s)
          } yield ()
        )
        .fork
      a = ZStream.service[WebSocketChannel]
//      _ <- stream.take(1).runCollect.fork
      _ <- stream.take(1).foreach(x => ZIO.log(x)).fork
      _ <- stream.foreach(x => ZIO.log(s"dropped ${x}")).fork
//      _ <- stream.take(2).foreach(s => queue.offer("queue " + s)).fork
      _ <- in_queue.offer("hello")
      _ <- in_queue.offer("world")
      _ <- in_queue.offer("thing")
      _ <- in_queue.offer("other")
//      x = stream.concat(ZStream.succeed("stuff"))
//      f <- stream.find(_ == "other").foreach(s => ZIO.log(s"Take $s")).fork
      _ <- ZIO.sleep(Duration.fromMillis(3000))
//      _ <- f.join
    } yield ()
}
