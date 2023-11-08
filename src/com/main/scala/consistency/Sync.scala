package consistency

import zio.ExitCode
import zio.IO
import zio.ZIO

trait Sync[E] {
  def commit: IO[E, ExitCode]
  def sync: IO[E, ExitCode]
}

object Sync {

  def commit[E]: ZIO[Sync[E], E, ExitCode] = ZIO.service[Sync[E]].flatMap(_.commit)
  def sync[E]: ZIO[Sync[E], E, ExitCode] = ZIO.service[Sync[E]].flatMap(_.sync)
}
