package controller

import controller.Command.CMD
import controller.SerializableCommand.CommandError
import zio.Schedule
import zio.ZIO

import java.time.Duration

trait Command[-Env, +Out] extends Product with Serializable { self =>
  def run: ZIO[Env, CommandError, Out]
  def flatMap[R1 <: Env, B](f: Out => Command[R1, B]): Command[R1, B] =
    CMD(self, f)
  def map[R1 <: Env, B](f: Out => B): Command[R1, B] = flatMap(a => Command.succeed(f(a)))
}

object Command {
  case class CMD[Env, A, B](first: Command[Env, A], success: A => Command[Env, B])
      extends Command[Env, B] {
    override def run: ZIO[Env, CommandError, B] =
      for {
        firstP <- first.run.map(success(_))
        b <- firstP.run
      } yield b
  }
  case class Root[A](eval: () => A) extends Command[Any, A] {
    override def run: ZIO[Any, CommandError, A] = ZIO.succeed(eval())
  }
  case class RootZIO[R, E, A](f: ZIO[R, E, A]) extends Command[R, A] {
    override def run: ZIO[R, CommandError, A] = f.mapError(_ => ???)
  }
  def succeed[A](a: A): Command[Any, A] = Root(() => a)

  def fromZIO[R, E, A](f: ZIO[R, E, A]): Command[R, A] = RootZIO(f)
}

case class RUN_COMMAND_ASYNC[-Env](
  cmd: Command[_ >: Env, Any],
  duration: Long
) extends Command[Env, Unit] {
  override def run: ZIO[Env, CommandError, Unit] =
    cmd.run.repeat(Schedule.spaced(Duration.ofMillis(duration))).fork.map(_ => ())
}
