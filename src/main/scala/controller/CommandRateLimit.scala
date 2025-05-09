package controller

import entity.WorldBlock

import java.util.concurrent.TimeUnit
import zio.*
trait CommandRateLimit[A] {
  def shouldExecute[E, O](
    command: Command[E, O]
  ): ZIO[A, CommandRateLimitError, Boolean]
  def updateExecuted(key: Any): UIO[Unit]
}

trait CommandRateLimitError

case class BasicCommandRateLimit(
  num_executes: Ref[Map[Any, Int]],
  last: Ref[Map[Any, Long]]
) extends CommandRateLimit[WorldBlock.Block]:
  override def shouldExecute[E, O](
    command: Command[E, O]
  ): ZIO[WorldBlock.Block, CommandRateLimitError, Boolean] = command match {
    case s: SerializableCommand[E, O] =>
      s.REF_TYPE match {
        case (ABILITY, id, 0) =>
          for {
            next_num <- num_executes.get.map(_.getOrElse(s.REF_TYPE, 0))
            res <- next_num match {
              case n if n > 2 =>
                for {
                  last_execute <- last.get.map(_.getOrElse(s.REF_TYPE, 0L))
                  current_time <- Clock.currentTime(TimeUnit.MILLISECONDS)
                } yield if (current_time - last_execute > 3000) true else false
              case _ => ZIO.succeed(true)
            }
          } yield res
        case (ADD_ABILITY, id) =>
          for {
            next_num <- num_executes.get.map(_.getOrElse(s.REF_TYPE, 0))
            res <- next_num match {
              case n if n > 2 =>
                for {
                  last_execute <- last.get.map(_.getOrElse(s.REF_TYPE, 0L))
                  current_time <- Clock.currentTime(TimeUnit.MILLISECONDS)
                } yield if (current_time - last_execute > 500) true else false
              case _ => ZIO.succeed(true)
            }
          } yield res

        case _ => ZIO.succeed(true)
      }
    case _ => ZIO.succeed(true)
  }

  override def updateExecuted(key: Any): UIO[Unit] = for {
    current_time <- Clock.currentTime(TimeUnit.MILLISECONDS)
    _ <- last.update(_.updated(key, current_time))
    maxnum <- key match {
      case (ABILITY, _, _) => ZIO.succeed(2)
      case _               => ZIO.succeed(1)
    }
    currnum <- num_executes.get.map(_.getOrElse(key, 0))
    should_reset_count <- key match {
      case (ABILITY, _, _) => ZIO.succeed(currnum > maxnum)
      case _               => ZIO.succeed(false)
    }
    _ <-
      if (!should_reset_count) num_executes.update(_.updated(key, currnum + 1))
      else num_executes.update(_.updated(key, 0))
  } yield ()

object BasicCommandRateLimit {
  def make: UIO[BasicCommandRateLimit] = for {
    num_executes <- Ref.make(Map.empty[Any, Int])
    last <- Ref.make(Map.empty[Any, Long])
  } yield BasicCommandRateLimit(num_executes, last)
}
