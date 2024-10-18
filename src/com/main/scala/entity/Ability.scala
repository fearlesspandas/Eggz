package entity

import controller.Command
import controller.DoAbility
import controller.MultiResponse
import controller.QueryResponse
import controller.QueuedClientBroadcast
import controller.QueuedClientMessage
import controller.QueuedServerMessage
import controller.SerializableCommand
import controller.SerializableCommand.CommandError
import entity.Ability.ABILITY_ID
import entity.DATA_TYPE.GLOBULAR_TELEPORT
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.*
import zio.json.*
import implicits._

trait Ability extends Command[WorldBlock.Block, QueryResponse] {
  val id: ABILITY_ID
}
object Ability {
  type ABILITY_ID = Int
  private val ability_config = Chunk(Smack)
  def make(
    id: ABILITY_ID,
    from: GLOBZ_ID,
    args: AbilityArgs = NoArgs
  ): IO[AbilityError, Ability] =
    (id, args) match {
      case (0, _) => ZIO.succeed(Smack(from))
      case (1, gta: GlobularTeleportArgs) =>
        ZIO.succeed(GlobularTeleport(from, gta))
      case _ => ZIO.fail(AbilityDoesNotExistError)
    }
}
trait AbilityError extends CommandError
case class SmackAbilityError(msg: String) extends AbilityError
case object AbilityRequirementsNotMetError extends AbilityError
case object AbilityDoesNotExistError extends AbilityError
case object WrongArgsForAbilityError extends AbilityError
case object NoEntityFoundError extends AbilityError
case object NoLocationFoundForEntityError extends AbilityError

case class Smack(from: GLOBZ_ID) extends Ability {
  override val id: ABILITY_ID = 0
  override def run
    : ZIO[WorldBlock.Block, SerializableCommand.CommandError, QueryResponse] =
    for {
      res <- ZIO
        .succeed(
          MultiResponse(
            Chunk(
              QueuedServerMessage(Chunk(DoAbility(from, 0))),
              QueuedClientBroadcast(Chunk(DoAbility(from, 0)))
            )
          )
        )
    } yield res
}
case class GlobularTeleport(
  from: GLOBZ_ID,
  args: GlobularTeleportArgs
) extends Ability {
  override val id: ABILITY_ID = 0
  override def run
    : ZIO[WorldBlock.Block, SerializableCommand.CommandError, QueryResponse] =
    for {
      glob <- get_entity(from).orElseFail(NoEntityFoundError)
      res <- args match {
        case Point(x, y, z) =>
          add_point(from, Vector(x, y, z))
            .orElseFail(
              GlobularTeleportAddPointError
            )
            .as(
              MultiResponse(
                Chunk(
//                  QueuedClientMessage(from, Chunk(DoAbility(from, 1, args)))
                )
              )
            )
        case Base(x, y, z) =>
          add_base(from, Vector(x, y, z))
            .orElseFail(
              GlobularTeleportAddBaseError
            )
            .as(
              MultiResponse(
                Chunk(
//                  QueuedClientMessage(from, Chunk(DoAbility(from, 1, args)))
                )
              )
            )
        case Do =>
          for {
            data <- get_data(from).orElseFail(GlobularTeleportExecuteError)
            points <- data.points.get
            mapped_points <- ZIO.foreachPar(points.take(3))(v =>
              ZIO
                .succeed(v(0))
                .zip(ZIO.succeed(v(1)))
                .zip(ZIO.succeed(v(2)))
            )
            base <- data.base.get
              .flatMap(ZIO.fromOption(_))
              .flatMap(v =>
                ZIO
                  .succeed(v(0))
                  .zip(ZIO.succeed(v(1)))
                  .zip(ZIO.succeed(v(2)))
              )
              .orElseFail(GlobularTeleportExecuteError1)
            shape <- ZIO
              .succeed(Shape(mapped_points.toSet, base))
            _ <- clear(from).orElseFail(GlobularTeleportExecuteError2)
          } yield MultiResponse(
            Chunk(
              QueuedServerMessage(Chunk(DoAbility(from, 1, shape))),
              QueuedClientBroadcast(Chunk(DoAbility(from, 1, shape)))
            )
          )
        case _ =>
          ZIO.log("Unrecognized glob teleport arg") *> ZIO.succeed(
            MultiResponse(Chunk())
          )
      }
    } yield res

  def get_entity(
    id: GLOBZ_ID
  ): ZIO[WorldBlock.Block, AbilityDataError, LivingEntity] =
    for {
      glob <- ZIO
        .serviceWithZIO[WorldBlock.Block](_.getBlob(id))
        .flatMap(ZIO.fromOption(_))
        .mapBoth(
          _ => AbilityDataNoGlobFoundError,
          { case li: LivingEntity => li }
        )
    } yield glob

  def get_data(
    id: GLOBZ_ID
  ): ZIO[WorldBlock.Block, AbilityDataError, GlobularTeleportData] =
    for {
      glob <- get_entity(id)
      data <- glob.ability_data_ref.get
        .map(
          _.get(GLOBULAR_TELEPORT)
        )
        .flatMap(ZIO.fromOption(_))
        .foldZIO(
          _ =>
            for {
              r_base <- Ref.make[Option[Vector[Double]]](None)
              r_points <- Ref.make(Chunk.empty[Vector[Double]])
              res <- ZIO.succeed(GlobularTeleportData(r_base, r_points))
              _ <- glob.ability_data_ref
                .update(_.updated(GLOBULAR_TELEPORT, res))
            } yield res,
          ZIO.succeed(_)
        )
        .map { case gt: GlobularTeleportData => gt }
    } yield data

  def add_point(
    id: GLOBZ_ID,
    point: Vector[Double]
  ): ZIO[WorldBlock.Block, AbilityDataError, Unit] =
    for {
      glob <- get_entity(id)
      loc <- glob.getLocation.orElseFail(AbilityDataNoLocationFoundError)
      dist <- ZIO.succeed((loc - point).length())
      data <- get_data(id)
      _ <- data.points.update(Chunk(point) ++ _).when(dist < 5)
    } yield ()

  def add_base(
    id: GLOBZ_ID,
    point: Vector[Double]
  ): ZIO[WorldBlock.Block, AbilityDataError, Unit] =
    for {
      glob <- get_entity(id)
      loc <- glob.getLocation.orElseFail(AbilityDataNoLocationFoundError)
      dist <- ZIO.succeed((loc - point).length())
      data <- get_data(id)
      _ <- data.base.update(_ => Some(point)).when(dist < 5)
    } yield ()

  def clear(id: GLOBZ_ID): ZIO[WorldBlock.Block, AbilityDataError, Unit] =
    for {
      data <- get_data(id)
      _ <- data.base.update(_ => None)
      _ <- data.points.update(_ => Chunk())
    } yield ()
}
case object GlobularTeleportAddPointError extends AbilityError
case object GlobularTeleportAddBaseError extends AbilityError
case object GlobularTeleportExecuteError extends AbilityError

case object GlobularTeleportExecuteError1 extends AbilityError
case object GlobularTeleportExecuteError2 extends AbilityError
case object GlobularTeleportExecuteError3 extends AbilityError

sealed trait AbilityArgs
object AbilityArgs {
  implicit val encoder: JsonEncoder[AbilityArgs] =
    DeriveJsonEncoder.gen[AbilityArgs]
  implicit val decoder: JsonDecoder[AbilityArgs] =
    DeriveJsonDecoder.gen[AbilityArgs]
}

case object NoArgs extends AbilityArgs

case class Shape(
  points: Set[(Double, Double, Double)],
  location: (Double, Double, Double)
) extends AbilityArgs

//object Shape {
//  implicit val encoder: JsonEncoder[Shape] =
//    DeriveJsonEncoder.gen[Shape]
//  implicit val decoder: JsonDecoder[Shape] =
//    DeriveJsonDecoder.gen[Shape]
//}

//GlobularTeleport args
sealed trait GlobularTeleportArgs extends AbilityArgs
case class Point(x: Double, y: Double, z: Double) extends GlobularTeleportArgs
case class Base(x: Double, y: Double, z: Double) extends GlobularTeleportArgs
case object Do extends GlobularTeleportArgs
