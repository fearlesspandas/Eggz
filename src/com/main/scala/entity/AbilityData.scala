package entity

import entity.DATA_TYPE.GLOBULAR_TELEPORT
import src.com.main.scala.entity.Globz.GLOBZ_ID
import zio.*
import implicits.*
enum DATA_TYPE:
  case GLOBULAR_TELEPORT
end DATA_TYPE

trait AbilityData {
  // GlobularTeleport
  val ability_data_ref: Ref[Map[DATA_TYPE, DATA]]
}
trait AbilityDataError
case object AbilityDataNoGlobFoundError extends AbilityDataError
case object AbilityDataNoLocationFoundError extends AbilityDataError
case object GlobularTeleportInconsistentDataState extends AbilityDataError
trait DATA
case class GlobularTeleportData(
  base: Ref[Option[Vector[Double]]],
  points: Ref[Chunk[Vector[Double]]]
) extends DATA
