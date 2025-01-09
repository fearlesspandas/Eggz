package entity

enum TerrainTypes {
  case MONK_GARDEN
  case BLOCK_TERRAIN
  case HEALTH_STAR
}
object TerrainTypes {
  def toString(t: TerrainTypes): String = t match {
    case MONK_GARDEN   => "24"
    case BLOCK_TERRAIN => "6"
    case HEALTH_STAR   => "11"
  }
}
implicit class TerrainTypesToString(t: TerrainTypes) {
  def toId(): String = TerrainTypes.toString(t)
}
