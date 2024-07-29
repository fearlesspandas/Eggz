package entity
import entity.TerrainChunk.ChunkId
import zio.*

import java.util.UUID
trait TerrainChunk {
  val id: ChunkId
  def get_boundary: IO[TerrainChunkError, Boundary]
  def add_chunk(chunk: TerrainChunk): IO[TerrainChunkError, Unit]
  def get_chunks(location: Vector[Double], radius: Double): Chunk[TerrainChunk]
}
object TerrainChunk {
  type ChunkId = UUID
}
case class GroupChunk(
  id: ChunkId,
  boundary: Boundary,
  chunksRef: Ref[Map[ChunkId, TerrainChunk]]
) extends TerrainChunk {

  override def get_boundary: IO[TerrainChunkError, Boundary] =
    ZIO.succeed(boundary)

  override def add_chunk(chunk: TerrainChunk): IO[TerrainChunkError, Unit] =
    for {
      loc <- chunk.get_boundary.flatMap(_.center.get)
      chunks <- chunksRef.get.map(_.values)
      chunks_around <- ZIO.filter(chunks)(c =>
        c.get_boundary.flatMap(_.is_within_boundary(loc))
      )
      _ <-
        if (chunks_around.size > 0) chunks.head.add_chunk(chunk)
        else chunksRef.update(_.updated(chunk.id, chunk))
    } yield ()

  override def get_chunks(
    location: Vector[Double],
    radius: Double
  ): Chunk[TerrainChunk] = ???
}
trait Boundary {
  val center: Ref[Vector[Double]]
  val up: Ref[Double]
  val down: Ref[Double]
  val left: Ref[Double]
  val right: Ref[Double]
  def is_within_boundary(loc: Vector[Double]): IO[TerrainChunkError, Boolean]
}
trait TerrainChunkError
