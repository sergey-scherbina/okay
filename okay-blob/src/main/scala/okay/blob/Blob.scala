package okay.blob

import okay.{!, +, Async, Chunk, Produce}

/**
 * The object-store seam (specs/blob.md): bytes and streams in the
 * engine, meaning at the edge. Keys are opaque strings with `/` as
 * the conventional prefix separator — the S3 model, which is the
 * model. Streams both directions at constant memory: a 10GB segment
 * never lives in the heap. Multipart is an engine detail under the
 * same `put`; conditional put joins when a consumer needs optimistic
 * commit over blobs.
 */
final case class Etag(value: String)

final case class Meta(key: String, size: Long, etag: Etag, modified: Long)

trait Blob:
  /** stream bytes in; the etag identifies what landed */
  def put(key: String, bytes: Chunk[Byte] ! (Produce + Async)): Etag ! Async

  /** stream bytes out — the chunks are the body, the ANSWER is the
   * outcome: an absent key is a Left naming it, never a throw */
  def get(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Unit] ! (Produce + Async)

  /** size, etag, modified — no body */
  def head(key: String): Option[Meta] ! Async

  /** every key under the prefix, once, in key order; paged
   * underneath where the engine pages */
  def list(prefix: String): Chunk[Meta] ! (Produce + Async)

  /** idempotent — deleting the absent is a no-op */
  def delete(key: String): Unit ! Async
