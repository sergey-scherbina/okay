package okay.blob

import okay.Async
import okay.freer.{!}
/** Atomic create-only capability. No check-then-write emulation is valid.
 * Larger values must be represented by bounded immutable chunks. */
trait ConditionalObjects:
  def create(key: String, bytes: Array[Byte]): ConditionalObjects.Created ! Async
  def read(key: String, maxBytes: Int): Option[Array[Byte]] ! Async

object ConditionalObjects:
  val MaxBytes: Int = 8 * 1024 * 1024
  enum Created:
    case Added, Exists
