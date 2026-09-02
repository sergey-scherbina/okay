package okay.persist

import okay.{!, +, Condition, Pure, pure}
import okay.codec.Schema

/**
 * Typed.Bad meets Condition (specs/condition.md's first consumer
 * outside the core): a decode road where damage does not merely
 * report — it ASKS. Each record that fails to decode SIGNALS a
 * `Damaged` condition carrying the offset, the error and the RAW
 * record, under a per-element "skip" restart:
 *
 *   - `Resume(a)` is the PATCH: the corrected value flows into the
 *     result exactly where the damaged record sat — the operator
 *     repairs at the point of failure instead of rerunning;
 *   - `Invoke("skip", _)` drops this element and the road
 *     continues — the damage-as-data fold, chosen dynamically;
 *   - `Fail` aborts naming the offset and the error — the old
 *     throw, for runs where repair has no mandate.
 *
 * Additive, as the rule demands: `Typed.read`'s total
 * `Decoded.Bad` answer is untouched — this road is what a caller
 * reaches for when tolerating is not enough and rerunning is too
 * much.
 */
object Repair {

  /** the condition a damaged record raises: everything a repair
   * policy could want — where, what went wrong, and the raw bytes
   * to repair FROM */
  final case class Damaged(offset: Long, error: String, raw: Record)

  /** decode records through the typed view, damage signalling as it
   * goes; answers (offset, value) pairs in record order */
  def decode[A](typed: Typed[A], records: Vector[Record])
               (using Schema[A], scala.reflect.ClassTag[A]): Vector[(Long, A)] ! Condition.Op =
    records.foldLeft(pure[Condition.Op, Vector[(Long, A)]](Vector.empty)) { (acc, r) =>
      acc.flatMap { done =>
        typed.decode(r) match
          case Typed.Decoded.Ok(off, _, _, a) => pure(done :+ (off, a))
          case Typed.Decoded.Bad(off, err) =>
            Condition.within[Vector[(Long, A)], Pure]("skip") {
              Condition.signal[A](Damaged(off, err, r)).map(a => done :+ (off, a))
            }(_ => done)
      }
    }

  /** the read-and-repair convenience: one partition slice through
   * the road above; dropped history stays the caller's concern
   * (TooEarly is an answer, not a condition — it needs no repair,
   * it needs a decision about WHERE to read) */
  def read[A](typed: Typed[A], partition: Int, from: Long, max: Int)
             (using Schema[A], scala.reflect.ClassTag[A]): Vector[(Long, A)] ! Condition.Op =
    typed.topic.read(partition, from, max) match
      case Topic.Read.TooEarly(_) => pure(Vector.empty)
      case Topic.Read.Records(rs) => decode(typed, rs)
}
