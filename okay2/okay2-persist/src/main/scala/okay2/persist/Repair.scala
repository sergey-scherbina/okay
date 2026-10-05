package okay2.persist

import scala.reflect.ClassTag

import okay2.{!, Condition, Free, Pure, pure}

/**
 * Typed.Bad meets Condition (okay2-condition-repair; the Scala 3 core's okay-persist `Repair`, specs/condition.md's
 * first consumer outside the core): a decode road where damage does not merely report — it ASKS. Each record that
 * fails to decode SIGNALS a `Damaged` condition carrying the offset, the error and the RAW record, under a
 * per-element "skip" restart:
 *
 *   - `Resume(a)` is the PATCH: the corrected value flows into the result exactly where the damaged record sat;
 *   - `Invoke("skip", _)` drops this element and the road continues;
 *   - `Fail` aborts naming the offset and the error.
 *
 * Additive: `Typed.read`'s total `Decoded.Bad` answer is untouched — this road is what a caller reaches for when
 * tolerating is not enough and rerunning is too much.
 */
object Repair {

  /** the condition a damaged record raises: where, what went wrong, and the raw bytes to repair FROM */
  final case class Damaged(offset: Long, error: String, raw: Record)

  /** decode records through the typed view, damage signalling as it goes; (offset, value) pairs in record order.
   * The walk recurses inside `flatMap`, so the interpreter trampolines it (okay2 has no `foldM`) */
  def decode[A](typed: Typed[A], records: Vector[Record])(implicit tag: ClassTag[A]): Vector[(Long, A)] ! Condition = {
    def go(i: Int, done: Vector[(Long, A)]): Vector[(Long, A)] ! Condition =
      if (i >= records.length) pure[Condition, Vector[(Long, A)]](done)
      else {
        val r = records(i)
        val step: Vector[(Long, A)] ! Condition = typed.decode(r) match {
          case Typed.Decoded.Ok(off, _, _, a) => pure[Condition, Vector[(Long, A)]](done :+ ((off, a)))
          case Typed.Decoded.Bad(off, err) =>
            Condition.within[Vector[(Long, A)], Pure]("skip") {
              Condition.signal[A](Damaged(off, err, r)).map(a => done :+ ((off, a)))
            }(_ => done)
        }
        step.flatMap(next => go(i + 1, next))
      }
    Free.delay(() => go(0, Vector.empty))
  }

  /** one partition slice through the road above; dropped history stays the caller's concern (TooEarly is an
   * answer, not a condition) */
  def read[A](typed: Typed[A], partition: Int, from: Long, max: Int)(implicit tag: ClassTag[A]): Vector[(Long, A)] ! Condition =
    typed.topic.read(partition, from, max) match {
      case Topic.Read.TooEarly(_) => pure[Condition, Vector[(Long, A)]](Vector.empty)
      case Topic.Read.Records(rs) => decode(typed, rs)
    }
}
