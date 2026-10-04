package okay2.codec

import okay2.{Answers, Row}

/**
 * What a journal needs of an operation in order to record it
 * (okay-codec's Journalled, durable-any-operation): its name, a
 * fingerprint compared on replay, the retry that carries a far end's
 * dedup key, and — the one that decides the shape — how its answer is
 * written down and read back, since a journal stores strings and only
 * the operation knows its own answer type.
 *
 * Scala 2's signature is a ROW, `F <: Row`, whose operations are
 * `F#Op[A]`, as `Answers[F]` takes them.
 *
 * `perform` runs the operation AND says how its answer is written, both
 * at once: an operation type is COVARIANT, so a matched case cannot hand
 * its `A` to an encoder. Scala 3 rebuilds the operation inside its own
 * match, where it knows the answer type; Scala 2 does not refine `A` on
 * a constructor pattern at all, so an instance's typed road is a method
 * PER CASE — the operation performs and decodes itself
 * (`case class Lookup(k) extends Op[Int] { def run(h) = { val n = h.handle(this); (n, n.toString) } }`),
 * and the instance forwards to it. No cast either way (TestJournalled).
 */
trait Journalled[F <: Row] {
  /** the journal's `op` column, and the span's name */
  def name[A](op: F#Op[A]): String

  /** what the program ASKED FOR, compared on replay to catch drift */
  def fingerprint[A](op: F#Op[A]): String

  /** the retry that carries the first attempt's key, so the far end
   * recognises it as the same request. An operation with nowhere to put
   * a key returns itself. */
  def withKey[A](op: F#Op[A], key: String): F#Op[A]

  /** run the operation, and its answer beside the answer's written form */
  def perform[A](op: F#Op[A], inner: Answers[F]): (A, String)

  /** the answer, back out of the journal */
  def decode[A](op: F#Op[A], written: String): A

  /** what to show whoever must answer a parked question; defaults to the
   * fingerprint, always available and always cheap */
  def asked[A](op: F#Op[A]): Json = Json.JStr(fingerprint(op))
}
