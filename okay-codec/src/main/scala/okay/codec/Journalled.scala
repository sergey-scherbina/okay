package okay.codec

import okay.Handler

/**
 * What a journal needs of an operation in order to record it
 * (durable-any-operation, specs/llm-agentic.md "Any operation, not
 * only a tool").
 *
 * `Durable` journalled `Tool` and nothing else, and two other specs
 * noticed by promising what it could not do — specs/r.md and
 * specs/py.md both said a foreign-runtime step is "journalable by
 * Durable", and both had to be corrected. This is the seam that makes
 * them true.
 *
 * FOUR OF THESE ARE WHAT THE CODE ALREADY USED. Reading the handler
 * below, a `ToolCall` reached the journal in exactly four ways: its
 * name, a fingerprint compared on replay, a key a far end
 * deduplicates on, and the retry that carries that key. Nothing else
 * about a tool ever got in.
 *
 * THE FIFTH IS THE ONE THAT DECIDES THE SHAPE. `Entry.answer` is an
 * `Option[String]` and every file and table behind `Journal` stores
 * strings, so an operation answering an `A` must say how that `A` is
 * written down and read back. The codec belongs HERE rather than in
 * the framework because the operation knows its own answer type and
 * the framework does not — and because it is the honest place to pay:
 * a journal that can replay a million-row frame has to have written a
 * million rows down, and an instance that does not want that journals
 * a handle instead. Either way the choice is stated where the
 * operation is declared, which is where every other Durable decision
 * already lives.
 *
 * The `Tool` instance is the identity on all of it, which is why
 * `Durable.tools` behaves exactly as it did.
 */
trait Journalled[Op[_]]:
  /** the journal's `op` column, and the span's name */
  def name[A](op: Op[A]): String

  /** what the program ASKED FOR, compared on replay to catch drift.
   * Whatever identifies the request — an R instance fingerprints the
   * script and a hash of its inputs, not the frame it will return. */
  def fingerprint[A](op: Op[A]): String

  /** the retry that carries the first attempt's key, so the far end
   * recognises it as the same request (`OnRepeat.WithKey`). An
   * operation with nowhere to put a key returns itself — and should
   * not be declared `WithKey`. */
  def withKey[A](op: Op[A], key: String): Op[A]

  /**
   * Run the operation and say how its answer is written down — both
   * at once, and that is not a convenience.
   *
   * `Tool[+A]` is COVARIANT, so matching `Tool.Call` refines `A` only
   * to `A >: String`: enough to hand a `String` back as an `A`
   * (decode), not enough to turn an `A` into a `String` (encode). An
   * `encode[A](op: Op[A], answer: A): String` therefore cannot be
   * written for it without a runtime type test, and this repository
   * does not take a cast while a typed road exists.
   *
   * The typed road is here. The answer type is exact wherever the
   * operation is CONSTRUCTED — `inner.handle(Tool.Call(c))` is a
   * `Tool[String]` and answers a `String` — so the instance performs
   * the call inside its own match, where it still knows that, and
   * returns the typed answer beside its written form. `(s, s)` for a
   * tool, and neither half is a cast.
   *
   * (Making `Tool` invariant was tried first and refused by the
   * compiler for a better reason than variance pedantry: the effect
   * row needs it, and `effect(Tool.Call(c)): String ! Agent` stops
   * type-checking without it.)
   */
  def perform[A](op: Op[A], inner: Handler[Op]): (A, String)

  /** the answer, back out of the journal. Given the operation, so a
   * GADT can refine `A` — and String <: A is the easy direction. */
  def decode[A](op: Op[A], written: String): A

  /** what to show whoever must answer a parked question. Defaults to
   * the fingerprint, which is always available and always cheap. */
  def asked[A](op: Op[A]): Json = Json.JStr(fingerprint(op))

