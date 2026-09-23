package okay

/**
 * Whether a value is an operation of the row F — for an operation that
 * arrives UNTYPED, as an `Object` from another language (okay-clojure's
 * `Program`, okay-frege's `Frege`; interop-shared).
 *
 * `TypeableK` tests one signature, and a runner needs no more: `split`
 * tests one side of a row and takes the other by exclusion. An `Object`
 * handed over from Clojure or Frege has no other side to exclude, so a row
 * is tested member by member. A single signature's `Member` is FOUND (its
 * `TypeableK`); a union is BUILT with `|` — dotty does not infer the two
 * sides of a union type lambda for a given of `F + G` (it answered
 * `Nothing` for both, measured in okay-frege), so a union is spelled once:
 * `Member.of[Reader % Long] | Member.of[State % Long]`.
 *
 * Resolved at the concrete row of a call site, never searched for at an
 * abstract one (AGENTS.md, the row crash).
 */
trait Member[F[+_]]:
  def test(x: Any): Boolean

  /** this row and another, side by side */
  def |[G[+_]](g: Member[G]): Member[F + G] = x => test(x) || g.test(x)

  /**
   * The value as an operation of F, once the test has said it IS one — the
   * one cast of the interop bridges, made here once: `F` is covariant, so
   * an `F[X]` is an `F[Any]` and the answer needs no cast (it goes back to
   * the other language as an `Object`). The claim `split` makes for every
   * runner in the core.
   */
  def operation(x: AnyRef): Option[F[Any]] =
    if test(x) then Some(x.asInstanceOf[F[Any]]) else None

object Member:
  given one[F[+_]](using t: TypeableK[F]): Member[F] = x => t.test(x)
  def of[F[+_]](using t: TypeableK[F]): Member[F] = one[F]
