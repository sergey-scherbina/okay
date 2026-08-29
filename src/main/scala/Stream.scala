package okay

/**
 * A stream is codata: defined not by its constructors but by the one
 * observation uncons — the next element and the rest of the stream,
 * or None at the end. The observer holds the pace: nothing past the
 * asked element is computed, which is what makes early stop, zip and
 * interleave expressible (a push consumer like Fold cannot zip).
 * LazyList is the final coalgebra of X => Option[(A, X)] — the
 * canonical carrier every stream unfolds into (see toLazyList); a
 * Producer observes by stepping its next operation. Writer is the
 * cousin with a richer observation, Either[A, (W, rest)] — the same
 * codata but with the answer carried at the end (see Writer.uncons).
 *
 * Re-observation contract: uncons is pure and repeatable, but on a
 * program carrier a repeated uncons repeats the step's work — only
 * the LazyList bridge memoizes.
 */
trait Stream[S[_]]:
  /** the next element and the rest, or None at the end */
  def uncons[A](s: S[A]): Option[(A, S[A])]

/** the final coalgebra observes itself */
given Stream[LazyList] with
  def uncons[A](s: LazyList[A]): Option[(A, LazyList[A])] =
    if s.isEmpty then None else Some((s.head, s.tail))

extension [S[_], A](s: S[A])(using St: Stream[S])
  /** the next element and the rest, or None at the end */
  def uncons: Option[(A, S[A])] = St.uncons(s)

  /**
   * The anamorphism into the final coalgebra: unfold the stream into
   * a LazyList by repeated uncons, on demand and memoized — the
   * canonical bridge from any stream representation, and the free way
   * to every LazyList combinator (map, filter, zip, take, ...).
   */
  def toLazyList: LazyList[A] = LazyList.unfold(s)(St.uncons(_))

/**
 * The same observations directly on a writer program (as overloads
 * beside the generic ones, so both resolve): inference cannot reach
 * Writer's Stream instance through its type lambda — it would have to
 * abstract the element slot of a constant lambda, a higher-order
 * unification the compiler does not attempt — so the shape gets them
 * first-order.
 */
extension [W, A](a: A ! Writer % W)
  /** the next told value and the rest, or None (the answer forgotten) */
  def uncons: Option[(W, A ! Writer % W)] = Writer.uncons(a).toOption

  /** unfold the told values into the final coalgebra, on demand */
  def toLazyList: LazyList[W] = LazyList.unfold(a)(Writer.uncons(_).toOption)
