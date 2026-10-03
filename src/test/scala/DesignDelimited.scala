package okay

import okay.Freer.{Return, Bind}
import scala.annotation.tailrec

/**
 * DESIGN SKETCH (specs/cont-atm.md, the operator's two entities): the stack of `Freer`'s continuations as data,
 * knowing no effect. Two joins, two types:
 *
 *  - `Segment`: frames joined by `Bind` — a value flows to the next frame, the answer pair chains. It IS
 *    `Freer`'s continuation made data, and a function `A => Freer[G, S, R, B]` like the lambda it replaces.
 *  - `Slice`: closed segments joined by BOUNDARIES — a level's ANSWER flows to the next segment as its VALUE.
 *    `Freer` has no such join; it is all a delimited stack adds. Not a function: crossing a boundary needs the
 *    machine. The whole stack is a slice; a captured continuation is a slice.
 */
object DesignDelimited:

  // ---- the segment: Bind's join ----

  /** frames from a value `A` to `Freer[G, S, R, B]`, joined as `Bind` joins: `f: A => F[X, T, R]` then
   * `X => F[B, S, T]` is `A => F[B, S, R]` */
  enum Segment[G[_, _, +_], -A, B, S, R] extends (A => Freer[G, S, R, B]):
    case Id[G[_, _, +_], A, S]() extends Segment[G, A, A, S, S]
    case Frame[G[_, _, +_], A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Segment[G, X, B, S, T])
      extends Segment[G, A, B, S, R]

    /** the segment as the function it stands for */
    def apply(a: A): Freer[G, S, R, B] = this match
      case Id() => Return(a)
      case Frame(f, rest) => Bind(f(a), rest)

  // ---- the slice: the boundary's join ----

  /** a boundary's mark, opaque to the stack, which an effect finds a boundary by: `T` the answer of its level */
  trait Tag[T]

  /**
   * a piece of the stack, from a value `A` (the hole) to the answer `T` at its bottom: CLOSED segments — each a
   * level, its last value its answer (`Segment[A, X, X, U]`) — joined by boundaries, where the inner level's
   * answer `U` is the next segment's value
   */
  enum Slice[G[_, _, +_], -A, T]:
    case Seg[G[_, _, +_], A, X, T](s: Segment[G, A, X, X, T]) extends Slice[G, A, T]
    case Under[G[_, _, +_], A, X, U, T](s: Segment[G, A, X, X, U], tag: Tag[U] | Null, rest: Slice[G, U, T])
      extends Slice[G, A, T]

  /** what lies under a level: the run's top, or a boundary and the slice under it */
  enum Below[G[_, _, +_], T, R]:
    case Top[G[_, _, +_], R]() extends Below[G, R, R]
    case At[G[_, _, +_], T, R](tag: Tag[T] | Null, rest: Slice[G, T, R]) extends Below[G, T, R]

  // ---- the primitives over the stack ----

  /** a level's segment and what lies under it, as one slice */
  def slice[G[_, _, +_], A, X, T, R](s: Segment[G, A, X, X, T], below: Below[G, T, R]): Slice[G, A, R] = below match
    case Below.Top() => Slice.Seg(s)
    case Below.At(tag, rest) => Slice.Under(s, tag, rest)

  /** what a capture found: the slice up to a marked boundary, and the slice under that boundary */
  sealed abstract class Found[G[_, _, +_], A, R]:
    type Y
    def taken: Slice[G, A, Y]
    def tag: Tag[Y]
    def under: Slice[G, Y, R]

  /** walk out from a level's segment to the nearest boundary whose mark `is` holds for: the slice above it, and
   * the slice under it; null when there is none */
  def capture[G[_, _, +_], A, X, T, R](s: Segment[G, A, X, X, T], below: Below[G, T, R],
                                       is: Tag[?] => Boolean): Found[G, A, R] | Null =
    walk(Rev.Nil[G, A](), s, below, is)

  /** put a slice back above a boundary marked `tag` over the slice `under` */
  def resume[G[_, _, +_], A, Y, R](taken: Slice[G, A, Y], tag: Tag[Y] | Null, under: Slice[G, Y, R]): Slice[G, A, R] =
    onto(Rev.Nil[G, A](), taken, tag, under)

  // ---- inside: the outward walk builds the slice reversed; a typed reversed list ----

  private enum Rev[G[_, _, +_], A0, A]:
    case Nil[G[_, _, +_], A0]() extends Rev[G, A0, A0]
    case Snoc[G[_, _, +_], A0, A, X, U](prev: Rev[G, A0, A], s: Segment[G, A, X, X, U], tag: Tag[U] | Null)
      extends Rev[G, A0, U]

  @tailrec private def walk[G[_, _, +_], A0, A, X, T, R](rev: Rev[G, A0, A], s: Segment[G, A, X, X, T],
                                                         below: Below[G, T, R], is: Tag[?] => Boolean): Found[G, A0, R] | Null =
    below match
      case Below.Top() => null
      case at: Below.At[G, T, R] =>
        val t = at.tag
        if t != null && is(t) then found(link(rev, Slice.Seg(s)), t.nn, at.rest)
        else at.rest match
          case Slice.Seg(s2) => walk(Rev.Snoc(rev, s, t), s2, Below.Top(), is)
          case Slice.Under(s2, t2, rest2) => walk(Rev.Snoc(rev, s, t), s2, Below.At(t2, rest2), is)

  private def found[G[_, _, +_], A, Y0, R](taken0: Slice[G, A, Y0], tag0: Tag[Y0], under0: Slice[G, Y0, R]): Found[G, A, R] =
    new Found[G, A, R]:
      type Y = Y0
      def taken = taken0
      def tag = tag0
      def under = under0

  /** the reversed pieces onto a slice, innermost last */
  @tailrec private def link[G[_, _, +_], A0, A, T](rev: Rev[G, A0, A], acc: Slice[G, A, T]): Slice[G, A0, T] = rev match
    case Rev.Nil() => acc
    case Rev.Snoc(prev, s, tag) => link(prev, Slice.Under(s, tag, acc))

  /** a slice's pieces set aside, its last segment closed over `under` by the boundary `tag`, then linked back */
  @tailrec private def onto[G[_, _, +_], A0, A, Y, R](rev: Rev[G, A0, A], s: Slice[G, A, Y], tag: Tag[Y] | Null,
                                                     under: Slice[G, Y, R]): Slice[G, A0, R] = s match
    case Slice.Seg(s1) => link(rev, Slice.Under(s1, tag, under))
    case Slice.Under(s1, t1, rest) => onto(Rev.Snoc(rev, s1, t1), rest, tag, under)
