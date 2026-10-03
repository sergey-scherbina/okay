package okay

import okay.Freer.{Return, Inject, Bind, Delay, Diag}
import scala.annotation.tailrec

/**
 * THE ABSTRACT MACHINE FOR `Freer`, effect-independent (specs/cont-atm.md; the operator: "Freer/Delimited должны
 * быть полностью независимы от того какой именно эффект в нем работает — их задача предоставить все
 * необходимые примитивы абстрактной машины для реализации эффектов").
 *
 * An interpreter of `Freer` written directly is correct and typed by `Freer`'s own indexes, but grows the host
 * stack in two places: the continuations of `Bind` (opaque lambdas) and the interpreter's own nested runs. The
 * machine is that host stack made DATA (the functional correspondence: Ager, Biernacki, Danvy & Midtgaard 2003;
 * Biernacka, Biernacki & Danvy 2005):
 *
 *  - `Frames`, a SEGMENT, the operator's `A => F[B, S, R]`: `Bind`'s continuations as data, joined as `Bind` joins.
 *    A MARK in it is transparent — a value passes it, its answer types chain through — and is what an effect finds
 *    again: a continuation mark (an environment) or a VALUE BOUNDARY (a prompt: a capture to it stops there).
 *  - `Stack`: segments joined by ANSWER BOUNDARIES — a level closes, and its answer flows to the segment outside
 *    as its value. Each has its own answer type, so answer-type modification is typed with no claim.
 *
 * `Delimited` is the interface — the machine's primitives, which an effect's `Step` answers its operations with.
 * Nothing here names an effect.
 */
trait Delimited[G[_, _, +_]]:
  import Delimited.{Mark, Tag, Next, Split, Closed}

  /** a closed segment from `x` to its answer, NOW: a nested run */
  def force[A, S, T](k: Frames[G, A, S, S, T], x: A): T

  /** the next state: a program, its segment, the stack under it */
  def next[A, B, S, T, R, Z](c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Next[G, Z]

  /** the empty segment */
  def end[A, S]: Frames[G, A, A, S, S]

  /** a frame on a segment: `f`'s value flows into `rest` */
  def frame[A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T]): Frames[G, A, B, S, R]

  /** a mark on a segment: transparent — the segment's types are its own */
  def mark[A, B, S, R](m: Mark, rest: Frames[G, A, B, S, R]): Frames[G, A, B, S, R]

  /** an answer boundary over the stack `rest`: the level inside closes and answers `R`; `out` takes that on */
  def bound[S, R, B2, S2, X, Z](tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X], rest: Stack[G, B2, S2, X, Z]): Stack[G, S, S, R, Z]

  /** the nearest mark `is` holds for, from a segment out through every boundary; null when none */
  def find[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null

  /** a segment cut at its nearest mark `is` holds for: the frames above it, the mark, the frames below; null when
   * the segment has none */
  def split[A, B, S, T](k: Frames[G, A, B, S, T], is: Mark => Boolean): Split[G, A, B, S, T] | Null

  /** the frames `above` put on top of the segment `below`: one segment again */
  def join[A, Y, S1, T, B, S](above: Frames[G, A, Y, S1, T], below: Frames[G, Y, B, S, S1]): Frames[G, A, B, S, T]

  /** the nearest boundary as an ANSWER boundary, the segment up to it closed by it: null when the level ends
   * by value (the run's own end) */
  def closed[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Closed[G, A, T, R, Z] | Null

/**
 * A STEP: what the machine does with one of effect `G`'s operations in a program over the row `H` — given the
 * segment up to the nearest boundary and the stack under it, the next state, built with the machine's
 * primitives. `G[T, R, A]` is the operation at the program's indexes; the equations its own GADT match supplies
 * type the state it answers.
 */
trait Step[G[_, _, +_], H[_, _, +_]]:
  def step[A, B, S, T, R, Z](op: G[T, R, A], k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z],
                             machine: Delimited[H]): Delimited.Next[H, Z]

/** a SEGMENT, `A => Freer[G, S, R, B]` as data: frames joined as `Bind` joins them, and transparent marks.
 * Contravariant in `A`: it consumes a value. */
enum Frames[G[_, _, +_], -A, B, S, R]:
  case End[G[_, _, +_], A, S]() extends Frames[G, A, A, S, S]
  case Frame[G[_, _, +_], A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T])
    extends Frames[G, A, B, S, R]
  /** a mark: a value passes it unchanged, a capture carries it, no answer type moves */
  case Marked[G[_, _, +_], A, B, S, R](mark: Delimited.Mark, rest: Frames[G, A, B, S, R]) extends Frames[G, A, B, S, R]

/** THE STACK: what closes a level computing `Freer[G, S, R, B]` into the run's result `Z` */
enum Stack[G[_, _, +_], B, S, R, Z]:
  /** the run's end by VALUE: the level's value is the result, its answers diagonal */
  case Done[G[_, _, +_], B, X]() extends Stack[G, B, X, X, B]
  /** the run's end by ANSWER: the level closes (its value is its answer index) and its answer is the result */
  case Answered[G[_, _, +_], S, R]() extends Stack[G, S, S, R, R]
  /** an ANSWER boundary: the level closes and answers `R`; outside it, `out` takes that on as its value */
  case Bound[G[_, _, +_], S, R, B2, S2, X, Z](tag: Delimited.Tag[R] | Null, out: Frames[G, R, B2, S2, X],
                                               rest: Stack[G, B2, S2, X, Z]) extends Stack[G, S, S, R, Z]

object Delimited:

  /** a machine for one effect, alone */
  def apply[G[_, _, +_]](steps: Step[G, G]): Machine[G] = Machine(steps)

  /** a machine for effect `G` under the effects `F`: it answers `G`'s operations and leaves `F`'s in the program
   * it answers, for the machine outside */
  def under[G[_, _, +_], F[+_]](steps: Step[G, Row[G, F]]): Under[G, F] = Under(steps)

  /**
   * an operation of a program over effect `G` under the effects `F`: one of `G`'s, at any indexes (`Own`), or one
   * of `F`'s, which passes through and so is DIAGONAL — it changes no answer type of `G`'s levels (`Fwd`)
   */
  enum Sum[G[_, _, +_], F[+_], S, R, +A]:
    case Own[G[_, _, +_], F[+_], S, R, A](op: G[S, R, A]) extends Sum[G, F, S, R, A]
    case Fwd[G[_, _, +_], F[+_], S, A](op: F[A]) extends Sum[G, F, S, S, A]

  /** the row: `G`'s operations and `F`'s */
  type Row[G[_, _, +_], F[+_]] = [S, R, A] =>> Sum[G, F, S, R, A]

  // ---- what the primitives speak of ----

  /** what an effect hangs on a segment to find again; opaque to the machine */
  trait Mark

  /** a mark that marks a boundary's place — a prompt — with `T` the value or answer at that place */
  trait Tag[T] extends Mark

  /** the machine's state, which a step answers: a program, its segment, the stack under it */
  sealed abstract class Next[G[_, _, +_], Z]:
    type A
    type B
    type S
    type T
    type R
    def c: Freer[G, T, R, A]
    def k: Frames[G, A, B, S, T]
    def m: Stack[G, B, S, R, Z]

  /** a segment cut at a mark: `above` from the hole to the mark's place `Y`, then `below` from there on */
  sealed abstract class Split[G[_, _, +_], A, B, S, T]:
    type Y
    type S1
    def above: Frames[G, A, Y, S1, T]
    def mark: Mark
    def below: Frames[G, Y, B, S, S1]

  /** a CAPTURED continuation: a segment closed by its answer boundary, from `A` to the answer `T` */
  sealed abstract class Kont[G[_, _, +_], -A, T]:
    type S
    def k: Frames[G, A, S, S, T]

  /** the nearest boundary seen as an answer boundary: the segment closed by it, and its two ways on */
  sealed abstract class Closed[G[_, _, +_], A, T, R, Z] extends Kont[G, A, T]:
    /** the level answers `r`: it goes to the boundary, and on outside it */
    def answer(r: R): Next[G, Z]
    /** `c` runs in the level's place, closed by the same boundary */
    def instead(c: Freer[G, R, R, R]): Next[G, Z]

  // ---- the implementation ----

  /** the primitives over `Frames` and `Stack`, shared by every machine, and its room for nested runs */
  abstract class Core[G[_, _, +_]] extends Delimited[G]:
    private var room: Int = StackSwitch.firstRoom

    /** `body` one level deeper, on a fresh stack when there is no room left here (`StackSwitch`) */
    protected final def deeper[X](body: => X): X =
      val here = room - 1
      if here > 0 then within(here, body)
      else StackSwitch.fresh(fresh => within(fresh, body))

    private def within[X](left: Int, body: => X): X =
      val saved = room
      room = left
      try body finally room = saved

    def next[A0, B0, S0, T0, R0, Z](c0: Freer[G, T0, R0, A0], k0: Frames[G, A0, B0, S0, T0], m0: Stack[G, B0, S0, R0, Z]): Next[G, Z] =
      new Next[G, Z]:
        type A = A0
        type B = B0
        type S = S0
        type T = T0
        type R = R0
        def c = c0
        def k = k0
        def m = m0

    def end[A, S]: Frames[G, A, A, S, S] = Frames.End()
    def frame[A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T]): Frames[G, A, B, S, R] =
      Frames.Frame(f, rest)
    def mark[A, B, S, R](m: Mark, rest: Frames[G, A, B, S, R]): Frames[G, A, B, S, R] = Frames.Marked(m, rest)
    def bound[S, R, B2, S2, X, Z](tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X], rest: Stack[G, B2, S2, X, Z]): Stack[G, S, S, R, Z] =
      Stack.Bound(tag, out, rest)

    @tailrec final def find[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null =
      k match
        case Frames.Marked(mk, rest) => if is(mk) then mk else find(rest, m, is)
        case Frames.Frame(_, rest) => find(rest, m, is)
        case Frames.End() => m match
          case Stack.Bound(_, out, rest) => find(out, rest, is)
          case _ => null

    def split[A, B, S, T](k: Frames[G, A, B, S, T], is: Mark => Boolean): Split[G, A, B, S, T] | Null =
      splitFrom(Rev.Nil[G, A, T](), k, is)

    @tailrec private def splitFrom[A0, T0, A, B, S, T](rev: Rev[G, A0, A, T, T0], k: Frames[G, A, B, S, T],
                                                       is: Mark => Boolean): Split[G, A0, B, S, T0] | Null = k match
      case Frames.Marked(mk, rest) =>
        if is(mk) then cut(Rev.link(rev, end[A, T]), mk, rest)
        else splitFrom(Rev.Marks(rev, mk), rest, is)
      case Frames.Frame(f, rest) => splitFrom(Rev.Snoc(rev, f), rest, is)
      case Frames.End() => null

    private def cut[A, Y0, S10, T, B, S](above0: Frames[G, A, Y0, S10, T], mk: Mark, below0: Frames[G, Y0, B, S, S10]): Split[G, A, B, S, T] =
      new Split[G, A, B, S, T]:
        type Y = Y0
        type S1 = S10
        def above = above0
        def mark = mk
        def below = below0

    def join[A, Y, S1, T, B, S](above: Frames[G, A, Y, S1, T], below: Frames[G, Y, B, S, S1]): Frames[G, A, B, S, T] =
      Rev.link(Rev.reverse(Rev.Nil[G, A, T](), above), below)

    def closed[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Closed[G, A, T, R, Z] | Null = m match
      case Stack.Bound(tag, out, rest) => closedBy(k, tag, out, rest)
      case Stack.Answered() => atTop[A, S, T, R](k)
      case Stack.Done() => null

    private def closedBy[A, S0, T, R, B2, S2, X, Z](k0: Frames[G, A, S0, S0, T], tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X],
                                                    rest: Stack[G, B2, S2, X, Z]): Closed[G, A, T, R, Z] =
      new Closed[G, A, T, R, Z]:
        type S = S0
        def k = k0
        def answer(r: R): Next[G, Z] = next(Return[G, X, R](r), out, rest)
        def instead(c: Freer[G, R, R, R]): Next[G, Z] = next(c, end[R, R], Stack.Bound[G, R, R, B2, S2, X, Z](tag, out, rest))

    private def atTop[A, S0, T, R](k0: Frames[G, A, S0, S0, T]): Closed[G, A, T, R, R] =
      new Closed[G, A, T, R, R]:
        type S = S0
        def k = k0
        def answer(r: R): Next[G, R] = next(Return[G, R, R](r), end[R, R], Stack.Answered[G, R, R]())
        def instead(c: Freer[G, R, R, R]): Next[G, R] = next(c, end[R, R], Stack.Answered[G, R, R]())

  /** the machine for one effect, alone: every operation is its own */
  final class Machine[G[_, _, +_]](steps: Step[G, G]) extends Core[G]:

    /** run `c` with `k` as the last frame of its continuation; the run's result is its answer */
    def run[A, S, R](c: Freer[G, S, R, A], k: A => S): R =
      go(c, frame((a: A) => Return[G, S, S](k(a)), end[S, S]), Stack.Answered[G, S, R]())

    /** run `c` to its value: answers diagonal, as a `Free` program's are */
    def value[A, X](c: Freer[G, X, X, A]): A = go(c, end[A, X], Stack.Done[G, A, X]())

    def force[A, S, T](k: Frames[G, A, S, S, T], x: A): T = deeper(go(Return[G, T, A](x), k, Stack.Answered[G, S, T]()))

    @tailrec private def go[A, B, S, T, R, Z](c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Z =
      c match
        case Return(a) => k match
          case Frames.Frame(f, k2) => go(f(a), k2, m)
          case Frames.Marked(_, k2) => go(Return(a), k2, m)
          case Frames.End() => m match
            case Stack.Done() => a
            case Stack.Answered() => a
            case Stack.Bound(_, out, rest) => go(Return(a), out, rest)
        case Bind(c0, f) => go(c0, Frames.Frame(f, k), m)
        case Delay(t) => go(t(), k, m)
        case Inject(op) =>
          val n = steps.step(op, k, m, this)
          go(n.c, n.k, n.m)
        case Diag(op) => go(Inject(op), k, m)


  /**
   * the machine for effect `G` under the effects `F`: `G`'s operations go to its steps; an `F` operation leaves,
   * as a node of the program the machine answers, with this machine's state behind it in a `Delay` — so the
   * machine outside, forcing it in its own loop, re-enters this one with no host frame per nesting
   */
  final class Under[G[_, _, +_], F[+_]](steps: Step[G, Row[G, F]]) extends Core[Row[G, F]]:
    private type H[S, R, A] = Sum[G, F, S, R, A]

    /** run `c` with `k` as the last frame of its continuation: the program, over `F`, that answers its answer */
    def run[A, S, R](c: Freer[H, S, R, A], k: A => S): Free[F, R] =
      go(c, frame((a: A) => Return[H, S, S](k(a)), end[S, S]), Stack.Answered[H, S, R]())

    /** run `c` to its value: the program, over `F`, that answers it */
    def value[A, X](c: Freer[H, X, X, A]): Free[F, A] = go(c, end[A, X], Stack.Done[H, A, X]())

    /** a strict `k` cannot wait for an outer effect: one met inside it is refused, by name */
    def force[A, S, T](k: Frames[H, A, S, S, T], x: A): T = deeper(go(Return[H, T, A](x), k, Stack.Answered[H, S, T]())) match
      case Return(t) => t
      case _ => throw IllegalStateException("a strict k performed an operation of an outer effect; give its body the lazy k")

    @tailrec private def go[A, B, S, T, R, Z](c: Freer[H, T, R, A], k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z]): Free[F, Z] =
      c match
        case Return(a) => k match
          case Frames.Frame(f, k2) => go(f(a), k2, m)
          case Frames.Marked(_, k2) => go(Return(a), k2, m)
          case Frames.End() => m match
            case Stack.Done() => Return(a)
            case Stack.Answered() => Return(a)
            case Stack.Bound(_, out, rest) => go(Return(a), out, rest)
        case Bind(c0, f) => go(c0, Frames.Frame(f, k), m)
        case Delay(t) => go(t(), k, m)
        case Inject(op) => op match
          case Sum.Own(o) =>
            val n = steps.step(o, k, m, this)
            go(n.c, n.k, n.m)
          case Sum.Fwd(o) => forward(o, k, m)
        case Diag(op) => go(Inject(op), k, m)

    /** an `F` operation out, as a node of the answered program; its answer enters this machine again, in a `Delay` */
    private def forward[X, B, S, T, Z](o: F[X], k: Frames[H, X, B, S, T], m: Stack[H, B, S, T, Z]): Free[F, Z] =
      Bind(Inject[Freer.Lift[F], Unit, Unit, X](o), (x: X) => Delay(() => again(Return[H, T, X](x), k, m)))

    /** the loop entered again from the outside: a call, so `go` stays a loop */
    private def again[A, B, S, T, R, Z](c: Freer[H, T, R, A], k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z]): Free[F, Z] =
      go(c, k, m)

  /** a segment's frames set aside, top first, to build a segment back to front: typed, for `split` and `join` */
  private enum Rev[G[_, _, +_], A0, +A, T, T0]:
    case Nil[G[_, _, +_], A0, T0]() extends Rev[G, A0, A0, T0, T0]
    case Snoc[G[_, _, +_], A0, A, X, T, R, T0](prev: Rev[G, A0, A, R, T0], f: A => Freer[G, T, R, X]) extends Rev[G, A0, X, T, T0]
    case Marks[G[_, _, +_], A0, A, T, T0](prev: Rev[G, A0, A, T, T0], mark: Mark) extends Rev[G, A0, A, T, T0]

  private object Rev:
    /** the set-aside frames back on top of `k` */
    @tailrec def link[G[_, _, +_], A0, A, T, T0, B, S](rev: Rev[G, A0, A, T, T0], k: Frames[G, A, B, S, T]): Frames[G, A0, B, S, T0] =
      rev match
        case Nil() => k
        case Snoc(prev, f) => link(prev, Frames.Frame(f, k))
        case Marks(prev, mk) => link(prev, Frames.Marked(mk, k))

    /** a segment's frames set aside, top first */
    @tailrec def reverse[G[_, _, +_], A0, A, Y, S1, T, T0](rev: Rev[G, A0, A, T, T0], k: Frames[G, A, Y, S1, T]): Rev[G, A0, Y, S1, T0] =
      k match
        case Frames.End() => rev
        case Frames.Frame(f, rest) => reverse(Snoc(rev, f), rest)
        case Frames.Marked(mk, rest) => reverse(Marks(rev, mk), rest)
