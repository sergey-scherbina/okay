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
 * Biernacka, Biernacki & Danvy 2005), two entities for the two joins:
 *
 *  - `Frames`, a SEGMENT: `Bind`'s continuations, joined as `Bind` joins — a value flows to the next frame;
 *  - `Stack`: segments joined by BOUNDARIES — a level's ANSWER flows to the segment outside it as its value.
 *
 * Every boundary has its own answer type, so answer-type modification is typed with no claim. `Delimited` is the
 * interface — the machine's primitives, which an effect's `Step` answers its operations with. A machine runs ONE
 * effect; effects nest as machines (`Delimited.under`): an inner machine answers its own operations and leaves
 * the rest's in the program it answers, which the machine outside runs. Nothing here names an effect.
 */
trait Delimited[G[_, _, +_]]:
  import Delimited.{Tag, Mark, Next, Found, Cap}

  /** a segment from `x` to its answer, NOW: a nested run */
  def force[A, S](k: Frames[G, A, S], x: A): S

  /** the next state: a program, its segment, the stack under it */
  def next[A, S, T, R](c: Freer[G, S, T, A], k: Frames[G, A, S], m: Stack[G, T, R]): Next[G, R]

  /** the empty segment: a value is its level's answer */
  def end[A]: Frames[G, A, A]

  /** a frame on a segment: `f`'s value flows into `rest` */
  def frame[A, B, S, T](f: A => Freer[G, S, T, B], rest: Frames[G, B, S]): Frames[G, A, T]

  /** a boundary over the stack `rest`: the level inside answers `T`, and `out` takes that on; `tag` marks it */
  def bound[T, U, R](tag: Tag[T] | Null, out: Frames[G, T, U], rest: Stack[G, U, R]): Stack[G, T, R]

  /** a continuation mark on a segment: transparent — the segment's types are its own */
  def mark[A, S](m: Mark, rest: Frames[G, A, S]): Frames[G, A, S]

  /** the nearest continuation mark `is` holds for, from a segment out through every boundary; null when none */
  def find[A, S, T, R](k: Frames[G, A, S], m: Stack[G, T, R], is: Mark => Boolean): Mark | Null

  /** walk out from a segment and its stack to the nearest boundary whose mark `is` holds for: the capture up to
   * and including it, and what lies outside it; null when there is none */
  def cut[A, T, R](k: Frames[G, A, T], m: Stack[G, T, R], is: Tag[?] => Boolean): Found[G, A, R] | Null

  /** a captured continuation put back over `out` and `m`, its boundaries outermost first, `a` into the innermost */
  def reinstall[A0, Y, U, R](cap: Cap[G, A0, Y], a: A0, out: Frames[G, Y, U], m: Stack[G, U, R]): Next[G, R]

/**
 * A STEP: what the machine does with one of effect `G`'s operations in a program over the row `H` — given the
 * segment up to the nearest boundary and the stack under it, the next state, built with the machine's
 * primitives. `G[S, T, A]` is the operation at the program's indexes; the equations its own GADT match supplies
 * type the state it answers. Alone, `H` is `G`; under other effects, `Delimited.Row[G, F]`.
 */
trait Step[G[_, _, +_], H[_, _, +_]]:
  def step[A, S, T, R](op: G[S, T, A], k: Frames[H, A, S], m: Stack[H, T, R], machine: Delimited[H]): Delimited.Next[H, R]

/** a SEGMENT: `Bind`'s continuations up to the nearest boundary, as data — from a value `A` to the answer `S`
 * of its level. Contravariant in `A`: it consumes a value. */
enum Frames[G[_, _, +_], -A, S]:
  case End[G[_, _, +_], A]() extends Frames[G, A, A]
  case Frame[G[_, _, +_], A, B, S, T](f: A => Freer[G, S, T, B], rest: Frames[G, B, S]) extends Frames[G, A, T]
  /** a CONTINUATION MARK (Clements & Felleisen's `with-continuation-mark`; Kiselyov, Shan & Sabry's delimited
   * dynamic binding): not a boundary — a value passes it unchanged, a capture carries it, no answer type moves —
   * only something an effect finds by walking (`find`) */
  case Marked[G[_, _, +_], A, S](mark: Delimited.Mark, rest: Frames[G, A, S]) extends Frames[G, A, S]

/** THE STACK: segments joined by boundaries, from the innermost level's answer `T` to the whole run's `R`. A
 * boundary takes its level's answer to the segment outside it, as that segment's value. */
enum Stack[G[_, _, +_], T, R]:
  case Done[G[_, _, +_], R]() extends Stack[G, R, R]
  /** a boundary: the level inside answers `T`; outside it, `out` takes that on to the levels below. An effect may
   * mark it (`tag`) to find it again. */
  case Bound[G[_, _, +_], T, U, R](tag: Delimited.Tag[T] | Null, out: Frames[G, T, U], rest: Stack[G, U, R])
    extends Stack[G, T, R]

object Delimited:

  /** a machine for one effect, alone */
  def apply[G[_, _, +_]](steps: Step[G, G]): Machine[G] = Machine(steps)

  /** a machine for effect `G` under the effects `F`: it answers `G`'s operations and leaves `F`'s in the program
   * it answers, for the machine outside */
  def under[G[_, _, +_], F[+_]](steps: Step[G, Row[G, F]]): Under[G, F] = Under(steps)

  // ---- the row of a machine under others ----

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

  /** a boundary's mark, opaque to the machine, which an effect finds a boundary by: `T` the answer of its level */
  trait Tag[T]

  /** a continuation mark's content, opaque to the machine: what an effect hangs on the stack to find again */
  trait Mark

  /** the machine's state, which a step answers: a program, its segment, the stack under it */
  sealed abstract class Next[G[_, _, +_], R]:
    type A
    type S
    type T
    def c: Freer[G, S, T, A]
    def k: Frames[G, A, S]
    def m: Stack[G, T, R]

  /** the boundaries a capture crossed, innermost first, the last outermost */
  enum Rev[G[_, _, +_], A0, A]:
    case Nil[G[_, _, +_], A0]() extends Rev[G, A0, A0]
    case Snoc[G[_, _, +_], A0, A, T](prev: Rev[G, A0, A], k: Frames[G, A, T], tag: Tag[T] | Null) extends Rev[G, A0, T]

  /** a captured continuation, from `A0` up to and including a marked boundary whose level answers `Y`: the
   * boundaries crossed, the marked level's own segment, its mark */
  sealed abstract class Cap[G[_, _, +_], A0, Y]:
    type A
    def rev: Rev[G, A0, A]
    def k: Frames[G, A, Y]
    def tag: Tag[Y]

  /** what a capture found: the continuation up to the marked boundary, and what lies outside it */
  sealed abstract class Found[G[_, _, +_], A0, R]:
    type Y
    type U
    def cap: Cap[G, A0, Y]
    def out: Frames[G, Y, U]
    def m: Stack[G, U, R]

  // ---- the implementations ----

  /** the primitives over `Frames` and `Stack`, shared by every machine */
  abstract class Primitives[G[_, _, +_]] extends Delimited[G]:

    def end[A]: Frames[G, A, A] = Frames.End()
    def frame[A, B, S, T](f: A => Freer[G, S, T, B], rest: Frames[G, B, S]): Frames[G, A, T] = Frames.Frame(f, rest)
    def bound[T, U, R](tag: Tag[T] | Null, out: Frames[G, T, U], rest: Stack[G, U, R]): Stack[G, T, R] =
      Stack.Bound(tag, out, rest)
    def mark[A, S](m: Mark, rest: Frames[G, A, S]): Frames[G, A, S] = Frames.Marked(m, rest)

    @tailrec final def find[A, S, T, R](k: Frames[G, A, S], m: Stack[G, T, R], is: Mark => Boolean): Mark | Null = k match
      case Frames.Marked(mk, rest) => if is(mk) then mk else find(rest, m, is)
      case Frames.Frame(_, rest) => find(rest, m, is)
      case Frames.End() => m match
        case Stack.Done() => null
        case Stack.Bound(_, out, rest) => find(out, rest, is)

    def next[A0, S0, T0, R](c0: Freer[G, S0, T0, A0], k0: Frames[G, A0, S0], m0: Stack[G, T0, R]): Next[G, R] =
      new Next[G, R]:
        type A = A0
        type S = S0
        type T = T0
        def c = c0
        def k = k0
        def m = m0

    def cut[A, T, R](k: Frames[G, A, T], m: Stack[G, T, R], is: Tag[?] => Boolean): Found[G, A, R] | Null =
      cutFrom(Rev.Nil[G, A](), k, m, is)

    @tailrec private def cutFrom[A0, A, T, R](rev: Rev[G, A0, A], k: Frames[G, A, T], m: Stack[G, T, R],
                                              is: Tag[?] => Boolean): Found[G, A0, R] | Null = m match
      case Stack.Done() => null
      case b: Stack.Bound[G, T, u, R] =>
        val t = b.tag
        if t != null && is(t) then found(rev, k, t.nn, b.out, b.rest)
        else cutFrom(Rev.Snoc(rev, k, t), b.out, b.rest, is)

    private def found[A0, A1, Y0, U0, R](r: Rev[G, A0, A1], k1: Frames[G, A1, Y0], t: Tag[Y0],
                                         out0: Frames[G, Y0, U0], m0: Stack[G, U0, R]): Found[G, A0, R] =
      new Found[G, A0, R]:
        type Y = Y0
        type U = U0
        def cap = new Cap[G, A0, Y0]:
          type A = A1
          def rev = r
          def k = k1
          def tag = t
        def out = out0
        def m = m0

    def reinstall[A0, Y, U, R](cap: Cap[G, A0, Y], a: A0, out: Frames[G, Y, U], m: Stack[G, U, R]): Next[G, R] =
      link(cap.rev, cap.k, Stack.Bound(cap.tag, out, m), a)

    @tailrec private def link[A0, A, T, R](rev: Rev[G, A0, A], k: Frames[G, A, T], m: Stack[G, T, R], a: A0): Next[G, R] =
      rev match
        case Rev.Nil() => next(Return(a), k, m)
        case Rev.Snoc(prev, k0, tag) => link(prev, k0, Stack.Bound(tag, k, m), a)

    /** this run's room for nested runs (`StackSwitch`): the depth left on this stack */
    private var room: Int = StackSwitch.firstRoom

    /** `body` one level deeper, on a fresh stack when there is no room left here */
    protected final def deeper[X](body: => X): X =
      val here = room - 1
      if here > 0 then within(here, body)
      else StackSwitch.fresh(fresh => within(fresh, body))

    private def within[X](left: Int, body: => X): X =
      val saved = room
      room = left
      try body finally room = saved

  /** the machine for one effect, alone: every operation is its own */
  final class Machine[G[_, _, +_]](steps: Step[G, G]) extends Primitives[G]:

    /** run `c` with `k` as the last frame of its continuation */
    def run[A, S, R](c: Freer[G, S, R, A], k: A => S): R =
      go(c, frame((a: A) => Return[G, S, S](k(a)), end[S]), Stack.Done[G, R]())

    def force[A, S](k: Frames[G, A, S], x: A): S = deeper(go(Return[G, S, A](x), k, Stack.Done[G, S]()))

    @tailrec private def go[A, S, T, R](c: Freer[G, S, T, A], k: Frames[G, A, S], m: Stack[G, T, R]): R = c match
      case Return(a) => k match
        case Frames.End() => m match
          case Stack.Done() => a
          case Stack.Bound(_, out, rest) => go(Return(a), out, rest)
        case Frames.Frame(f, k2) => go(f(a), k2, m)
        case Frames.Marked(_, k2) => go(Return(a), k2, m)
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
  final class Under[G[_, _, +_], F[+_]](steps: Step[G, Row[G, F]]) extends Primitives[Row[G, F]]:
    private type H[S, R, A] = Sum[G, F, S, R, A]

    /** run `c` with `k` as the last frame of its continuation: the program, over `F`, that answers `R` */
    def run[A, S, R](c: Freer[H, S, R, A], k: A => S): Free[F, R] =
      go(c, frame((a: A) => Return[H, S, S](k(a)), end[S]), Stack.Done[H, R]())

    /** a strict `k` cannot wait for an outer effect: one met inside it is refused, by name */
    def force[A, S](k: Frames[H, A, S], x: A): S = deeper(go(Return[H, S, A](x), k, Stack.Done[H, S]())) match
      case Return(s) => s
      case _ => throw IllegalStateException("a strict k performed an operation of an outer effect; give its body the lazy k")

    @tailrec private def go[A, S, T, R](c: Freer[H, S, T, A], k: Frames[H, A, S], m: Stack[H, T, R]): Free[F, R] = c match
      case Return(a) => k match
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Bound(_, out, rest) => go(Return(a), out, rest)
        case Frames.Frame(f, k2) => go(f(a), k2, m)
        case Frames.Marked(_, k2) => go(Return(a), k2, m)
      case Bind(c0, f) => go(c0, Frames.Frame(f, k), m)
      case Delay(t) => go(t(), k, m)
      case Inject(op) => op match
        case Sum.Own(o) =>
          val n = steps.step(o, k, m, this)
          go(n.c, n.k, n.m)
        case Sum.Fwd(o) => forward(o, k, m)
      case Diag(op) => go(Inject(op), k, m)

    /** an `F` operation out, as a node of the answered program; its answer enters this machine again, in a `Delay` */
    private def forward[X, S, R](o: F[X], k: Frames[H, X, S], m: Stack[H, S, R]): Free[F, R] =
      Bind(Inject[Freer.Lift[F], Unit, Unit, X](o), (x: X) => Delay(() => again(Return[H, S, X](x), k, m)))

    /** the loop entered again from the outside (the forwarded operation's answer): a call, so `go` stays a loop */
    private def again[A, S, T, R](c: Freer[H, S, T, A], k: Frames[H, A, S], m: Stack[H, T, R]): Free[F, R] = go(c, k, m)
