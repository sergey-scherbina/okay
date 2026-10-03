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
 * Biernacka, Biernacki & Danvy 2005), segmented as Dybvig, Peyton Jones & Sabry's (JFP 2007) is:
 *
 *  - `Frames`, a SEGMENT, the operator's `A => F[B, S, R]`: `Bind`'s continuations as data, joined as `Bind` joins;
 *    a MARK in it is transparent, what an effect finds again (an environment);
 *  - `Stack`: segments joined by BOUNDARIES, of two kinds, each typed by its installation — a VALUE boundary
 *    (`Delim`: a value passes, the answer types chain through, a capture to it stops there: a prompt, a nested
 *    run's barrier) and an ANSWER boundary (`Bound`: the level closes and its answer flows out as a value —
 *    Danvy & Filinski's `reset`, answer-type modification typed with no claim).
 *
 * A capture walks boundaries, never frames: to the nearest it takes the segment as it is. `Delimited` is the
 * interface — the machine's primitives, which an effect's `Step` answers its operations with. Nothing here names
 * an effect.
 */
trait Delimited[G[_, _, +_]]:
  import Delimited.{Mark, Tag, Next, Piece, Found, Closed}

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

  /** a VALUE boundary over `rest`, marked `tag`: the level's value goes on into `out`, its answers chain through */
  def delim[B, S, R, B2, S2, Z](tag: Mark | Null, out: Frames[G, B, B2, S2, S], rest: Stack[G, B2, S2, R, Z]): Stack[G, B, S, R, Z]

  /** an ANSWER boundary over `rest`: the level inside closes and answers `R`; `out` takes that on */
  def bound[S, R, B2, S2, X, Z](tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X], rest: Stack[G, B2, S2, X, Z]): Stack[G, S, S, R, Z]

  /** the nearest mark `is` holds for, from a segment out through every boundary; null when none */
  def find[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null

  /** walk out over value boundaries to the nearest whose mark `is` holds for: the piece up to and including it,
   * and what lies under it; null when there is none, or a boundary `stop` holds for comes first */
  def cut[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean,
                            stop: Mark => Boolean): Found[G, A, T, R, Z] | Null

  /** a captured piece put back on top of a segment and its stack, and `c` run inside it: its value into the
   * piece's innermost frames, its captures reaching the piece's boundaries (DPJS's `pushSubCont`) */
  def reinstall[A0, Y, I, B, S, R, Z](piece: Piece[G, A0, R, Y, I], c: Freer[G, R, R, A0], k: Frames[G, Y, B, S, I], m: Stack[G, B, S, R, Z]): Next[G, Z]

  /** the nearest boundary as an ANSWER boundary, the segment up to it closed by it: null when it is not one */
  def closed[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Closed[G, A, T, R, Z] | Null

  /** the nearest BOUNDARY whose mark `is` holds for, walking the boundaries only, never a frame; null when none */
  def holds[B, S, R, Z](m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null

  /** from now on this run calls user code under a `try`, and a throw goes to the step's `thrown` (a catch frame
   * was installed): until then a run pays nothing for exceptions */
  def guarding(): Unit

/**
 * A STEP: what the machine does with one of effect `G`'s operations in a program over the row `H` — given the
 * segment up to the nearest boundary and the stack under it, the next state, built with the machine's
 * primitives. `G[T, R, A]` is the operation at the program's indexes; the equations its own GADT match supplies
 * type the state it answers.
 */
trait Step[G[_, _, +_], H[_, _, +_]]:
  def step[A, B, S, T, R, Z](op: G[T, R, A], k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z],
                             machine: Delimited[H]): Delimited.Next[H, Z]

  /** a throw from user code at segment `k` and stack `m`, in a run that is `guarding`: the next state, or null —
   * nobody here takes it, and it is thrown on */
  def thrown[A, B, S, T, R, Z](t: Throwable, k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z],
                               machine: Delimited[H]): Delimited.Next[H, Z] | Null = null

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
  /** a VALUE boundary: the level's value flows on into `out`; its answers chain through, as `Bind`'s do */
  case Delim[G[_, _, +_], B, S, R, B2, S2, Z](tag: Delimited.Mark | Null, out: Frames[G, B, B2, S2, S],
                                              rest: Stack[G, B2, S2, R, Z]) extends Stack[G, B, S, R, Z]
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

  /** a machine for a `Free` row `H` whose effects outside are `F`: it answers every operation of `H` but those
   * `outer` sends out, which leave in the program it answers */
  def over[H[+_], F[+_]](steps: Step[Freer.Lift[H], Freer.Lift[H]], outer: Outer[H, F]): Over[H, F] = Over(steps, outer)

  /** what a machine for a `Free` row needs told by the effect running the row */
  trait Outer[H[+_], F[+_]]:
    /** `op` as an operation of the effects outside, or null when this machine answers it — which may depend on
     * the boundaries on its stack `m` (`machine.holds`) */
    def apply[X, B, S, R, Z](op: H[X], m: Stack[Freer.Lift[H], B, S, R, Z], machine: Delimited[Freer.Lift[H]]): F[X] | Null
    /** an operation sent out stands at one index: in a `Free` row every node does */
    def diagonal[T, R]: T =:= R
    /** a deferred run of this row this machine steps into rather than forces (a nested run): its program, or null */
    def enter[T, R, A](t: () => Freer[Freer.Lift[H], T, R, A]): Freer[Freer.Lift[H], T, R, A] | Null
    /** the mark of the value boundary stepping into `t` installs (a barrier), or null for none */
    def barrier(t: () => Any): Mark | Null

  // ---- what the primitives speak of ----

  /** what an effect hangs on the stack to find again; opaque to the machine */
  trait Mark

  /** an answer boundary's mark, with `T` the answer of its level */
  trait Tag[T] extends Mark

  /**
   * a throw from user code, as the program a guarded call answers instead (`guarding`): a `Delay` of this. The
   * machine hands it to its step's `thrown`; anything else that forces it throws it again, the same object — so
   * it is a correct program everywhere, and typed at any answer with no claim (`() => Nothing`)
   */
  final class Thrown(val t: Throwable) extends (() => Nothing):
    def apply(): Nothing = throw t

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

  /** a captured piece of the stack, built outward: from the hole `A0` (at index `T0`) through segments and the
   * value boundaries between them to `A` (at index `T`) */
  enum Piece[G[_, _, +_], A0, T0, A, T]:
    case Nil[G[_, _, +_], A0, T0]() extends Piece[G, A0, T0, A0, T0]
    case Snoc[G[_, _, +_], A0, T0, A, T, B, S](prev: Piece[G, A0, T0, A, T], k: Frames[G, A, B, S, T], tag: Mark | Null)
      extends Piece[G, A0, T0, B, S]

  /** what a capture found: the piece up to the marked boundary, and what lies under it */
  sealed abstract class Found[G[_, _, +_], A0, T0, R, Z]:
    type Y
    type I
    type B2
    type S2
    def piece: Piece[G, A0, T0, Y, I]
    def tag: Mark
    def out: Frames[G, Y, B2, S2, I]
    def rest: Stack[G, B2, S2, R, Z]

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

  // ---- the implementations ----

  /** the primitives over `Frames` and `Stack`, shared by every machine, and its room for nested runs */
  abstract class Core[G[_, _, +_]] extends Delimited[G]:
    private var room: Int = StackSwitch.firstRoom

    /** user code runs under a `try` (`guarding`) */
    @scala.annotation.publicInBinary protected var guarded: Boolean = false
    def guarding(): Unit = guarded = true

    /** a call of user code: under a `try` once the run is `guarding`, a throw answered as a `Thrown` program */
    protected final inline def call[T, R, A](inline body: Freer[G, T, R, A]): Freer[G, T, R, A] =
      if !guarded then body
      else
        try body
        catch case t: Throwable => Delay(Thrown(t))

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
    def delim[B, S, R, B2, S2, Z](tag: Mark | Null, out: Frames[G, B, B2, S2, S], rest: Stack[G, B2, S2, R, Z]): Stack[G, B, S, R, Z] =
      Stack.Delim(tag, out, rest)
    def bound[S, R, B2, S2, X, Z](tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X], rest: Stack[G, B2, S2, X, Z]): Stack[G, S, S, R, Z] =
      Stack.Bound(tag, out, rest)

    @tailrec final def find[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null =
      k match
        case Frames.Marked(mk, rest) => if is(mk) then mk else find(rest, m, is)
        case Frames.Frame(_, rest) => find(rest, m, is)
        case Frames.End() => m match
          case Stack.Delim(_, out, rest) => find(out, rest, is)
          case Stack.Bound(_, out, rest) => find(out, rest, is)
          case _ => null

    def cut[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean,
                              stop: Mark => Boolean): Found[G, A, T, R, Z] | Null =
      cutFrom(Piece.Nil[G, A, T](), k, m, is, stop)

    @tailrec private def cutFrom[A0, T0, A, B, S, T, R, Z](piece: Piece[G, A0, T0, A, T], k: Frames[G, A, B, S, T],
                                                           m: Stack[G, B, S, R, Z], is: Mark => Boolean,
                                                           stop: Mark => Boolean): Found[G, A0, T0, R, Z] | Null = m match
      case Stack.Delim(tag, out, rest) =>
        if tag != null && is(tag) then found(Piece.Snoc(piece, k, tag), tag, out, rest)
        else if tag != null && stop(tag) then null
        else cutFrom(Piece.Snoc(piece, k, tag), out, rest, is, stop)
      case _ => null

    private def found[A0, T0, Y0, I0, B20, S20, R, Z](p: Piece[G, A0, T0, Y0, I0], t: Mark, o: Frames[G, Y0, B20, S20, I0],
                                                      r: Stack[G, B20, S20, R, Z]): Found[G, A0, T0, R, Z] =
      new Found[G, A0, T0, R, Z]:
        type Y = Y0
        type I = I0
        type B2 = B20
        type S2 = S20
        def piece = p
        def tag = t
        def out = o
        def rest = r

    def reinstall[A0, Y, I, B, S, R, Z](piece: Piece[G, A0, R, Y, I], c: Freer[G, R, R, A0], k: Frames[G, Y, B, S, I], m: Stack[G, B, S, R, Z]): Next[G, Z] =
      link(piece, k, m, c)

    @tailrec private def link[A0, R, A, T, B, S, Z](piece: Piece[G, A0, R, A, T], k: Frames[G, A, B, S, T],
                                                    m: Stack[G, B, S, R, Z], c: Freer[G, R, R, A0]): Next[G, Z] = piece match
      case Piece.Nil() => next(c, k, m)
      case Piece.Snoc(prev, kk, tag) => link(prev, kk, Stack.Delim(tag, k, m), c)

    @tailrec final def holds[B, S, R, Z](m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null = m match
      case Stack.Delim(tag, _, rest) => if tag != null && is(tag) then tag else holds(rest, is)
      case Stack.Bound(tag, _, rest) => if tag != null && is(tag) then tag else holds(rest, is)
      case _ => null

    def closed[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Closed[G, A, T, R, Z] | Null = m match
      case Stack.Bound(tag, out, rest) => closedBy(k, tag, out, rest)
      case Stack.Answered() => atTop[A, S, T, R](k)
      case _ => null

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
            case Stack.Delim(_, out, rest) => go(Return(a), out, rest)
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
            case Stack.Delim(_, out, rest) => go(Return(a), out, rest)
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

  /**
   * the machine for a `Free` row `H` under the effects `F`: an operation `outer` sends out leaves, as a node of the
   * program the machine answers, with this machine's state behind it in a `Delay`; every other goes to its steps.
   * A nested run of the same row (`outer.enter`) is stepped into, under a value boundary, not forced: its depth is
   * this machine's stack, not the host's.
   */
  final class Over[H[+_], F[+_]](steps: Step[Freer.Lift[H], Freer.Lift[H]], outer: Outer[H, F]) extends Core[Freer.Lift[H]]:
    private type L[S, R, A] = Freer.Lift[H][S, R, A]

    /** run `c` to its value: the program, over `F`, that answers it */
    def value[A, X](c: Freer[L, X, X, A]): Free[F, A] = go(c, end[A, X], Stack.Done[L, A, X]())

    /** a strict `k` cannot wait for an outer effect: one met inside it is refused, by name */
    def force[A, S, T](k: Frames[L, A, S, S, T], x: A): T = deeper(go(Return[L, T, A](x), k, Stack.Answered[L, S, T]())) match
      case Return(t) => t
      case _ => throw IllegalStateException("a strict k performed an operation of an outer effect; give its body the lazy k")

    @tailrec private def go[A, B, S, T, R, Z](c: Freer[L, T, R, A], k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z]): Free[F, Z] =
      c match
        case Return(a) => k match
          case Frames.Frame(f, k2) => go(call(f(a)), k2, m)
          case Frames.Marked(_, k2) => go(Return(a), k2, m)
          case Frames.End() => m match
            case Stack.Done() => Return(a)
            case Stack.Answered() => Return(a)
            case Stack.Delim(_, out, rest) => go(Return(a), out, rest)
            case Stack.Bound(_, out, rest) => go(Return(a), out, rest)
        case Bind(c0, f) => go(c0, Frames.Frame(f, k), m)
        case Delay(t) => t match
          case th: Thrown =>
            val n = steps.thrown(th.t, k, m, this)
            if n == null then throw th.t
            go(n.c, n.k, n.m)
          case _ => outer.enter(t) match
            case null => go(call(t()), k, m)
            case inner => go(inner, end[A, T], Stack.Delim(outer.barrier(t), k, m))
        case Inject(op) => outer(op, m, this) match
          case null =>
            val n =
              if !guarded then steps.step(op, k, m, this)
              else
                try steps.step(op, k, m, this)
                catch case t: Throwable => next(Delay(Thrown(t)), k, m)
            go(n.c, n.k, n.m)
          case o => forward(o.nn, k, outer.diagonal[R, T].substituteCo[[r] =>> Stack[L, B, S, r, Z]](m))
        case Diag(op) => go(Inject(op), k, m)

    /** an `F` operation out, as a node of the answered program; its answer enters this machine again, in a `Delay` */
    private def forward[X, B, S, T, Z](o: F[X], k: Frames[L, X, B, S, T], m: Stack[L, B, S, T, Z]): Free[F, Z] =
      Bind(Inject[Freer.Lift[F], Unit, Unit, X](o), (x: X) => Delay(() => again(Return[L, T, X](x), k, m)))

    /** the loop entered again from the outside: a call, so `go` stays a loop */
    private def again[A, B, S, T, R, Z](c: Freer[L, T, R, A], k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z]): Free[F, Z] =
      go(c, k, m)
