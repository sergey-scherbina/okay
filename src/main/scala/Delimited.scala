package okay

import okay.Freer.{Return, Inject, Bind, Delay, Diag}
import scala.annotation.tailrec

// ======================================================================
// THE ABSTRACT MACHINE FOR FREER, effect-independent (specs/cont-atm.md;
// the operator: "Freer/Delimited должны быть полностью независимы от того
// какой именно эффект в нем работает — их задача предоставить все
// необходимые примитивы абстрактной машины для реализации эффектов").
//
// An interpreter of `Freer` written directly is correct and typed by
// `Freer`'s own indexes, but grows the host stack in two places: the
// continuations of `Bind` (opaque lambdas) and the interpreter's own
// nested runs. This machine is that host stack made DATA, both places,
// each aligned by its types (the functional correspondence: Ager,
// Biernacki, Danvy & Midtgaard 2003; Biernacka, Biernacki & Danvy 2005):
//   `K` — `Bind`'s continuations, from a value to an answer;
//   `M` — the boundaries between nested runs, each with its own answer.
// The types are inherited, not added: `K`'s are `Bind`'s indexes, `M`'s an
// interpreter's own. The machine runs `Return`, `Bind`, `Delay`; an
// operation goes to the EFFECT, which answers with the next state, built
// from the primitives here. Nothing here names an effect. No cast.
// ======================================================================

object Delimited:

  /**
   * `Bind`'s continuations up to the nearest boundary, as data: from a value `A` to the answer `S` they deliver
   * at that boundary. Contravariant in `A`: it consumes a value.
   */
  enum K[G[_, _, +_], -A, S]:
    case Done[G[_, _, +_], A]() extends K[G, A, A]
    case Push[G[_, _, +_], A, B, S, T](f: A => Freer[G, S, T, B], k: K[G, B, S]) extends K[G, A, T]

  /**
   * The boundaries between nested runs, as data, each with its own answer: from the innermost level's `T` to
   * the whole run's `R`. A level's answer arrives at its boundary and goes on outside it.
   */
  enum M[G[_, _, +_], T, R]:
    case Top[G[_, _, +_], R]() extends M[G, R, R]
    /** a boundary: the level inside answers `T`, and outside it `out` takes that on to the levels below; an
     * effect may mark it (`tag`) to find it again */
    case Level[G[_, _, +_], T, U, R](tag: Tag[T] | Null, out: K[G, T, U], m: M[G, U, R]) extends M[G, T, R]

  /** a boundary's mark, opaque to the machine, which an effect names a boundary by: `T` is the answer of the
   * level it marks */
  trait Tag[T]

  // ---- the machine's primitives over the stack, for an effect to compose ----

  /** the levels a capture crossed, innermost first, the last outermost */
  enum Rev[G[_, _, +_], A0, A]:
    case Nil[G[_, _, +_], A0]() extends Rev[G, A0, A0]
    case Snoc[G[_, _, +_], A0, A, T](prev: Rev[G, A0, A], k: K[G, A, T], tag: Tag[T] | Null) extends Rev[G, A0, T]

  /** a captured continuation, from `A0` up to and including a marked boundary whose level answers `Y`: the
   * levels crossed, the marked level's own continuation, its mark */
  sealed abstract class Cap[G[_, _, +_], A0, Y]:
    type A
    def rev: Rev[G, A0, A]
    def k: K[G, A, Y]
    def tag: Tag[Y]

  /** what a capture found: the continuation up to the marked boundary, and what lies outside it */
  sealed abstract class Found[G[_, _, +_], A0, R]:
    type Y
    type U
    def cap: Cap[G, A0, Y]
    def out: K[G, Y, U]
    def m: M[G, U, R]

  /** walk out from the live `k` and `m` to the nearest boundary whose mark `is` holds for: the capture up to and
   * including it, and what lies outside it; null when there is none */
  def cut[G[_, _, +_], A, T, R](k: K[G, A, T], m: M[G, T, R], is: Tag[?] => Boolean): Found[G, A, R] | Null =
    cutFrom(Rev.Nil[G, A](), k, m, is)

  @tailrec private def cutFrom[G[_, _, +_], A0, A, T, R](rev: Rev[G, A0, A], k: K[G, A, T], m: M[G, T, R],
                                                         is: Tag[?] => Boolean): Found[G, A0, R] | Null = m match
    case M.Top() => null
    case l: M.Level[G, T, u, R] =>
      val t = l.tag
      if t != null && is(t) then found(rev, k, t.nn, l.out, l.m)
      else cutFrom(Rev.Snoc(rev, k, t), l.out, l.m, is)

  private def found[G[_, _, +_], A0, A1, Y0, U0, R](r: Rev[G, A0, A1], k1: K[G, A1, Y0], t: Tag[Y0],
                                                   out0: K[G, Y0, U0], m0: M[G, U0, R]): Found[G, A0, R] =
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

  /** a captured continuation put back over `out` and `m`, its levels outermost first, `a` into the innermost */
  def reinstall[G[_, _, +_], A0, Y, U, R](cap: Cap[G, A0, Y], a: A0, out: K[G, Y, U], m: M[G, U, R]): Next[G, R] =
    link(cap.rev, cap.k, M.Level(cap.tag, out, m), a)

  @tailrec private def link[G[_, _, +_], A0, A, T, R](rev: Rev[G, A0, A], k: K[G, A, T], m: M[G, T, R], a: A0): Next[G, R] =
    rev match
      case Rev.Nil() => Next(Return(a), k, m)
      case Rev.Snoc(prev, k0, tag) => link(prev, k0, M.Level(tag, k, m), a)

  /** the machine's state, which an effect's step answers: a program, its continuation, its boundaries */
  sealed abstract class Next[G[_, _, +_], R]:
    type A
    type S
    type T
    def c: Freer[G, S, T, A]
    def k: K[G, A, S]
    def m: M[G, T, R]

  object Next:
    def apply[G[_, _, +_], A0, S0, T0, R](c0: Freer[G, S0, T0, A0], k0: K[G, A0, S0], m0: M[G, T0, R]): Next[G, R] =
      new Next[G, R]:
        type A = A0
        type S = S0
        type T = T0
        def c = c0
        def k = k0
        def m = m0

  /**
   * AN EFFECT: what the machine does with one of its operations, given the continuation up to the nearest
   * boundary and the boundaries below — the next state, built from the primitives here. `G[S, T, A]` is the
   * operation at the program's indexes; the equations its own GADT match supplies type the state it answers.
   */
  trait Effect[G[_, _, +_]]:
    def step[A, S, T, R](op: G[S, T, A], k: K[G, A, S], m: M[G, T, R], run: Run[G]): Next[G, R]

  /** a run: its effect, and its room for nested runs (`StackSwitch`) — the depth left on this stack */
  final class Run[G[_, _, +_]](val effect: Effect[G], var room: Int):

    /** a continuation from `x` to its answer, NOW: a nested run, counted; at no room left on a fresh stack */
    def force[A, S](k: K[G, A, S], x: A): S =
      val here = room - 1
      if here > 0 then nested(here, k, x)
      else StackSwitch.fresh(fresh => nested(fresh, k, x))

    private def nested[A, S](left: Int, k: K[G, A, S], x: A): S =
      val saved = room
      room = left
      try go(Return[G, S, A](x), k, M.Top[G, S](), this) finally room = saved

  /** run `c` with `k` as the last frame of its continuation */
  def run[G[_, _, +_], A, S, R](c: Freer[G, S, R, A], k: A => S, effect: Effect[G]): R =
    go(c, K.Push((a: A) => Return[G, S, S](k(a)), K.Done[G, S]()), M.Top[G, R](), Run(effect, StackSwitch.firstRoom))

  /** a continuation from `x` to its answer, on a run of its own */
  def runK[G[_, _, +_], A, S](k: K[G, A, S], x: A, effect: Effect[G]): S =
    go(Return[G, S, A](x), k, M.Top[G, S](), Run(effect, StackSwitch.firstRoom))

  @tailrec private def go[G[_, _, +_], A, S, T, R](c: Freer[G, S, T, A], k: K[G, A, S], m: M[G, T, R], run: Run[G]): R =
    c match
      case Return(a) => k match
        case K.Done() => m match
          case M.Top() => a
          case M.Level(_, out, m2) => go(Return(a), out, m2, run)
        case K.Push(f, k2) => go(f(a), k2, m, run)
      case Bind(c0, f) => go(c0, K.Push(f, k), m, run)
      case Delay(t) => go(t(), k, m, run)
      case Inject(op) =>
        val n = run.effect.step(op, k, m, run)
        go(n.c, n.k, n.m, run)
      case Diag(op) => go(Inject(op), k, m, run)
