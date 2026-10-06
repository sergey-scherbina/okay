package okay.cont

import scala.annotation.tailrec
import Cont.*

/** a captured piece: the segments from the hole up to the delimiter, it not included, composed as one segment — a
 * segment is one (`Frames`), `Over` is one over the next, `Crossed` one under a delimiter crossed */
sealed trait Piece[-A0, B, I <: Tuple, O <: Tuple]
/** a segment: `A => Cont[I, O, B]` as data, composed as `Bind` composes; contravariant in what it consumes */
enum Frames[-A, B, I <: Tuple, O <: Tuple] extends Piece[A, B, I, O]:
  case End[A, Σ <: Tuple]() extends Frames[A, A, Σ, Σ]
  case Frame[A, X, B, I <: Tuple, T <: Tuple, O <: Tuple](f: A => Cont[T, O, X], rest: Frames[X, B, I, T]) extends Frames[A, B, I, O]
/** a piece over the next segment: the hole's segments end in `X` at `T`, the segment takes `X` to `B` */
final case class Over[A0, X, B, I <: Tuple, T <: Tuple, O <: Tuple](prev: Piece[A0, X, T, O], out: Frames[X, B, I, T]) extends Piece[A0, B, I, O]
/** a piece over a level CROSSED: the inner level's piece, up to its delimiter's value `S1`, and that delimiter's own
 * frames at this level, taking its answer `R1` to `B` — the delimiter put back on a resumption */
final case class Crossed[D1 <: Tuple, S1, R1, X, B, I2 <: Tuple](inner: Piece[X, S1, At[D1, S1] *: D1, At[D1, R1] *: D1], out: Frames[R1, B, I2, D1])
  extends Piece[X, B, I2, D1]

/**
 * THE STACK: closes a level — value `B`, from `I` to `O` — into the run's result, from `I0` to `O0`, value `Z`. A
 * delimiter's level has value = answer (`S`, `S`) and its own level on top, its `D` the stacks outside; when it
 * returns, the GADT says that answer is its final one, `R`, and `out` takes it as a value outside.
 */
enum Stack[B, I <: Tuple, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z]:
  case Done[B, I <: Tuple, O <: Tuple]() extends Stack[B, I, O, I, O, B]
  case Run[B, I <: Tuple, O <: Tuple, B2, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](out: Frames[B, B2, I2, I], rest: Stack[B2, I2, O, I0, O0, Z])
    extends Stack[B, I, O, I0, O0, Z]
  case Delim[D <: Tuple, O <: Tuple, S, R, B, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](out: Frames[R, B, I2, D], rest: Stack[B, I2, O, I0, O0, Z])
    extends Stack[S, At[D, S] *: D, At[D, R] *: O, I0, O0, Z]

/** a captured continuation: the piece from the hole, `X` at the answer `T` with the stacks `I` outside, to its
 * delimiter's level, value-and-answer `S`, the stacks `D` outside; put back under a delimiter of its own, it
 * delivers the answer at the hole, outside, from `D` to `I` */
final class Captured[D <: Tuple, I <: Tuple, X, T, S](val piece: Piece[X, S, At[D, S] *: D, At[D, T] *: I]) extends (X => Cont[D, I, T]):
  /** the last `k(x)` made: a clause that returns it as its whole body resumed once, last — answered in place */
  private var last: Resume[D, I, X, T] | Null = null
  def apply(x: X): Cont[D, I, T] =
    val r: Resume[D, I, X, T] = Resume(x, this)
    last = r
    r
  /** the body a clause returned, if it is the last `k(x)`: its `x`, typed by this capture */
  def tail(body: Cont[?, ?, ?]): Resume[D, I, X, T] | Null =
    val r = last
    if r != null && (body eq r) then r else null

/** the machine's state with a value `A` due: the segment `k` over `m`. As a function it is the rest of a run after
 * something handed out: applied by whoever answers it, the run goes on */
sealed abstract class Resumption[A, I0 <: Tuple, O0 <: Tuple, Z] extends (A => Cont[I0, O0, Z]):
  type B
  type I <: Tuple
  type T <: Tuple
  def k: Frames[A, B, I, T]
  def m: Stack[B, I, T, I0, O0, Z]
  def apply(a: A): Cont[I0, O0, Z] = Machine.go(Return(a), k, m)

/** the machine's next state with its program: what a capture's walk of the stack answers with, its level's types
 * its own */
sealed abstract class Step[I0 <: Tuple, O0 <: Tuple, Z]:
  type A
  type B
  type I <: Tuple
  type T <: Tuple
  type O <: Tuple
  def c: Cont[T, O, A]
  def k: Frames[A, B, I, T]
  def m: Stack[B, I, O, I0, O0, Z]

/** one loop, one rule per node, nothing else */
object Machine:
  /** run to a head form: `Return(z)`, or `Bind(shift, rest)` — a capture whose delimiter is outside this run, for
   * the machine outside. At any level: a handler's clause may run a program of its own */
  def run[I <: Tuple, O <: Tuple, A](p: Cont[I, O, A]): Cont[I, O, A] = go(p, Frames.End(), Stack.Done())

  @tailrec private[cont] def go[A, B, I <: Tuple, T <: Tuple, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      c: Cont[T, O, A], k: Frames[A, B, I, T], m: Stack[B, I, O, I0, O0, Z]): Cont[I0, O0, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest)
          // the body returned: its answer is its final one, and the delimiter delivers it
          case Stack.Delim(out, rest) => go(Return(a), out, rest)
      // a head form handed out (its rest is a `Resumption`), met at a run's bottom: as it is, a run over a run allocates nothing
      case Bind(c0, f) => f match
        case _: Resumption[?, ?, ?, ?] => k match
          case Frames.End() => m match
            case Stack.Done() => c
            case _ => go(c0, Frames.Frame(f, k), m)
          case _ => go(c0, Frames.Frame(f, k), m)
        case _ => go(c0, Frames.Frame(f, k), m)
      case Delay(t) => go(t(), k, m)
      // the four that move a level answer with the machine's next state, each where its node's types are names
      case r: Reset[?, ?, ?, ?] => val n = enter(r, k, m); go(n.c, n.k, n.m)
      case s: Shift0[d, i, o, t, r, x] => val n = cut[x, t, B, I, r, o, I0, O0, Z, d, i](s, s.f, k, m); go(n.c, n.k, n.m)
      case s: Op[n, x, dn, ansn] => val n = cutN[x, B, I, I0, O0, Z, n, dn, ansn, B, I, n](s.at, s.f, k, m, k, m); go(n.c, n.k, n.m)
      case r: Resume[?, ?, ?, ?] => val n = under(r.x, r.k.piece, Stack.Delim(k, m)); go(n.c, n.k, n.m)

  /** into the delimiter: its level pushed, the body at its start */
  private def enter[D <: Tuple, O <: Tuple, S, R, B, I <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      r: Reset[D, O, S, R], k: Frames[R, B, I, D], m: Stack[B, I, O, I0, O0, Z]): Step[I0, O0, Z] =
    step(r.body, Frames.End(), Stack.Delim(k, m))

  /** down the stack to the nearest delimiter — the index says it is the node's — and the shift's body, given `k`,
   * outside; the run's bottom instead means the delimiter is outside this run: the capture is handed out whole as a
   * program of the run, the rest of this run after it, re-closed by a `Done` at the hole — built here, where the
   * run's types are names */
  @tailrec private def cut[X, T, A, I <: Tuple, R, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z, D <: Tuple, Ik <: Tuple](
      c: Cont[At[D, T] *: Ik, At[D, R] *: O, X], f: (X => Cont[D, Ik, T]) => Cont[D, O, R],
      piece: Piece[X, A, I, At[D, T] *: Ik], m: Stack[A, I, At[D, R] *: O, I0, O0, Z]): Step[I0, O0, Z] =
    m match
      case Stack.Run(out, rest) => cut(c, f, Over(piece, out), rest)
      case d @ Stack.Delim(_, _) => found(d)[X, T, Ik](piece, f)
      case Stack.Done() =>
        val n = link(piece, Stack.Done[A, I, At[D, T] *: Ik]())
        step(Bind(c, n), Frames.End(), Stack.Done())

  private def found[D <: Tuple, O <: Tuple, S, R, B0, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](d: Stack.Delim[D, O, S, R, B0, I2, I0, O0, Z])
      [X, T, Ik <: Tuple](piece: Piece[X, S, At[D, S] *: D, At[D, T] *: Ik], f: (X => Cont[D, Ik, T]) => Cont[D, O, R]): Step[I0, O0, Z] =
    step(f(Captured(piece)), d.out, d.rest)

  /** `cut` for an operation: the walk to its handler's delimiter, `at` levels out — a delimiter between is CROSSED,
   * its record into the piece, the walk going on at the level outside, whose index the reach names; at the target,
   * the clause, and if it returned `k(x)` as its whole body, `x` where the operation was, the stack as it stands
   * (`k0`, `m0`); the run's bottom instead hands the operation out, with the reach left, as a program of the run */
  @tailrec private def cutN[X, A, I <: Tuple, I0 <: Tuple, O0 <: Tuple, Z, N <: Tuple, Dn <: Tuple, Ansn, B0, Ih <: Tuple, N0 <: Tuple](
      at: Reach[N, Dn, Ansn], f: (X => Cont[Dn, Dn, Ansn]) => Cont[Dn, Dn, Ansn],
      piece: Piece[X, A, I, N], m: Stack[A, I, N, I0, O0, Z], k0: Frames[X, B0, Ih, N0], m0: Stack[B0, Ih, N0, I0, O0, Z]): Step[I0, O0, Z] =
    m match
      case Stack.Run(out, rest) => cutN(at, f, Over(piece, out), rest, k0, m0)
      case d @ Stack.Delim(_, _) => at match
        case Reach.Here() => foundN(d)[X, B0, Ih, N0](piece, f, k0, m0)
        case Reach.Out(next) => cutN(next, f, crossed(d)[X](piece), d.rest, k0, m0)
      case Stack.Done() =>
        val n = link(piece, Stack.Done[A, I, N]())
        step(Bind(Op(at, f), n), Frames.End(), Stack.Done())

  /** the delimiter crossed: the piece so far under its record, a piece of the level outside, at the delimiter's
   * outside — which the reach names as the next level's index */
  private def crossed[D <: Tuple, S, R, B0, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](d: Stack.Delim[D, D, S, R, B0, I2, I0, O0, Z])
      [X](piece: Piece[X, S, At[D, S] *: D, At[D, R] *: D]): Piece[X, B0, I2, D] =
    Crossed(piece, d.out)

  private def foundN[D <: Tuple, S, Ans, B0, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](d: Stack.Delim[D, D, S, Ans, B0, I2, I0, O0, Z])
      [X, Bh, Ih <: Tuple, N0 <: Tuple](piece: Piece[X, S, At[D, S] *: D, At[D, Ans] *: D], f: (X => Cont[D, D, Ans]) => Cont[D, D, Ans],
      k0: Frames[X, Bh, Ih, N0], m0: Stack[Bh, Ih, N0, I0, O0, Z]): Step[I0, O0, Z] =
    val captured = Captured(piece)
    val body = f(captured)
    val r = captured.tail(body)
    if r != null then step(Return(r.x), k0, m0) else step(body, d.out, d.rest)

  /** the piece put back over `m`, the value `x` at the hole: the next state, one object */
  @tailrec private def under[A0, A, I <: Tuple, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      x: A0, piece: Piece[A0, A, I, O], m: Stack[A, I, O, I0, O0, Z]): Step[I0, O0, Z] =
    piece match
      case Over(prev, out) => under(x, prev, Stack.Run(out, m))
      case Crossed(inner, out) => under(x, inner, Stack.Delim(out, m))
      case e @ Frames.End() => step(Return(x), e, m)
      case f @ Frames.Frame(_, _) => step(Return(x), f, m)

  /** the piece put back over `m`: a value due at the hole */
  @tailrec private[cont] def link[A0, A, I <: Tuple, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      piece: Piece[A0, A, I, O], m: Stack[A, I, O, I0, O0, Z]): Resumption[A0, I0, O0, Z] =
    piece match
      case Over(prev, out) => link(prev, Stack.Run(out, m))
      case Crossed(inner, out) => link(inner, Stack.Delim(out, m))
      case e @ Frames.End() => resumption(e, m)
      case f @ Frames.Frame(_, _) => resumption(f, m)

  private def resumption[A0, B0, I1 <: Tuple, T1 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      k0: Frames[A0, B0, I1, T1], m0: Stack[B0, I1, T1, I0, O0, Z]): Resumption[A0, I0, O0, Z] =
    new Resumption[A0, I0, O0, Z]:
      type B = B0
      type I = I1
      type T = T1
      def k = k0
      def m = m0

  private def step[A0, B0, I1 <: Tuple, T1 <: Tuple, O1 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      c0: Cont[T1, O1, A0], k0: Frames[A0, B0, I1, T1], m0: Stack[B0, I1, O1, I0, O0, Z]): Step[I0, O0, Z] =
    new Step[I0, O0, Z]:
      type A = A0
      type B = B0
      type I = I1
      type T = T1
      type O = O1
      def c = c0
      def k = k0
      def m = m0
