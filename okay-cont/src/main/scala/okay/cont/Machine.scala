package okay.cont

import scala.annotation.tailrec
import Cont.*

/** the witness that the row `G` lies inside `F`: a polymorphic identity, made where the compiler knows it */
trait Widen[G[+_], F[+_]]:
  def apply[I <: Tuple, O <: Tuple, A](p: Cont[G, I, O, A]): Cont[F, I, O, A]
object Widen:
  /** where the compiler knows `H` lies in `G`: the identity, as that knowledge made a value */
  def sub[H[+A] <: G[A], G[+_]]: Widen[H, G] = new Widen[H, G]:
    def apply[I <: Tuple, O <: Tuple, A](p: Cont[H, I, O, A]): Cont[G, I, O, A] = p

/** a captured piece: the segments from the hole up to the delimiter, it not included, composed as one segment — a
 * segment is one (`Frames`), and `Over` is one over the next */
sealed trait Piece[G[+_], -A0, B, I <: Tuple, O <: Tuple]
/** a segment over the row `G`: `A => Cont[G, I, O, B]` as data, composed as `Bind` composes; contravariant in what
 * it consumes */
enum Frames[G[+_], -A, B, I <: Tuple, O <: Tuple] extends Piece[G, A, B, I, O]:
  case End[G[+_], A, Σ <: Tuple]() extends Frames[G, A, A, Σ, Σ]
  case Frame[G[+_], A, X, B, I <: Tuple, T <: Tuple, O <: Tuple](f: A => Cont[G, T, O, X], rest: Frames[G, X, B, I, T]) extends Frames[G, A, B, I, O]
/** a piece over the next segment: the hole's segments end in `X` at `T`, the segment takes `X` to `B` */
final case class Over[G[+_], A0, X, B, I <: Tuple, T <: Tuple, O <: Tuple](prev: Piece[G, A0, X, T, O], out: Frames[G, X, B, I, T])
  extends Piece[G, A0, B, I, O]

/**
 * THE STACK: closes a level over `G` — value `B`, from `I` to `O` — into the run's result over `F`, from `I0` to
 * `O0`, value `Z`. A delimiter's level has value = answer (`S`, `S`) and its own level on top, its `D` the stacks
 * outside; when it returns, the GADT says that answer is its final one, `R`, and `out` takes it as a value outside.
 */
enum Stack[F[+_], G[+_], B, I <: Tuple, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z]:
  case Done[F[+_], B, I <: Tuple, O <: Tuple]() extends Stack[F, F, B, I, O, I, O, B]
  case Run[F[+_], G[+_], B, I <: Tuple, O <: Tuple, B2, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      out: Frames[G, B, B2, I2, I], rest: Stack[F, G, B2, I2, O, I0, O0, Z])
    extends Stack[F, G, B, I, O, I0, O0, Z]
  case Delim[F[+_], H[+_], Hf[+_], G[+_], D <: Tuple, O <: Tuple, S, R, B, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      up: Widen[Hf, G], out: Frames[G, R, B, I2, D], rest: Stack[F, G, B, I2, O, I0, O0, Z])
    extends Stack[F, H, S, At[H, Hf, D, S] *: D, At[H, Hf, D, R] *: O, I0, O0, Z]

/** a captured continuation: the piece from the hole, `X` at the answer `T` with the stacks `I` outside, to its
 * delimiter's level, value-and-answer `S`, the stacks `D` outside; put back under a delimiter of its own, it
 * delivers the answer at the hole, outside, at the row the delimiter leaves `Hf`, from `D` to `I` */
final class Captured[H[+_], Hf[+_], D <: Tuple, I <: Tuple, X, T, S](val piece: Piece[H, X, S, At[H, Hf, D, S] *: D, At[H, Hf, D, T] *: I])
  extends (X => Cont[Hf, D, I, T]):
  def apply(x: X): Cont[Hf, D, I, T] = Resume(x, this)

/** the machine's state with a value `A` due: the segment `k`, level `G`, over `m`. As a function it is the rest of
 * a run after something handed out: applied by whoever answers it, the run goes on */
sealed abstract class Resumption[F[+_], A, I0 <: Tuple, O0 <: Tuple, Z] extends (A => Cont[F, I0, O0, Z]):
  type G[+_]
  type B
  type I <: Tuple
  type T <: Tuple
  def k: Frames[G, A, B, I, T]
  def m: Stack[F, G, B, I, T, I0, O0, Z]
  def apply(a: A): Cont[F, I0, O0, Z] = Machine.go(Return(a), k, m)

/** the machine's next state with its program: what a capture's walk of the stack answers with — the shift's body
 * at the delimiter's level, or the capture handed out at the run's bottom — its level's types its own */
sealed abstract class Step[F[+_], I0 <: Tuple, O0 <: Tuple, Z]:
  type G[+_]
  type A
  type B
  type I <: Tuple
  type T <: Tuple
  type O <: Tuple
  def c: Cont[G, T, O, A]
  def k: Frames[G, A, B, I, T]
  def m: Stack[F, G, B, I, O, I0, O0, Z]

/** one loop, one rule per node, nothing else */
object Machine:
  /** run to a head form: `Return(z)`, or `Bind(shift0, rest)` — a capture whose delimiter is outside this run, for
   * the machine outside. At any level: a handler's clause may run a program of its own */
  def run[F[+_], I <: Tuple, O <: Tuple, A](p: Cont[F, I, O, A]): Cont[F, I, O, A] = go(p, Frames.End(), Stack.Done())

  @tailrec private[cont] def go[F[+_], G[+_], A, B, I <: Tuple, T <: Tuple, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      c: Cont[G, T, O, A], k: Frames[G, A, B, I, T], m: Stack[F, G, B, I, O, I0, O0, Z]): Cont[F, I0, O0, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest)
          // the body returned: its answer is its final one, and the delimiter delivers it
          case Stack.Delim(_, out, rest) => go(Return(a), out, rest)
      // a head form handed out (its rest is a `Resumption`), met at a run's bottom: as it is, a run over a run allocates nothing
      case Bind(c0, f) => f match
        case _: Resumption[?, ?, ?, ?, ?] => k match
          case Frames.End() => m match
            case Stack.Done() => c
            case _ => go(c0, Frames.Frame(f, k), m)
          case _ => go(c0, Frames.Frame(f, k), m)
        case _ => go(c0, Frames.Frame(f, k), m)
      case Delay(t) => go(t(), k, m)
      // the three that move a level answer with the machine's next state, each where its node's types are names
      case r: Reset[?, hf, ?, ?, ?, ?] => val n = enter(r, Widen.sub[hf, G], k, m); go(n.c, n.k, n.m)
      case s: Shift0[h, hf, d, i, o, t, r, x] => val n = cut[F, G, x, t, B, I, r, o, I0, O0, Z, h, hf, d, i](s, s.f, k, m); go(n.c, n.k, n.m)
      case r: Resume[hf, ?, ?, ?, ?] => val n = resume(r, Widen.sub[hf, G], k, m); go(n.c, n.k, n.m)

  /** into the delimiter: its level pushed, the body at its start */
  private def enter[F[+_], H[+_], Hf[+_], G[+_], D <: Tuple, O <: Tuple, S, R, B, I <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      r: Reset[H, Hf, D, O, S, R], up: Widen[Hf, G], k: Frames[G, R, B, I, D], m: Stack[F, G, B, I, O, I0, O0, Z]): Step[F, I0, O0, Z] =
    step(r.body, Frames.End(), Stack.Delim(up, k, m))

  /** `k(x)`: the piece put back under a delimiter of its own, the value at the hole */
  private def resume[F[+_], Hf[+_], G[+_], D <: Tuple, I <: Tuple, X, T, B, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      r: Resume[Hf, D, I, X, T], up: Widen[Hf, G], k: Frames[G, T, B, I2, D], m: Stack[F, G, B, I2, I, I0, O0, Z]): Step[F, I0, O0, Z] =
    val n = link(r.k.piece, Stack.Delim(up, k, m))
    step(Return(r.x), n.k, n.m)

  /** down the stack to the nearest delimiter — the index says its rows, and its `D` is the node's — and the
   * shift's body, given `k`, outside; the run's bottom instead means the delimiter is outside this run: the
   * capture is handed out whole as a program of the run, the rest of this run after it, re-closed by a `Done` at
   * the hole — built here, where the run's types are names */
  @tailrec private def cut[F[+_], G[+_], X, T, A, I <: Tuple, R, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z, H[+_], Hf[+_], D <: Tuple, Ik <: Tuple](
      c: Cont[G, At[H, Hf, D, T] *: Ik, At[H, Hf, D, R] *: O, X], f: (X => Cont[Hf, D, Ik, T]) => Cont[Hf, D, O, R],
      piece: Piece[G, X, A, I, At[H, Hf, D, T] *: Ik], m: Stack[F, G, A, I, At[H, Hf, D, R] *: O, I0, O0, Z]): Step[F, I0, O0, Z] =
    m match
      case Stack.Run(out, rest) => cut(c, f, Over(piece, out), rest)
      case d @ Stack.Delim(_, _, _) => found(d)[X, T, Ik](piece, f)
      case Stack.Done() =>
        val n = link(piece, Stack.Done[F, A, I, At[H, Hf, D, T] *: Ik]())
        step(Bind(c, n), Frames.End(), Stack.Done())

  private def found[F[+_], H[+_], Hf[+_], G0[+_], D <: Tuple, O <: Tuple, S, R, B0, I2 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      d: Stack.Delim[F, H, Hf, G0, D, O, S, R, B0, I2, I0, O0, Z])
      [X, T, Ik <: Tuple](piece: Piece[H, X, S, At[H, Hf, D, S] *: D, At[H, Hf, D, T] *: Ik], f: (X => Cont[Hf, D, Ik, T]) => Cont[Hf, D, O, R]): Step[F, I0, O0, Z] =
    step(d.up(f(Captured(piece))), d.out, d.rest)

  /** the piece put back over `m`: a value due at the hole */
  @tailrec private[cont] def link[F[+_], G[+_], A0, A, I <: Tuple, O <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      piece: Piece[G, A0, A, I, O], m: Stack[F, G, A, I, O, I0, O0, Z]): Resumption[F, A0, I0, O0, Z] =
    piece match
      case Over(prev, out) => link(prev, Stack.Run(out, m))
      case e @ Frames.End() => resumption(e, m)
      case f @ Frames.Frame(_, _) => resumption(f, m)

  private def resumption[F[+_], G0[+_], A0, B0, I1 <: Tuple, T1 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      k0: Frames[G0, A0, B0, I1, T1], m0: Stack[F, G0, B0, I1, T1, I0, O0, Z]): Resumption[F, A0, I0, O0, Z] =
    new Resumption[F, A0, I0, O0, Z]:
      type G[+A1] = G0[A1]
      type B = B0
      type I = I1
      type T = T1
      def k = k0
      def m = m0

  private def step[F[+_], G0[+_], A0, B0, I1 <: Tuple, T1 <: Tuple, O1 <: Tuple, I0 <: Tuple, O0 <: Tuple, Z](
      c0: Cont[G0, T1, O1, A0], k0: Frames[G0, A0, B0, I1, T1], m0: Stack[F, G0, B0, I1, O1, I0, O0, Z]): Step[F, I0, O0, Z] =
    new Step[F, I0, O0, Z]:
      type G[+A1] = G0[A1]
      type A = A0
      type B = B0
      type I = I1
      type T = T1
      type O = O1
      def c = c0
      def k = k0
      def m = m0
