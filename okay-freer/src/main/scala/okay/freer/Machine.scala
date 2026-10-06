package okay.freer

import scala.annotation.tailrec
import Freer.*

/** the witness that the row `G` lies inside `F`: a polymorphic identity, made where the compiler knows it */
trait Widen[G[+_], F[+_]]:
  def apply[Σ <: Tuple, S, R, A](p: Freer[G, Σ, S, R, A]): Freer[F, Σ, S, R, A]
  def andThen[E[+_]](next: Widen[F, E]): Widen[G, E] =
    val self = this
    new Widen[G, E]:
      def apply[Σ <: Tuple, S, R, A](p: Freer[G, Σ, S, R, A]): Freer[E, Σ, S, R, A] = next(self(p))
object Widen:
  /** where the compiler knows `H` lies in `G`: the identity, as that knowledge made a value */
  def sub[H[+A] <: G[A], G[+_]]: Widen[H, G] = new Widen[H, G]:
    def apply[Σ <: Tuple, S, R, A](p: Freer[H, Σ, S, R, A]): Freer[G, Σ, S, R, A] = p
  def refl[F[+_]]: Widen[F, F] = sub[F, F]

/** a segment over the row `G` under `Σ`: `A => Freer[G, Σ, S, R, B]` as data; contravariant in what it consumes */
enum Frames[G[+_], -A, B, Σ <: Tuple, S, R]:
  case End[G[+_], A, Σ <: Tuple, S]() extends Frames[G, A, A, Σ, S, S]
  case Frame[G[+_], A, X, B, Σ <: Tuple, S, T, R](f: A => Freer[G, Σ, T, R, X], rest: Frames[G, X, B, Σ, S, T]) extends Frames[G, A, B, Σ, S, R]

/**
 * THE STACK: closes a level over `G` under `Σ` — value `B` at the answer `S`, the program's final answer `R` — into
 * the run's result over `F`, `Freer[F, Σ0, S0, R0, Z]`. A delimiter's level has value = answer (`S`, `S`) and the
 * delimiter on its index; when it returns, the GADT says that answer is the program's final one, `R`, and `out`
 * takes it as a value, at the answer `U` outside, under the stack outside.
 */
enum Stack[F[+_], G[+_], Σ <: Tuple, B, S, R, Σ0 <: Tuple, S0, R0, Z]:
  case Done[F[+_], Σ <: Tuple, B, S, R]() extends Stack[F, F, Σ, B, S, R, Σ, S, R, B]
  case Run[F[+_], G[+_], Σ <: Tuple, B, S, R, B2, S2, Σ0 <: Tuple, S0, R0, Z](
      out: Frames[G, B, B2, Σ, S2, S], rest: Stack[F, G, Σ, B2, S2, R, Σ0, S0, R0, Z])
    extends Stack[F, G, Σ, B, S, R, Σ0, S0, R0, Z]
  case Delim[F[+_], H[+_], G[+_], Σ <: Tuple, S, R, B, S2, U, Σ0 <: Tuple, S0, R0, Z](
      up: Widen[H, G], sub: Widen[G, F], out: Frames[G, R, B, Σ, S2, U], rest: Stack[F, G, Σ, B, S2, U, Σ0, S0, R0, Z])
    extends Stack[F, H, Lvl[H] *: Σ, S, S, R, Σ0, S0, R0, Z]

/** a captured piece: the segments from the hole (`A0` at `T0`) up to the delimiter, it not included */
enum Piece[G[+_], A0, Σ <: Tuple, T0, A, T]:
  case Hole[G[+_], A0, Σ <: Tuple, T0, B, S](k: Frames[G, A0, B, Σ, S, T0]) extends Piece[G, A0, Σ, T0, B, S]
  case Over[G[+_], A0, Σ <: Tuple, T0, X, T2, B, S2](prev: Piece[G, A0, Σ, T0, X, T2], out: Frames[G, X, B, Σ, S2, T2])
    extends Piece[G, A0, Σ, T0, B, S2]

/** a captured continuation, `X => T [U, U]` for every `U`, under the stack `O` outside its delimiter: the piece from
 * the hole, put back under a delimiter of its own, delivers the answer at the hole, at any answer outside. `S` is the
 * delimiter level's own value-and-answer */
final class Captured[H[+_], X, T, S, O <: Tuple](val piece: Piece[H, X, Lvl[H] *: O, T, S, S]):
  def apply[U](x: X): Freer[H, O, U, U, T] = Resume(x, this)
  /** as the `k` a shift0's body receives, at the hole's own type, the answer of the context the body replaces named */
  def kAt[X0 <: X, Out0]: Continue[H, O, X0, T] { type Out = Out0 } = new Continue[H, O, X0, T]:
    type Out = Out0
    def apply[U](x: X0): Freer[H, O, U, U, T] = Captured.this.apply[U](x)
  def under[F[+_], G[+_], B, S2, U, Σ0 <: Tuple, S0, R0, Z](up: Widen[H, G], sub: Widen[G, F], out: Frames[G, T, B, O, S2, U],
                                                           rest: Stack[F, G, O, B, S2, U, Σ0, S0, R0, Z]): Resumption[F, X, T, Σ0, S0, R0, Z] =
    Machine.link(piece, Stack.Delim(up, sub, out, rest), up.andThen(sub))

/** the machine's state with a value `A` due at the answer `T`: the segment `k`, level `G` under `Σ`, over `m`. As
 * a function it is the rest of a run after something handed out: applied by whoever answers it, the run goes on */
sealed abstract class Resumption[F[+_], A, T, Σ0 <: Tuple, S0, R0, Z] extends (A => Freer[F, Σ0, S0, R0, Z]):
  type G[+_]
  type B
  type Σ <: Tuple
  type S
  def k: Frames[G, A, B, Σ, S, T]
  def m: Stack[F, G, Σ, B, S, T, Σ0, S0, R0, Z]
  def sub: Widen[G, F]
  def apply(a: A): Freer[F, Σ0, S0, R0, Z] = Machine.go(Return(a), k, m, sub)

/** the machine's next state with its program: what a capture's walk of the stack answers with — the shift's body
 * at the delimiter's level, or the capture handed out at the run's bottom — its level's types its own */
sealed abstract class Step[F[+_], Σ0 <: Tuple, S0, R0, Z]:
  type G[+_]
  type A
  type B
  type Σ <: Tuple
  type S
  type T
  type R
  def c: Freer[G, Σ, T, R, A]
  def k: Frames[G, A, B, Σ, S, T]
  def m: Stack[F, G, Σ, B, S, R, Σ0, S0, R0, Z]
  def sub: Widen[G, F]

/** one loop, one rule per node, nothing else */
object Machine:
  /** run to a head form: `Return(z)`, `Bind(op, rest)`, or `Bind(shift0, rest)` — a capture whose delimiter is
   * outside this run, for the machine outside. At any stack: a handler is a run inside a delimiter */
  def run[F[+_], Σ <: Tuple, S, R, A](p: Freer[F, Σ, S, R, A]): Freer[F, Σ, S, R, A] = go(p, Frames.End(), Stack.Done(), Widen.refl[F])

  @tailrec private[freer] def go[F[+_], G[+_], A, B, Σ <: Tuple, S, T, R, Σ0 <: Tuple, S0, R0, Z](
      c: Freer[G, Σ, T, R, A], k: Frames[G, A, B, Σ, S, T], m: Stack[F, G, Σ, B, S, R, Σ0, S0, R0, Z], sub: Widen[G, F]): Freer[F, Σ0, S0, R0, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m, sub)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest, sub)
          // the body returned: its answer is the program's final one, and the delimiter delivers it
          case Stack.Delim(_, subOut, out, rest) => go(Return(a), out, rest, subOut)
      case Bind(c0, f) => f match
        case _: Resumption[?, ?, ?, ?, ?, ?, ?] => c0 match
          case Inject(_) | Shift0(_) => k match
            case Frames.End() => m match
              case Stack.Done() => sub(c)
              case _ => go(c0, Frames.Frame(f, k), m, sub)
            case _ => go(c0, Frames.Frame(f, k), m, sub)
          case _ => go(c0, Frames.Frame(f, k), m, sub)
        case _ => go(c0, Frames.Frame(f, k), m, sub)
      case Delay(t) => go(t(), k, m, sub)
      // handed out as a program of the run: an operation carries no answer and no stack, so it is re-injected at the run's
      case Inject(op) => Bind(sub(Inject(op)), resumption(k, m, sub))
      case r: Reset[h, ?, ?, ?, ?] =>
        val up = Widen.sub[h, G]
        go(r.body, Frames.End(), Stack.Delim(up, sub, k, m), up.andThen(sub))
      // the body runs in the delimiter's place, at the level outside, its value the delimiter's answer; a capture
      // whose delimiter is outside this run is a head form, handed out
      case s: Shift0[h, o, ?, ?, x] =>
        val n = cut[F, G, x, T, B, S, R, Σ0, S0, R0, Z, h, o](s, s.f, Piece.Hole(k), m, sub)
        go(n.c, n.k, n.m, n.sub)
      case r: Resume[h, ?, ?, ?, ?] =>
        val n = r.k.under(Widen.sub[h, G], sub, k, m)
        go(Return(r.x), n.k, n.m, n.sub)

  /** down the stack to the nearest delimiter — the index says its row — and the shift's body, given `k`, at the
   * level outside, where that level's answer is a name; the run's bottom instead means the delimiter is outside
   * this run: the capture is handed out whole as a program of the run, the rest of this run after it, re-closed by
   * a `Done` at the hole's answer `T` — built here, where the run's types are names */
  @tailrec private def cut[F[+_], G[+_], X, T, A, Tp, R, Σ0 <: Tuple, S0, R0, Z, H[+_], O <: Tuple](
      c: Freer[G, Lvl[H] *: O, T, R, X], f: (k: Continue[H, O, X, T]) => Freer[H, O, k.Out, k.Out, R],
      piece: Piece[G, X, Lvl[H] *: O, T, A, Tp], m: Stack[F, G, Lvl[H] *: O, A, Tp, R, Σ0, S0, R0, Z], sub: Widen[G, F]): Step[F, Σ0, S0, R0, Z] =
    m match
      case Stack.Run(out, rest) => cut(c, f, Piece.Over(piece, out), rest, sub)
      case d @ Stack.Delim(_, _, _, _) => found(d)(piece, f)
      case Stack.Done() =>
        val n = link(piece, Stack.Done[F, Lvl[H] *: O, A, Tp, T](), sub)
        step(Bind(sub(c), n), Frames.End(), Stack.Done(), Widen.refl[F])

  private def found[F[+_], H[+_], G0[+_], O <: Tuple, S, R, B0, S20, U, Σ0 <: Tuple, S0, R0, Z](
      d: Stack.Delim[F, H, G0, O, S, R, B0, S20, U, Σ0, S0, R0, Z])
      [X, T](piece: Piece[H, X, Lvl[H] *: O, T, S, S], f: (k: Continue[H, O, X, T]) => Freer[H, O, k.Out, k.Out, R]): Step[F, Σ0, S0, R0, Z] =
    step(d.up(f(Captured(piece).kAt[X, U])), d.out, d.rest, d.sub)

  /** the piece put back over `m`: a value due at the hole */
  @tailrec private[freer] def link[F[+_], G[+_], A0, Σ <: Tuple, T0, A, T, Σ0 <: Tuple, S0, R0, Z](
      piece: Piece[G, A0, Σ, T0, A, T], m: Stack[F, G, Σ, A, T, T0, Σ0, S0, R0, Z], sub: Widen[G, F]): Resumption[F, A0, T0, Σ0, S0, R0, Z] =
    piece match
      case Piece.Hole(k0) => resumption(k0, m, sub)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m), sub)

  private def resumption[F[+_], G0[+_], A0, B0, Σ1 <: Tuple, S1, T0, Σ0 <: Tuple, S0, R0, Z](
      k0: Frames[G0, A0, B0, Σ1, S1, T0], m0: Stack[F, G0, Σ1, B0, S1, T0, Σ0, S0, R0, Z], sub0: Widen[G0, F]): Resumption[F, A0, T0, Σ0, S0, R0, Z] =
    new Resumption[F, A0, T0, Σ0, S0, R0, Z]:
      type G[+A1] = G0[A1]
      type B = B0
      type Σ = Σ1
      type S = S1
      def k = k0
      def m = m0
      def sub = sub0

  private def step[F[+_], G0[+_], A0, B0, Σ1 <: Tuple, S1, T1, R1, Σ0 <: Tuple, S0, R0, Z](
      c0: Freer[G0, Σ1, T1, R1, A0], k0: Frames[G0, A0, B0, Σ1, S1, T1], m0: Stack[F, G0, Σ1, B0, S1, R1, Σ0, S0, R0, Z], sub0: Widen[G0, F])
    : Step[F, Σ0, S0, R0, Z] =
    new Step[F, Σ0, S0, R0, Z]:
      type G[+A1] = G0[A1]
      type A = A0
      type B = B0
      type Σ = Σ1
      type S = S1
      type T = T1
      type R = R1
      def c = c0
      def k = k0
      def m = m0
      def sub = sub0
