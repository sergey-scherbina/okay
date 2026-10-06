package okay.freer

import scala.annotation.tailrec
import Freer.*

/** the witness that the row `G` lies inside `F`: a polymorphic identity, made where the compiler knows it */
trait Widen[G[+_], F[+_]]:
  def apply[Σ <: NonEmptyTuple, A](p: Freer[G, Σ, A]): Freer[F, Σ, A]
  def andThen[E[+_]](next: Widen[F, E]): Widen[G, E] =
    val self = this
    new Widen[G, E]:
      def apply[Σ <: NonEmptyTuple, A](p: Freer[G, Σ, A]): Freer[E, Σ, A] = next(self(p))
object Widen:
  /** where the compiler knows `H` lies in `G`: the identity, as that knowledge made a value */
  def sub[H[+A] <: G[A], G[+_]]: Widen[H, G] = new Widen[H, G]:
    def apply[Σ <: NonEmptyTuple, A](p: Freer[H, Σ, A]): Freer[G, Σ, A] = p
  def refl[F[+_]]: Widen[F, F] = sub[F, F]

/** a segment over the row `G`: `A => Freer[G, Σ, B]` as data, its index composed as `Bind` composes; contravariant
 * in what it consumes */
enum Frames[G[+_], -A, B, Σ <: NonEmptyTuple]:
  case End[G[+_], A, H[+_], S, Σ <: Tuple]() extends Frames[G, A, A, Lvl[H, S, S] *: Σ]
  case Frame[G[+_], A, X, B, H[+_], Σ <: Tuple, S, T, R](f: A => Freer[G, Lvl[H, T, R] *: Σ, X], rest: Frames[G, X, B, Lvl[H, S, T] *: Σ])
    extends Frames[G, A, B, Lvl[H, S, R] *: Σ]

/**
 * THE STACK: closes a level over `G` — value `B` at the index `Σ` — into the run's result over `F`, at the run's
 * level `Lvl[H0, S0, R0] *: Σ0`, value `Z`. A delimiter's level has value = answer (`S`, `S`) and its own level on
 * top of the index; when it returns, the GADT says that answer is its final one, `R`, and `out` takes it as a value
 * at the level outside, whose answer `U` the delimiter leaves.
 */
enum Stack[F[+_], G[+_], B, Σ <: NonEmptyTuple, H0[+_], Σ0 <: Tuple, S0, R0, Z]:
  case Done[F[+_], B, H0[+_], Σ0 <: Tuple, S0, R0]() extends Stack[F, F, B, Lvl[H0, S0, R0] *: Σ0, H0, Σ0, S0, R0, B]
  case Run[F[+_], G[+_], B, H[+_], Σ <: Tuple, S, R, B2, S2, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      out: Frames[G, B, B2, Lvl[H, S2, S] *: Σ], rest: Stack[F, G, B2, Lvl[H, S2, R] *: Σ, H0, Σ0, S0, R0, Z])
    extends Stack[F, G, B, Lvl[H, S, R] *: Σ, H0, Σ0, S0, R0, Z]
  case Delim[F[+_], H[+_], G[+_], H2[+_], Σ <: Tuple, S, R, B, S2, U, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      up: Widen[H, G], sub: Widen[G, F], out: Frames[G, R, B, Lvl[H2, S2, U] *: Σ], rest: Stack[F, G, B, Lvl[H2, S2, U] *: Σ, H0, Σ0, S0, R0, Z])
    extends Stack[F, H, S, Lvl[H, S, R] *: Lvl[H2, U, U] *: Σ, H0, Σ0, S0, R0, Z]

/** a captured piece: the segments from the hole up to the delimiter, it not included, composed as one segment */
enum Piece[G[+_], A0, B, Σ <: NonEmptyTuple]:
  case Hole[G[+_], A0, B, Σ <: NonEmptyTuple](k: Frames[G, A0, B, Σ]) extends Piece[G, A0, B, Σ]
  case Over[G[+_], A0, X, B, H[+_], Σ <: Tuple, S2, T2, T](prev: Piece[G, A0, X, Lvl[H, T2, T] *: Σ], out: Frames[G, X, B, Lvl[H, S2, T2] *: Σ])
    extends Piece[G, A0, B, Lvl[H, S2, T] *: Σ]

/** a captured continuation: the piece from the hole, `X` at the answer `T`, to its delimiter's level, value-and-answer
 * `S`; put back under a delimiter of its own, it delivers the answer at the hole, at the level outside, which it
 * leaves at its answer `U` */
final class Captured[H[+_], X, T, S, H2[+_], Σ <: Tuple, U](val piece: Piece[H, X, S, Lvl[H, S, T] *: Lvl[H2, U, U] *: Σ])
  extends (X => Freer[H, Lvl[H2, U, U] *: Σ, T]):
  def apply(x: X): Freer[H, Lvl[H2, U, U] *: Σ, T] = Resume(x, this)
  def under[F[+_], G[+_], B, S2, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      up: Widen[H, G], sub: Widen[G, F], out: Frames[G, T, B, Lvl[H2, S2, U] *: Σ], rest: Stack[F, G, B, Lvl[H2, S2, U] *: Σ, H0, Σ0, S0, R0, Z])
    : Resumption[F, X, H0, Σ0, S0, R0, Z] =
    Machine.link(piece, Stack.Delim(up, sub, out, rest), up.andThen(sub))

/** the machine's state with a value `A` due: the segment `k`, level `G`, over `m`. As a function it is the rest of
 * a run after something handed out: applied by whoever answers it, the run goes on */
sealed abstract class Resumption[F[+_], A, H0[+_], Σ0 <: Tuple, S0, R0, Z] extends (A => Freer[F, Lvl[H0, S0, R0] *: Σ0, Z]):
  type G[+_]
  type B
  type H[+_]
  type Σ <: Tuple
  type S
  type T
  def k: Frames[G, A, B, Lvl[H, S, T] *: Σ]
  def m: Stack[F, G, B, Lvl[H, S, T] *: Σ, H0, Σ0, S0, R0, Z]
  def sub: Widen[G, F]
  def apply(a: A): Freer[F, Lvl[H0, S0, R0] *: Σ0, Z] = Machine.go(Return(a), k, m, sub)

/** the machine's next state with its program: what a capture's walk of the stack answers with — the shift's body
 * at the delimiter's level, or the capture handed out at the run's bottom — its level's types its own */
sealed abstract class Step[F[+_], H0[+_], Σ0 <: Tuple, S0, R0, Z]:
  type G[+_]
  type A
  type B
  type H[+_]
  type Σ <: Tuple
  type S
  type T
  type R
  def c: Freer[G, Lvl[H, T, R] *: Σ, A]
  def k: Frames[G, A, B, Lvl[H, S, T] *: Σ]
  def m: Stack[F, G, B, Lvl[H, S, R] *: Σ, H0, Σ0, S0, R0, Z]
  def sub: Widen[G, F]

/** one loop, one rule per node, nothing else */
object Machine:
  /** run to a head form: `Return(z)`, `Bind(op, rest)`, or `Bind(shift0, rest)` — a capture whose delimiter is
   * outside this run, for the machine outside. At any level: a handler is a run inside a delimiter */
  def run[F[+_], H0[+_], Σ0 <: Tuple, S0, R0, A](p: Freer[F, Lvl[H0, S0, R0] *: Σ0, A]): Freer[F, Lvl[H0, S0, R0] *: Σ0, A] =
    go(p, Frames.End(), Stack.Done(), Widen.refl[F])

  @tailrec private[freer] def go[F[+_], G[+_], A, B, H[+_], Σ <: Tuple, S, T, R, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      c: Freer[G, Lvl[H, T, R] *: Σ, A], k: Frames[G, A, B, Lvl[H, S, T] *: Σ], m: Stack[F, G, B, Lvl[H, S, R] *: Σ, H0, Σ0, S0, R0, Z],
      sub: Widen[G, F]): Freer[F, Lvl[H0, S0, R0] *: Σ0, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m, sub)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest, sub)
          // the body returned: its answer is its final one, and the delimiter delivers it
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
      // handed out as a program of the run: an operation carries no answer and no level, so it is re-injected at the run's
      case Inject(op) => Bind(sub(Inject(op)), resumption(k, m, sub))
      case r: Reset[h, h2, σ, s, rr, u] =>
        val up = Widen.sub[h, G]
        go(r.body, Frames.End[h, s, h, s, Lvl[h2, u, u] *: σ](), Stack.Delim[F, h, G, h2, σ, s, rr, B, S, u, H0, Σ0, S0, R0, Z](up, sub, k, m), up.andThen(sub))
      // the body runs in the delimiter's place, at the level outside, its value the delimiter's answer; a capture
      // whose delimiter is outside this run is a head form, handed out
      case s: Shift0[h, h2, σ, ?, ?, x, u] =>
        val n = cut[F, G, x, T, B, S, R, H0, Σ0, S0, R0, Z, h, h2, σ, u](s, s.f, Piece.Hole(k), m, sub)
        go(n.c, n.k, n.m, n.sub)
      case r: Resume[h, x, t, h2, σ, u] =>
        val n = (r.k: Captured[h, x, t, ?, h2, σ, u]).under[F, G, B, S, H0, Σ0, S0, R0, Z](Widen.sub[h, G], sub, k, m)
        go(Return(r.x), n.k, n.m, n.sub)

  /** down the stack to the nearest delimiter — the index says its row — and the shift's body, given `k`, at the
   * level outside; the run's bottom instead means the delimiter is outside this run: the capture is handed out
   * whole as a program of the run, the rest of this run after it, re-closed by a `Done` at the hole's answer `T` —
   * built here, where the run's types are names */
  @tailrec private def cut[F[+_], G[+_], X, T, A, Tp, R, H0[+_], Σ0 <: Tuple, S0, R0, Z, H[+_], H2[+_], O <: Tuple, U](
      c: Freer[G, Lvl[H, T, R] *: Lvl[H2, U, U] *: O, X], f: (X => Freer[H, Lvl[H2, U, U] *: O, T]) => Freer[H, Lvl[H2, U, U] *: O, R],
      piece: Piece[G, X, A, Lvl[H, Tp, T] *: Lvl[H2, U, U] *: O], m: Stack[F, G, A, Lvl[H, Tp, R] *: Lvl[H2, U, U] *: O, H0, Σ0, S0, R0, Z],
      sub: Widen[G, F]): Step[F, H0, Σ0, S0, R0, Z] =
    m match
      case Stack.Run(out, rest) => cut(c, f, Piece.Over(piece, out), rest, sub)
      case d @ Stack.Delim(_, _, _, _) => found(d)(piece, f)
      case Stack.Done() =>
        val n = link(piece, Stack.Done[F, A, H, Lvl[H2, U, U] *: O, Tp, T](), sub)
        step(Bind(sub(c), n), Frames.End(), Stack.Done(), Widen.refl[F])

  private def found[F[+_], H[+_], G0[+_], H2[+_], O <: Tuple, S, R, B0, S20, U, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      d: Stack.Delim[F, H, G0, H2, O, S, R, B0, S20, U, H0, Σ0, S0, R0, Z])
      [X, T](piece: Piece[H, X, S, Lvl[H, S, T] *: Lvl[H2, U, U] *: O], f: (X => Freer[H, Lvl[H2, U, U] *: O, T]) => Freer[H, Lvl[H2, U, U] *: O, R])
    : Step[F, H0, Σ0, S0, R0, Z] =
    step(d.up(f(Captured(piece))), d.out, d.rest, d.sub)

  /** the piece put back over `m`: a value due at the hole */
  @tailrec private[freer] def link[F[+_], G[+_], A0, A, H[+_], Σ <: Tuple, Tp, T0, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      piece: Piece[G, A0, A, Lvl[H, Tp, T0] *: Σ], m: Stack[F, G, A, Lvl[H, Tp, T0] *: Σ, H0, Σ0, S0, R0, Z], sub: Widen[G, F])
    : Resumption[F, A0, H0, Σ0, S0, R0, Z] =
    piece match
      case Piece.Hole(k0) => resumption(k0, m, sub)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m), sub)

  private def resumption[F[+_], G0[+_], A0, B0, H1[+_], Σ1 <: Tuple, S1, T1, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      k0: Frames[G0, A0, B0, Lvl[H1, S1, T1] *: Σ1], m0: Stack[F, G0, B0, Lvl[H1, S1, T1] *: Σ1, H0, Σ0, S0, R0, Z], sub0: Widen[G0, F])
    : Resumption[F, A0, H0, Σ0, S0, R0, Z] =
    new Resumption[F, A0, H0, Σ0, S0, R0, Z]:
      type G[+A1] = G0[A1]
      type B = B0
      type H[+A1] = H1[A1]
      type Σ = Σ1
      type S = S1
      type T = T1
      def k = k0
      def m = m0
      def sub = sub0

  private def step[F[+_], G0[+_], A0, B0, H1[+_], Σ1 <: Tuple, S1, T1, R1, H0[+_], Σ0 <: Tuple, S0, R0, Z](
      c0: Freer[G0, Lvl[H1, T1, R1] *: Σ1, A0], k0: Frames[G0, A0, B0, Lvl[H1, S1, T1] *: Σ1], m0: Stack[F, G0, B0, Lvl[H1, S1, R1] *: Σ1, H0, Σ0, S0, R0, Z],
      sub0: Widen[G0, F]): Step[F, H0, Σ0, S0, R0, Z] =
    new Step[F, H0, Σ0, S0, R0, Z]:
      type G[+A1] = G0[A1]
      type A = A0
      type B = B0
      type H[+A1] = H1[A1]
      type Σ = Σ1
      type S = S1
      type T = T1
      type R = R1
      def c = c0
      def k = k0
      def m = m0
      def sub = sub0
