package okay.k7

import scala.annotation.tailrec
import Freer.*

trait Widen[G[_, _, +_], F[_, _, +_]]:
  def apply[Σ <: Tuple, S, R, A](p: Freer[G, Σ, S, R, A]): Freer[F, Σ, S, R, A]
  def andThen[E[_, _, +_]](next: Widen[F, E]): Widen[G, E] =
    val self = this
    new Widen[G, E]:
      def apply[Σ <: Tuple, S, R, A](p: Freer[G, Σ, S, R, A]): Freer[E, Σ, S, R, A] = next(self(p))
object Widen:
  def refl[F[_, _, +_]]: Widen[F, F] = new Widen[F, F]:
    def apply[Σ <: Tuple, S, R, A](p: Freer[F, Σ, S, R, A]): Freer[F, Σ, S, R, A] = p

enum Frames[G[_, _, +_], -A, B, Σ <: Tuple, S, R]:
  case End[G[_, _, +_], A, Σ <: Tuple, S]() extends Frames[G, A, A, Σ, S, S]
  case Frame[G[_, _, +_], A, X, B, Σ <: Tuple, S, T, R](f: A => Freer[G, Σ, T, R, X], rest: Frames[G, X, B, Σ, S, T]) extends Frames[G, A, B, Σ, S, R]

/** closes a level over `G` under `Σ`, ending in `B` at `S`, into the run's result over `F` at the empty stack */
enum Stack[F[_, _, +_], G[_, _, +_], B, Σ <: Tuple, S, S0, Z]:
  case Done[F[_, _, +_], B, S]() extends Stack[F, F, B, EmptyTuple, S, S, B]
  case Run[F[_, _, +_], G[_, _, +_], B, Σ <: Tuple, S, B2, S2, S0, Z](out: Frames[G, B, B2, Σ, S2, S], rest: Stack[F, G, B2, Σ, S2, S0, Z])
    extends Stack[F, G, B, Σ, S, S0, Z]
  /** a delimiter: above it the body on `Entry[H, S, Y] *: Σ`, under it `Σ` */
  case Delim[F[_, _, +_], H[_, _, +_], G[_, _, +_], Y, Σ <: Tuple, S, B, S2, S0, Z](
      up: Widen[H, G], sub: Widen[G, F], out: Frames[G, Y, B, Σ, S2, S], rest: Stack[F, G, B, Σ, S2, S0, Z])
    extends Stack[F, H, Y, Entry[H, S, Y] *: Σ, S, S0, Z]

/** a captured piece: the segments from the hole up to the nearest delimiter, no delimiter crossed */
enum Piece[G[_, _, +_], A0, Σ <: Tuple, T0, A, T]:
  case Hole[G[_, _, +_], A0, Σ <: Tuple, T0, B, S](k: Frames[G, A0, B, Σ, S, T0]) extends Piece[G, A0, Σ, T0, B, S]
  case Over[G[_, _, +_], A0, Σ <: Tuple, T0, X, T2, B, S2](prev: Piece[G, A0, Σ, T0, X, T2], out: Frames[G, X, B, Σ, S2, T2])
    extends Piece[G, A0, Σ, T0, B, S2]

/** a captured continuation: the piece and the delimiter, on the stack `O` outside it, from the hole's `T` to the
 * delimiter's `S` */
final class Captured[H[_, _, +_], X, Σ0 <: Tuple, T, O <: Tuple, S, Y](val piece: Piece[H, X, Entry[H, S, Y] *: O, T, Y, S])
  extends (X => Freer[H, O, S, T, Y]):
  def apply(x: X): Freer[H, O, S, T, Y] = Resume(x, this)
  def under[F[_, _, +_], G[_, _, +_], B, S2, S0, Z](up: Widen[H, G], sub: Widen[G, F], out: Frames[G, Y, B, O, S2, S], rest: Stack[F, G, B, O, S2, S0, Z]): Next[F, X, T, S0, Z] =
    Machine.link(piece, Stack.Delim[F, H, G, Y, O, S, B, S2, S0, Z](up, sub, out, rest), up.andThen(sub))

final class Resumption[F[_, _, +_], G[_, _, +_], A, B, Σ <: Tuple, S, T, S0, Z](k: Frames[G, A, B, Σ, S, T], m: Stack[F, G, B, Σ, S, S0, Z], sub: Widen[G, F])
  extends (A => Freer[F, EmptyTuple, S0, T, Z]):
  def apply(a: A): Freer[F, EmptyTuple, S0, T, Z] = Machine.go(Return(a), k, m, sub)

sealed abstract class Next[F[_, _, +_], A, T, S0, Z]:
  type G[_, _, +_]
  type B
  type Σ <: Tuple
  type S
  def k: Frames[G, A, B, Σ, S, T]
  def m: Stack[F, G, B, Σ, S, S0, Z]
  def sub: Widen[G, F]

sealed abstract class Cut[F[_, _, +_], X, H[_, _, +_], O <: Tuple, Sp, T0, Y, S0, Z] extends Next[F, Y, Sp, S0, Z]:
  type Σ = O
  def captured: X => Freer[H, O, Sp, T0, Y]
  def up: Widen[H, G]

object Machine:
  def run[F[_, _, +_], S, R, A](p: Freer[F, EmptyTuple, S, R, A]): Freer[F, EmptyTuple, S, R, A] =
    go(p, Frames.End(), Stack.Done(), Widen.refl[F])

  @tailrec private[k7] def go[F[_, _, +_], G[_, _, +_], A, B, Σ <: Tuple, S, T, R, S0, Z](
      c: Freer[G, Σ, T, R, A], k: Frames[G, A, B, Σ, S, T], m: Stack[F, G, B, Σ, S, S0, Z], sub: Widen[G, F]): Freer[F, EmptyTuple, S0, R, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m, sub)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest, sub)
          case Stack.Delim(up, subOut, out, rest) => go(up(Return(a)), out, rest, subOut)
      case Bind(c0, f) => f match
        case _: Resumption[?, ?, ?, ?, ?, ?, ?, ?, ?] => c0 match
          case Perform(_) => k match
            case Frames.End() => m match
              case Stack.Done() => sub(c)
              case _ => go(c0, Frames.Frame(f, k), m, sub)
            case _ => go(c0, Frames.Frame(f, k), m, sub)
          case _ => go(c0, Frames.Frame(f, k), m, sub)
        case _ => go(c0, Frames.Frame(f, k), m, sub)
      case Delay(t) => go(t(), k, m, sub)
      case Perform(op) => Bind(sub(Perform(op)), Resumption(k, m, sub))
      case r: Reset[h, ?, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[Σ2 <: Tuple, S2, R2, A2](p: Freer[h, Σ2, S2, R2, A2]): Freer[G, Σ2, S2, R2, A2] = p
        go(r.body, Frames.End(), Stack.Delim(up, sub, k, m), up.andThen(sub))
      case s: Shift0[h, o, sp, ?, ?, ?, y] =>
        val n = cut[F, G, A, T, B, S, S0, Z, h, sp, y, o](Piece.Hole(k), m, sub)
        go(n.up(s.f(n.captured)), n.k, n.m, n.sub)
      case r: Resume[h, ?, ?, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[Σ2 <: Tuple, S2, R2, A2](p: Freer[h, Σ2, S2, R2, A2]): Freer[G, Σ2, S2, R2, A2] = p
        val n = r.k.under(up, sub, k, m)
        go(Return(r.x), n.k, n.m, n.sub)

  /** down the stack to the nearest delimiter — the stack's type says there is one: the empty stack is no case */
  @tailrec private def cut[F[_, _, +_], G[_, _, +_], X, T0, A, Tp, S0, Z, H[_, _, +_], Sp, Y, O <: Tuple](
      piece: Piece[G, X, Entry[H, Sp, Y] *: O, T0, A, Tp], m: Stack[F, G, A, Entry[H, Sp, Y] *: O, Tp, S0, Z], sub: Widen[G, F])
    : Cut[F, X, H, O, Sp, T0, Y, S0, Z] =
    m match
      case Stack.Run(out, rest) => cut(Piece.Over(piece, out), rest, sub)
      case d @ Stack.Delim(_, _, _, _) => found(d)(piece)

  private def found[F[_, _, +_], H[_, _, +_], G0[_, _, +_], Y, O <: Tuple, Sp, B0, S20, S0, Z](d: Stack.Delim[F, H, G0, Y, O, Sp, B0, S20, S0, Z])
                   [X, T0](piece: Piece[H, X, Entry[H, Sp, Y] *: O, T0, Y, Sp]): Cut[F, X, H, O, Sp, T0, Y, S0, Z] =
    new Cut[F, X, H, O, Sp, T0, Y, S0, Z]:
      type G[S1, R1, +A1] = G0[S1, R1, A1]
      type B = B0
      type S = S20
      def captured = Captured[H, X, Entry[H, Sp, Y] *: O, T0, O, Sp, Y](piece)
      def up = d.up
      def k = d.out
      def m = d.rest
      def sub = d.sub

  @tailrec private[k7] def link[F[_, _, +_], G[_, _, +_], A0, Σ <: Tuple, T0, A, T, S0, Z](
      piece: Piece[G, A0, Σ, T0, A, T], m: Stack[F, G, A, Σ, T, S0, Z], sub: Widen[G, F]): Next[F, A0, T0, S0, Z] =
    piece match
      case Piece.Hole(k0) => linked(k0, m, sub)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m), sub)

  private def linked[F[_, _, +_], G0[_, _, +_], A0, B0, Σ0 <: Tuple, S0, T0, S00, Z](k0: Frames[G0, A0, B0, Σ0, S0, T0], m0: Stack[F, G0, B0, Σ0, S0, S00, Z], sub0: Widen[G0, F]): Next[F, A0, T0, S00, Z] =
    new Next[F, A0, T0, S00, Z]:
      type G[S1, R1, +A1] = G0[S1, R1, A1]
      type B = B0
      type Σ = Σ0
      type S = S0
      def k = k0
      def m = m0
      def sub = sub0
