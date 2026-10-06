package okay.ts

import scala.annotation.tailrec
import Freer.*

trait Widen[G[+_], F[+_]]:
  def apply[S <: Tuple, A](p: Freer[G, S, A]): Freer[F, S, A]
  def andThen[E[+_]](next: Widen[F, E]): Widen[G, E] =
    val self = this
    new Widen[G, E]:
      def apply[S <: Tuple, A](p: Freer[G, S, A]): Freer[E, S, A] = next(self(p))
object Widen:
  def refl[F[+_]]: Widen[F, F] = new Widen[F, F]:
    def apply[S <: Tuple, A](p: Freer[F, S, A]): Freer[F, S, A] = p

/** a segment on the stack `S`: `A => Freer[G, S, B]` as data */
enum Frames[G[+_], -A, B, S <: Tuple]:
  case End[G[+_], A, S <: Tuple]() extends Frames[G, A, A, S]
  case Frame[G[+_], A, X, B, S <: Tuple](f: A => Freer[G, S, X], rest: Frames[G, X, B, S]) extends Frames[G, A, B, S]

/** closes a level over `G`, ending in `B` on the stack `S`, into the run's result over `F`. `Done` is the EMPTY
 * stack: a run starts and ends with no delimiter in force, so a head form is a program of the outside */
enum Stack[F[+_], G[+_], B, S <: Tuple, Z]:
  case Done[F[+_], B]() extends Stack[F, F, B, EmptyTuple, B]
  case Run[F[+_], G[+_], B, S <: Tuple, B2, Z](out: Frames[G, B, B2, S], rest: Stack[F, G, B2, S, Z]) extends Stack[F, G, B, S, Z]
  /** a delimiter: above it the body on `At[P, Freer[H, EmptyTuple, Y]] *: S`, under it `S`. No prompt VALUE: the type is the identity */
  case Delim[F[+_], H[+_], G[+_], Y, S <: Tuple, P, B, Z](
      up: Widen[H, G], sub: Widen[G, F], out: Frames[G, Y, B, S], rest: Stack[F, G, B, S, Z])
    extends Stack[F, H, Y, At[P, Freer[H, EmptyTuple, Y]] *: S, Z]

/** a captured piece, from the hole (`A0` on `T0`) outward to `A` on `T` at the level `G` */
enum Piece[G[+_], A0, T0 <: Tuple, A, T <: Tuple]:
  case Hole[G[+_], A0, T0 <: Tuple, B](k: Frames[G, A0, B, T0]) extends Piece[G, A0, T0, B, T0]
  case Over[G[+_], A0, T0 <: Tuple, X, T <: Tuple, B](prev: Piece[G, A0, T0, X, T], out: Frames[G, X, B, T]) extends Piece[G, A0, T0, B, T]
  case Under[H[+_], G[+_], A0, T0 <: Tuple, Y, S <: Tuple, P, B](
      up: Widen[H, G], out: Frames[G, Y, B, S], prev: Piece[H, A0, T0, Y, At[P, Freer[H, EmptyTuple, Y]] *: S]) extends Piece[G, A0, T0, B, S]

/** a captured continuation, from the hole up to and including the delimiter of `p`: a program on the stack `O`
 * OUTSIDE its prompt — it carries the delimiter, so it runs wherever `O` is in force */
final class Captured[H[+_], X, T0 <: Tuple, O <: Tuple, Y, P](val piece: Piece[H, X, T0, Y, At[P, Freer[H, EmptyTuple, Y]] *: O])
  extends (X => Freer[H, O, Y]):
  def apply(x: X): Freer[H, O, Y] = Resume(x, this)
  def under[F[+_], G[+_], B, Z](up: Widen[H, G], sub: Widen[G, F], out: Frames[G, Y, B, O], rest: Stack[F, G, B, O, Z]): Next[F, X, Z] =
    Machine.link(piece, Stack.Delim[F, H, G, Y, O, P, B, Z](up, sub, out, rest), up.andThen(sub))

final class Resumption[F[+_], G[+_], A, B, S <: Tuple, Z](k: Frames[G, A, B, S], m: Stack[F, G, B, S, Z], sub: Widen[G, F])
  extends (A => Freer[F, EmptyTuple, Z]):
  def apply(a: A): Freer[F, EmptyTuple, Z] = Machine.go(Return(a), k, m, sub)

/** the machine's next state: a value `A` is due at the segment `k`, on the stack `T` at the level `G`, over `m` */
sealed abstract class Next[F[+_], A, Z]:
  type G[+_]
  type B
  type T <: Tuple
  def k: Frames[G, A, B, T]
  def m: Stack[F, G, B, T, Z]
  def sub: Widen[G, F]

/** a capture: the continuation, and the state under its delimiter — `Y` due on `O` */
sealed abstract class Cut[F[+_], X, H[+_], O <: Tuple, Y, Z] extends Next[F, Y, Z]:
  type T = O
  def captured: X => Freer[H, O, Y]
  def up: Widen[H, G]

object Machine:
  /** a program of the outside — no delimiter in force — run to a head form of the outside */
  def run[F[+_], A](p: Freer[F, EmptyTuple, A]): Freer[F, EmptyTuple, A] = go(p, Frames.End(), Stack.Done(), Widen.refl[F])

  @tailrec private[ts] def go[F[+_], G[+_], A, B, S <: Tuple, Z](
      c: Freer[G, S, A], k: Frames[G, A, B, S], m: Stack[F, G, B, S, Z], sub: Widen[G, F]): Freer[F, EmptyTuple, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m, sub)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest, sub)
          case Stack.Delim(up, subOut, out, rest) => go(up(Return(a)), out, rest, subOut)
      case Bind(c0, f) => f match
        case _: Resumption[?, ?, ?, ?, ?, ?] => c0 match
          case Inject(_) => k match
            case Frames.End() => m match
              case Stack.Done() => sub(c)
              case _ => go(c0, Frames.Frame(f, k), m, sub)
            case _ => go(c0, Frames.Frame(f, k), m, sub)
          case _ => go(c0, Frames.Frame(f, k), m, sub)
        case _ => go(c0, Frames.Frame(f, k), m, sub)
      case Delay(t) => go(t(), k, m, sub)
      // handed out as a program OF THE OUTSIDE: an operation carries no index, so it is re-injected on the empty stack
      case Inject(op) => Bind(sub(Inject(op)), Resumption(k, m, sub))
      case r: Reset[h, s, y, pp] =>
        val up = new Widen[h, G]:
          def apply[S2 <: Tuple, A2](p: Freer[h, S2, A2]): Freer[G, S2, A2] = p
        go(r.body, Frames.End(), Stack.Delim[F, h, G, y, s, pp, B, Z](up, sub, k, m), up.andThen(sub))
      case s: Shift0[h, ?, ?, ?, ?, ?] =>
        val n = cut(Piece.Hole(k), m, s.has, sub)
        go(n.up(s.f(n.captured)), n.k, n.m, n.sub)
      case r: Resume[h, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[S2 <: Tuple, A2](p: Freer[h, S2, A2]): Freer[G, S2, A2] = p
        val n = r.k.under(up, sub, k, m)
        go(Return(r.x), n.k, n.m, n.sub)

  /** down the stack, GUIDED BY THE WITNESS: `Head` says the next delimiter is `p`'s, `Tail` says cross it. The
   * stack's type follows the witness, so `Done` (the empty stack) is not a case — nothing to throw */
  /** down the stack, GUIDED BY THE WITNESS: `Head` says the next delimiter is the one, `Tail` says cross it. The
   * stack's type follows the witness, so `Done` (the empty stack) is no case — nothing to throw. A `Delim` is
   * matched by a type test whose arguments the refined scrutinee determines, the two it cannot left as `?` */
  private def cut[F[+_], G[+_], X, T0 <: Tuple, A, Tp <: Tuple, Z, PP, H[+_], Y, O <: Tuple](
      piece: Piece[G, X, T0, A, Tp], m: Stack[F, G, A, Tp, Z], has: Has[Tp, At[PP, Freer[H, EmptyTuple, Y]], O], sub: Widen[G, F]): Cut[F, X, H, O, Y, Z] =
    has match
      case _: Has.Head[?, ?] => head[F, G, X, T0, A, Z, PP, H, Y, O](piece, m, sub)
      case t: Has.Tail[q, tt, ?, ?] => tail[F, G, X, T0, A, Z, q, tt, PP, H, Y, O](piece, m, t.rest, sub)

  /** the delimiter is this level's: the match on it is what proves the level's row and value are the prompt's */
  @tailrec private def head[F[+_], G[+_], X, T0 <: Tuple, A, Z, PP, H[+_], Y, O <: Tuple](
      piece: Piece[G, X, T0, A, At[PP, Freer[H, EmptyTuple, Y]] *: O], m: Stack[F, G, A, At[PP, Freer[H, EmptyTuple, Y]] *: O, Z], sub: Widen[G, F]): Cut[F, X, H, O, Y, Z] =
    m match
      case Stack.Run(out, rest) => head(Piece.Over(piece, out), rest, sub)
      case d @ Stack.Delim(_, _, _, _) => found(d)(piece)

  /** crossing delimiters: one call per delimiter crossed, BOUNDED by the length of the index — a static tuple */
  @tailrec private def tail[F[+_], G[+_], X, T0 <: Tuple, A, Z, Q, T <: Tuple, PP, H[+_], Y, O <: Tuple](
      piece: Piece[G, X, T0, A, Q *: T], m: Stack[F, G, A, Q *: T, Z], rest: Has[T, At[PP, Freer[H, EmptyTuple, Y]], O], sub: Widen[G, F]): Cut[F, X, H, O, Y, Z] =
    m match
      case Stack.Run(out, r) => tail(Piece.Over(piece, out), r, rest, sub)
      case d @ Stack.Delim(_, _, _, _) => crossed(d)(piece, rest)

  private def found[F[+_], H[+_], G0[+_], O <: Tuple, Y, P, B0, Z](d: Stack.Delim[F, H, G0, Y, O, P, B0, Z])
                   [X, T0 <: Tuple](piece: Piece[H, X, T0, Y, At[P, Freer[H, EmptyTuple, Y]] *: O]): Cut[F, X, H, O, Y, Z] =
    new Cut[F, X, H, O, Y, Z]:
      type G[+A1] = G0[A1]
      type B = B0
      def captured = Captured[H, X, T0, O, Y, P](piece)
      def up = d.up
      def k = d.out
      def m = d.rest
      def sub = d.sub

  private def crossed[F[+_], H[+_], G0[+_], T <: Tuple, Y0, P0, B0, Z](d: Stack.Delim[F, H, G0, Y0, T, P0, B0, Z])
                     [X, T0 <: Tuple, PP, H2[+_], Y, O <: Tuple](piece: Piece[H, X, T0, Y0, At[P0, Freer[H, EmptyTuple, Y0]] *: T], rest: Has[T, At[PP, Freer[H2, EmptyTuple, Y]], O]): Cut[F, X, H2, O, Y, Z] =
    cut(Piece.Under(d.up, d.out, piece), d.rest, rest, d.sub)

  @tailrec private[ts] def link[F[+_], G[+_], A0, T0 <: Tuple, A, T <: Tuple, Z](
      piece: Piece[G, A0, T0, A, T], m: Stack[F, G, A, T, Z], sub: Widen[G, F]): Next[F, A0, Z] =
    piece match
      case Piece.Hole(k0) => linked(k0, m, sub)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m), sub)
      case Piece.Under(up, out, prev) => link(prev, Stack.Delim(up, sub, out, m), up.andThen(sub))

  private def linked[F[+_], G0[+_], A0, B0, T0 <: Tuple, Z](k0: Frames[G0, A0, B0, T0], m0: Stack[F, G0, B0, T0, Z], sub0: Widen[G0, F]): Next[F, A0, Z] =
    new Next[F, A0, Z]:
      type G[+A1] = G0[A1]
      type B = B0
      type T = T0
      def k = k0
      def m = m0
      def sub = sub0
