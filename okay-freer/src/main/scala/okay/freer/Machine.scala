package okay.freer

import scala.annotation.tailrec
import Freer.*

/** the witness that the row `G` lies inside `F`: a polymorphic identity, made where the compiler knows it */
trait Widen[G[+_], F[+_]]:
  def apply[S, R, A](p: Freer[G, S, R, A]): Freer[F, S, R, A]
  def andThen[E[+_]](next: Widen[F, E]): Widen[G, E] =
    val self = this
    new Widen[G, E]:
      def apply[S, R, A](p: Freer[G, S, R, A]): Freer[E, S, R, A] = next(self(p))
object Widen:
  def refl[F[+_]]: Widen[F, F] = new Widen[F, F]:
    def apply[S, R, A](p: Freer[F, S, R, A]): Freer[F, S, R, A] = p

/** a segment over the row `G`: `A => Freer[G, S, R, B]` as data; contravariant in what it consumes */
enum Frames[G[+_], -A, B, S, R]:
  case End[G[+_], A, S]() extends Frames[G, A, A, S, S]
  case Frame[G[+_], A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T]) extends Frames[G, A, B, S, R]

/**
 * THE STACK: closes a level over `G` — value `B` at the answer `S`, the program's final answer `R` — into the run's
 * result over `F`, `Freer[F, S0, R0, Z]`. A delimiter's level has value = answer (`S`, `S`); when it returns, the
 * GADT says that answer is the program's final one, `R`, and `out` takes it as a value, at the answer `U` outside.
 */
enum Stack[F[+_], G[+_], B, S, R, S0, R0, Z]:
  case Done[F[+_], B, S, R]() extends Stack[F, F, B, S, R, S, R, B]
  case Run[F[+_], G[+_], B, S, R, B2, S2, S0, R0, Z](out: Frames[G, B, B2, S2, S], rest: Stack[F, G, B2, S2, R, S0, R0, Z])
    extends Stack[F, G, B, S, R, S0, R0, Z]
  case Delim[F[+_], H[+_], G[+_], Sp, S, R, B, S2, U, S0, R0, Z](p: Prompt[H, Sp], up: Widen[H, G], sub: Widen[G, F],
                                                            out: Frames[G, R, B, S2, U], rest: Stack[F, G, B, S2, U, S0, R0, Z])
    extends Stack[F, H, S, S, R, S0, R0, Z]

/** a captured piece: the segments from the hole (`A0` at `T0`) up to the delimiter, it not included */
enum Piece[G[+_], A0, T0, A, T]:
  case Hole[G[+_], A0, T0, B, S](k: Frames[G, A0, B, S, T0]) extends Piece[G, A0, T0, B, S]
  case Over[G[+_], A0, T0, X, T2, B, S2](prev: Piece[G, A0, T0, X, T2], out: Frames[G, X, B, S2, T2]) extends Piece[G, A0, T0, B, S2]

/** a captured continuation, `X => T [U, U]` for every `U`: the piece from the hole, put back under its delimiter,
 * delivers the answer at the hole, at any answer outside. `S` is the delimiter level's own value-and-answer */
final class Captured[H[+_], X, T, S](val p: Prompt[H, ?], val piece: Piece[H, X, T, S, S]):
  def apply[U](x: X): Freer[H, U, U, T] = Resume(x, this)
  /** as the pure function a shift's body receives, at the hole's own type (a polymorphic function type has no
   * variance, so it is built at exactly the type the body expects) */
  def kAt[X0 <: X]: [U] => X0 => Freer[H, U, U, T] = [U] => (x: X0) => apply[U](x)
  def under[F[+_], G[+_], B, S2, U, S0, R0, Z](up: Widen[H, G], sub: Widen[G, F], out: Frames[G, T, B, S2, U],
                                              rest: Stack[F, G, B, S2, U, S0, R0, Z]): Next[F, X, T, S0, R0, Z] =
    Machine.link(piece, Stack.Delim(p, up, sub, out, rest), up.andThen(sub))

/** the rest of a run after something handed out: applied by whoever answers it, the run goes on */
final class Resumption[F[+_], G[+_], A, B, S, T, S0, R0, Z](k: Frames[G, A, B, S, T], m: Stack[F, G, B, S, T, S0, R0, Z], sub: Widen[G, F])
  extends (A => Freer[F, S0, R0, Z]):
  def apply(a: A): Freer[F, S0, R0, Z] = Machine.go(Return(a), k, m, sub)

/** the machine's next state: a value `A` is due at the answer `T`, at the segment `k`, level `G`, over `m` */
sealed abstract class Next[F[+_], A, T, S0, R0, Z]:
  type G[+_]
  type B
  type S
  def k: Frames[G, A, B, S, T]
  def m: Stack[F, G, B, S, T, S0, R0, Z]
  def sub: Widen[G, F]

/** what a capture meets walking down: its delimiter (`Found`), or the run's bottom (`Gone`: the delimiter is
 * outside this run; the capture is handed out whole, and the rest of this run, resumed from the hole, answers `T` —
 * so it is re-closed by a `Done` at `T`, a fresh one, nothing re-typed) */
enum Cut[F[+_], X, T, R, H[+_], S0, R0, Z]:
  case Found[F[+_], X, T, R, H[+_], S0, R0, Z, S, G0[+_], B0, S20, U](piece: Piece[H, X, T, S, S], up: Widen[H, G0],
      out: Frames[G0, R, B0, S20, U], rest: Stack[F, G0, B0, S20, U, S0, R0, Z], sub: Widen[G0, F]) extends Cut[F, X, T, R, H, S0, R0, Z]
  case Gone[F[+_], X, T, R, H[+_], S0, R0, Z](next: Next[F, X, T, S0, T, Z], same: R =:= R0) extends Cut[F, X, T, R, H, S0, R0, Z]

/** one loop, one rule per node, nothing else */
object Machine:
  /** run to a head form: `Return(z)`, `Bind(op, rest)`, or `Bind(shift, rest)` — a capture whose delimiter is
   * outside this run, for the machine outside */
  def run[F[+_], S, R, A](p: Freer[F, S, R, A]): Freer[F, S, R, A] = go(p, Frames.End(), Stack.Done(), Widen.refl[F])

  @tailrec private[freer] def go[F[+_], G[+_], A, B, S, T, R, S0, R0, Z](
      c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[F, G, B, S, R, S0, R0, Z], sub: Widen[G, F]): Freer[F, S0, R0, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m, sub)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest, sub)
          // the body returned: its answer is the program's final one, and the delimiter delivers it
          case Stack.Delim(_, _, subOut, out, rest) => go(Return(a), out, rest, subOut)
      case Bind(c0, f) => f match
        case _: Resumption[?, ?, ?, ?, ?, ?, ?, ?, ?] => c0 match
          case Inject(_) | Shift(_, _) => k match
            case Frames.End() => m match
              case Stack.Done() => sub(c)
              case _ => go(c0, Frames.Frame(f, k), m, sub)
            case _ => go(c0, Frames.Frame(f, k), m, sub)
          case _ => go(c0, Frames.Frame(f, k), m, sub)
        case _ => go(c0, Frames.Frame(f, k), m, sub)
      case Delay(t) => go(t(), k, m, sub)
      // handed out as a program of the run: an operation carries no answer, so it is re-injected at the run's
      case Inject(op) => Bind(sub(Inject(op)), Resumption(k, m, sub))
      case r: Reset[h, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[S2, R2, A2](p: Freer[h, S2, R2, A2]): Freer[G, S2, R2, A2] = p
        go(r.body, Frames.End(), Stack.Delim(r.p, up, sub, k, m), up.andThen(sub))
      case s: Shift[h, ?, ?, ?, x, ?] =>
        cut(Piece.Hole(k), m, s.p, sub) match
          // the body goes on inside the delimiter put back, with its own value-and-answer
          case Cut.Found(piece, up, out, rest, subOut) =>
            go(s.f(Captured(s.p, piece).kAt[x]), Frames.End(), Stack.Delim(s.p, up, subOut, out, rest), up.andThen(subOut))
          // the delimiter is outside this run: the capture goes out whole, with the rest of the run
          case Cut.Gone(next, same) => Bind(same.liftCo[[r] =>> Freer[F, T, r, A]](sub(c)), Resumption(next.k, next.m, next.sub))
      case r: Resume[h, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[S2, R2, A2](p: Freer[h, S2, R2, A2]): Freer[G, S2, R2, A2] = p
        val n = r.k.under(up, sub, k, m)
        go(Return(r.x), n.k, n.m, n.sub)

  /** down the stack to the nearest delimiter — by identity, and the identity carries the row; another prompt's is
   * refused; the run's bottom instead means the delimiter is outside this run */
  @tailrec private def cut[F[+_], G[+_], X, T, A, Tp, R, S0, R0, Z, H[+_]](
      piece: Piece[G, X, T, A, Tp], m: Stack[F, G, A, Tp, R, S0, R0, Z], p: Prompt[H, ?], sub: Widen[G, F]): Cut[F, X, T, R, H, S0, R0, Z] =
    m match
      case Stack.Run(out, rest) => cut(Piece.Over(piece, out), rest, p, sub)
      case d @ Stack.Delim(_: p.type, _, _, _, _) => found(d)(piece)
      case Stack.Delim(q, _, _, _, _) => throw NotNearest(p.label, q.label)
      case Stack.Done() => Cut.Gone(link(piece, Stack.Done[F, A, Tp, T](), sub), summon)

  private def found[F[+_], H[+_], G0[+_], Sp, S, R, B0, S20, U, S0, R0, Z](d: Stack.Delim[F, H, G0, Sp, S, R, B0, S20, U, S0, R0, Z])
                   [X, T](piece: Piece[H, X, T, S, S]): Cut[F, X, T, R, H, S0, R0, Z] =
    Cut.Found(piece, d.up, d.out, d.rest, d.sub)

  @tailrec private[freer] def link[F[+_], G[+_], A0, T0, A, T, S0, R0, Z](
      piece: Piece[G, A0, T0, A, T], m: Stack[F, G, A, T, T0, S0, R0, Z], sub: Widen[G, F]): Next[F, A0, T0, S0, R0, Z] =
    piece match
      case Piece.Hole(k0) => linked(k0, m, sub)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m), sub)

  private def linked[F[+_], G0[+_], A0, B0, S1, T0, S0, R0, Z](k0: Frames[G0, A0, B0, S1, T0], m0: Stack[F, G0, B0, S1, T0, S0, R0, Z],
                                                              sub0: Widen[G0, F]): Next[F, A0, T0, S0, R0, Z] =
    new Next[F, A0, T0, S0, R0, Z]:
      type G[+A1] = G0[A1]
      type B = B0
      type S = S1
      def k = k0
      def m = m0
      def sub = sub0
