package okay.freer

import scala.annotation.tailrec
import Freer.*

/** the witness that the row `G` lies inside `F`: a polymorphic identity, made where the compiler knows it */
trait Widen[G[+_], F[+_]]:
  def apply[D, S, R, A](p: Freer[G, D, S, R, A]): Freer[F, D, S, R, A]
  def andThen[E[+_]](next: Widen[F, E]): Widen[G, E] =
    val self = this
    new Widen[G, E]:
      def apply[D, S, R, A](p: Freer[G, D, S, R, A]): Freer[E, D, S, R, A] = next(self(p))
object Widen:
  def refl[F[+_]]: Widen[F, F] = new Widen[F, F]:
    def apply[D, S, R, A](p: Freer[F, D, S, R, A]): Freer[F, D, S, R, A] = p

/** a segment over the row `G` under `D`: `A => Freer[G, D, S, R, B]` as data; contravariant in what it consumes */
enum Frames[G[+_], -A, B, D, S, R]:
  case End[G[+_], A, D, S]() extends Frames[G, A, A, D, S, S]
  case Frame[G[+_], A, X, B, D, S, T, R](f: A => Freer[G, D, T, R, X], rest: Frames[G, X, B, D, S, T]) extends Frames[G, A, B, D, S, R]

/**
 * THE STACK: closes a level over `G` under `D` — value `B` at the answer `S`, the program's final answer `R` — into
 * the run's result over `F`, `Freer[F, D0, S0, R0, Z]`. A delimiter's level has value = answer (`S`, `S`) and the
 * delimiter as its index; when it returns, the GADT says that answer is the program's final one, `R`, and `out`
 * takes it as a value, at the answer `U` outside, under the delimiter outside.
 */
enum Stack[F[+_], G[+_], D, B, S, R, D0, S0, R0, Z]:
  case Done[F[+_], D, B, S, R]() extends Stack[F, F, D, B, S, R, D, S, R, B]
  case Run[F[+_], G[+_], D, B, S, R, B2, S2, D0, S0, R0, Z](out: Frames[G, B, B2, D, S2, S], rest: Stack[F, G, D, B2, S2, R, D0, S0, R0, Z])
    extends Stack[F, G, D, B, S, R, D0, S0, R0, Z]
  case Delim[F[+_], H[+_], G[+_], D, S, R, B, S2, U, D0, S0, R0, Z](
      up: Widen[H, G], sub: Widen[G, F], out: Frames[G, R, B, D, S2, U], rest: Stack[F, G, D, B, S2, U, D0, S0, R0, Z])
    extends Stack[F, H, Lvl[H], S, S, R, D0, S0, R0, Z]

/** a captured piece: the segments from the hole (`A0` at `T0`) up to the delimiter, it not included */
enum Piece[G[+_], A0, D, T0, A, T]:
  case Hole[G[+_], A0, D, T0, B, S](k: Frames[G, A0, B, D, S, T0]) extends Piece[G, A0, D, T0, B, S]
  case Over[G[+_], A0, D, T0, X, T2, B, S2](prev: Piece[G, A0, D, T0, X, T2], out: Frames[G, X, B, D, S2, T2]) extends Piece[G, A0, D, T0, B, S2]

/** a captured continuation, `X => T [U, U]` for every `U`, under any delimiter `D`: the piece from the hole, put
 * back under a delimiter of its own, delivers the answer at the hole, at any answer outside. `S` is the delimiter
 * level's own value-and-answer */
final class Captured[H[+_], X, T, S](val piece: Piece[H, X, Lvl[H], T, S, S]):
  def apply[U, D](x: X): Freer[H, D, U, U, T] = Resume(x, this)
  /** as the pure function a shift's body receives, at the hole's own type (a polymorphic function type has no
   * variance, so it is built at exactly the type the body expects) */
  def kAt[X0 <: X]: [U, D] => X0 => Freer[H, D, U, U, T] = [U, D] => (x: X0) => apply[U, D](x)
  def under[F[+_], G[+_], B, D, S2, U, D0, S0, R0, Z](up: Widen[H, G], sub: Widen[G, F], out: Frames[G, T, B, D, S2, U],
                                                     rest: Stack[F, G, D, B, S2, U, D0, S0, R0, Z]): Next[F, X, T, D0, S0, R0, Z] =
    Machine.link(piece, Stack.Delim(up, sub, out, rest), up.andThen(sub))

/** the rest of a run after something handed out: applied by whoever answers it, the run goes on */
final class Resumption[F[+_], G[+_], A, B, D, S, T, D0, S0, R0, Z](k: Frames[G, A, B, D, S, T], m: Stack[F, G, D, B, S, T, D0, S0, R0, Z], sub: Widen[G, F])
  extends (A => Freer[F, D0, S0, R0, Z]):
  def apply(a: A): Freer[F, D0, S0, R0, Z] = Machine.go(Return(a), k, m, sub)

/** the machine's next state: a value `A` is due at the answer `T`, at the segment `k`, level `G` under `D`, over `m` */
sealed abstract class Next[F[+_], A, T, D0, S0, R0, Z]:
  type G[+_]
  type B
  type D
  type S
  def k: Frames[G, A, B, D, S, T]
  def m: Stack[F, G, D, B, S, T, D0, S0, R0, Z]
  def sub: Widen[G, F]

/** what a capture meets walking down: the nearest delimiter (`Found`), or the run's bottom (`Gone`: the delimiter
 * is outside this run; the capture is handed out whole as a program of the run, with the rest of this run, resumed
 * from the hole, after it — re-closed by a `Done` at the hole's answer `T`, a fresh one, nothing re-typed) */
enum Cut[F[+_], X, T, R, H[+_], D0, S0, R0, Z]:
  case Found[F[+_], X, T, R, H[+_], D0, S0, R0, Z, S, G0[+_], B0, D, S20, U](piece: Piece[H, X, Lvl[H], T, S, S], up: Widen[H, G0],
      out: Frames[G0, R, B0, D, S20, U], rest: Stack[F, G0, D, B0, S20, U, D0, S0, R0, Z], sub: Widen[G0, F]) extends Cut[F, X, T, R, H, D0, S0, R0, Z]
  case Gone[F[+_], X, T, R, H[+_], D0, S0, R0, Z](out: Freer[F, D0, S0, R0, Z]) extends Cut[F, X, T, R, H, D0, S0, R0, Z]

/** one loop, one rule per node, nothing else */
object Machine:
  /** run to a head form: `Return(z)`, `Bind(op, rest)`, or `Bind(shift, rest)` — a capture whose delimiter is
   * outside this run, for the machine outside. Under any delimiter: a handler is a run inside one */
  def run[F[+_], D, S, R, A](p: Freer[F, D, S, R, A]): Freer[F, D, S, R, A] = go(p, Frames.End(), Stack.Done(), Widen.refl[F])

  @tailrec private[freer] def go[F[+_], G[+_], A, B, D, S, T, R, D0, S0, R0, Z](
      c: Freer[G, D, T, R, A], k: Frames[G, A, B, D, S, T], m: Stack[F, G, D, B, S, R, D0, S0, R0, Z], sub: Widen[G, F]): Freer[F, D0, S0, R0, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m, sub)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest, sub)
          // the body returned: its answer is the program's final one, and the delimiter delivers it
          case Stack.Delim(_, subOut, out, rest) => go(Return(a), out, rest, subOut)
      case Bind(c0, f) => f match
        case _: Resumption[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => c0 match
          case Inject(_) | Shift(_) => k match
            case Frames.End() => m match
              case Stack.Done() => sub(c)
              case _ => go(c0, Frames.Frame(f, k), m, sub)
            case _ => go(c0, Frames.Frame(f, k), m, sub)
          case _ => go(c0, Frames.Frame(f, k), m, sub)
        case _ => go(c0, Frames.Frame(f, k), m, sub)
      case Delay(t) => go(t(), k, m, sub)
      // handed out as a program of the run: an operation carries no answer and no delimiter, so it is re-injected at the run's
      case Inject(op) => Bind(sub(Inject(op)), Resumption(k, m, sub))
      case r: Reset[h, ?, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[D2, S2, R2, A2](p: Freer[h, D2, S2, R2, A2]): Freer[G, D2, S2, R2, A2] = p
        go(r.body, Frames.End(), Stack.Delim(up, sub, k, m), up.andThen(sub))
      case s: Shift[h, ?, ?, x, ?] =>
        cut[F, G, x, T, B, S, R, D0, S0, R0, Z, h](s, Piece.Hole(k), m, sub) match
          // the body goes on inside the delimiter put back, with its own value-and-answer
          case Cut.Found(piece, up, out, rest, subOut) =>
            go(s.f(Captured(piece).kAt[x]), Frames.End(), Stack.Delim(up, subOut, out, rest), up.andThen(subOut))
          case Cut.Gone(out) => out
      case r: Resume[h, ?, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[D2, S2, R2, A2](p: Freer[h, D2, S2, R2, A2]): Freer[G, D2, S2, R2, A2] = p
        val n = r.k.under(up, sub, k, m)
        go(Return(r.x), n.k, n.m, n.sub)

  /** down the stack to the nearest delimiter — the index says its row; the run's bottom instead means the delimiter
   * is outside this run, and the capture is handed out here, where the run's types are known */
  @tailrec private def cut[F[+_], G[+_], X, T, A, Tp, R, D0, S0, R0, Z, H[+_]](
      c: Freer[G, Lvl[H], T, R, X], piece: Piece[G, X, Lvl[H], T, A, Tp], m: Stack[F, G, Lvl[H], A, Tp, R, D0, S0, R0, Z], sub: Widen[G, F])
    : Cut[F, X, T, R, H, D0, S0, R0, Z] =
    m match
      case Stack.Run(out, rest) => cut(c, Piece.Over(piece, out), rest, sub)
      case d @ Stack.Delim(_, _, _, _) => found(d)(piece)
      case Stack.Done() =>
        val n = link(piece, Stack.Done[F, Lvl[H], A, Tp, T](), sub)
        Cut.Gone(Bind(sub(c), Resumption(n.k, n.m, n.sub)))

  private def found[F[+_], H[+_], G0[+_], D, S, R, B0, S20, U, D0, S0, R0, Z](d: Stack.Delim[F, H, G0, D, S, R, B0, S20, U, D0, S0, R0, Z])
                   [X, T](piece: Piece[H, X, Lvl[H], T, S, S]): Cut[F, X, T, R, H, D0, S0, R0, Z] =
    Cut.Found(piece, d.up, d.out, d.rest, d.sub)

  @tailrec private[freer] def link[F[+_], G[+_], A0, D, T0, A, T, D0, S0, R0, Z](
      piece: Piece[G, A0, D, T0, A, T], m: Stack[F, G, D, A, T, T0, D0, S0, R0, Z], sub: Widen[G, F]): Next[F, A0, T0, D0, S0, R0, Z] =
    piece match
      case Piece.Hole(k0) => linked(k0, m, sub)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m), sub)

  private def linked[F[+_], G0[+_], A0, B0, D1, S1, T0, D0, S0, R0, Z](
      k0: Frames[G0, A0, B0, D1, S1, T0], m0: Stack[F, G0, D1, B0, S1, T0, D0, S0, R0, Z], sub0: Widen[G0, F]): Next[F, A0, T0, D0, S0, R0, Z] =
    new Next[F, A0, T0, D0, S0, R0, Z]:
      type G[+A1] = G0[A1]
      type B = B0
      type D = D1
      type S = S1
      def k = k0
      def m = m0
      def sub = sub0
