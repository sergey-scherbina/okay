package okay.freer

import scala.annotation.tailrec
import Freer.*

/** the witness that the row `G` lies inside `F`: a polymorphic identity, made where the compiler knows the
 * inclusion (a covariant match, `Widen.id`), composed along the levels (`andThen`). Never a cast */
trait Widen[G[_, _, +_], F[_, _, +_]]:
  def apply[S, R, A](p: Freer[G, S, R, A]): Freer[F, S, R, A]
  def andThen[E[_, _, +_]](next: Widen[F, E]): Widen[G, E] =
    val self = this
    new Widen[G, E]:
      def apply[S, R, A](p: Freer[G, S, R, A]): Freer[E, S, R, A] = next(self(p))
object Widen:
  def refl[F[_, _, +_]]: Widen[F, F] = new Widen[F, F]:
    def apply[S, R, A](p: Freer[F, S, R, A]): Freer[F, S, R, A] = p

/** a SEGMENT over the row `G`: `A => Freer[G, S, R, B]` as data */
enum Frames[G[_, _, +_], -A, B, S, R]:
  case End[G[_, _, +_], A, S]() extends Frames[G, A, A, S, S]
  case Frame[G[_, _, +_], A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T]) extends Frames[G, A, B, S, R]

/**
 * THE STACK: closes a level over the row `G` computing `Freer[G, S, _, B]` into the run's result over `F`,
 * `Freer[F, S0, _, Z]`. The answer index is the PROGRAM's, not the stack's: no node holds a value of it, so it is
 * not a parameter here, and an operation handed out keeps its own. A delimiter joins two levels, the body's row
 * `H` above and `G` below, with the witness `H` inside `G` made when it was pushed.
 */
enum Stack[F[_, _, +_], G[_, _, +_], B, S, S0, Z]:
  case Done[F[_, _, +_], B, S]() extends Stack[F, F, B, S, S, B]
  case Run[F[_, _, +_], G[_, _, +_], B, S, B2, S2, S0, Z](out: Frames[G, B, B2, S2, S], rest: Stack[F, G, B2, S2, S0, Z])
    extends Stack[F, G, B, S, S0, Z]
  case Delim[F[_, _, +_], H[_, _, +_], G[_, _, +_], S, Y, B, S2, S0, Z](
      p: Prompt[H, S, Y], up: Widen[H, G], sub: Widen[G, F], out: Frames[G, Y, B, S2, S], rest: Stack[F, G, B, S2, S0, Z])
    extends Stack[F, H, Y, S, S0, Z]

/** a captured piece, built outward from the hole (`A0` at `T0`, at some level) to the top (`A` at `T`, at the
 * level of the row `G`); a delimiter crossed keeps its witness, so the piece links back anywhere */
enum Piece[G[_, _, +_], A0, T0, A, T]:
  case Hole[G[_, _, +_], A0, T0, B, S](k: Frames[G, A0, B, S, T0]) extends Piece[G, A0, T0, B, S]
  case Over[G[_, _, +_], A0, T0, X, T2, B, S2](prev: Piece[G, A0, T0, X, T2], out: Frames[G, X, B, S2, T2])
    extends Piece[G, A0, T0, B, S2]
  case Under[H[_, _, +_], G[_, _, +_], A0, T0, S, Y, B, S2](
      prev: Piece[H, A0, T0, Y, S], p: Prompt[H, S, Y], up: Widen[H, G], out: Frames[G, Y, B, S2, S])
    extends Piece[G, A0, T0, B, S2]

/** a captured continuation: the piece up to `p`'s delimiter, the delimiter included; applied, it is the `Resume`
 * node, which only the machine answers */
final class Captured[H[_, _, +_], X, T, S, Y](val piece: Piece[H, X, T, Y, S], val p: Prompt[H, S, Y])
  extends (X => Freer[H, S, T, Y]):
  def apply(x: X): Freer[H, S, T, Y] = Resume(x, this)
  /** the piece linked onto a live stack, the delimiter put back over `out`/`rest`, at the level `G` */
  def under[F[_, _, +_], G[_, _, +_], B, S2, S0, Z](up: Widen[H, G], sub: Widen[G, F], out: Frames[G, Y, B, S2, S],
                                                    rest: Stack[F, G, B, S2, S0, Z]): Next[F, X, T, S0, Z] =
    Machine.link(piece, Stack.Delim(p, up, sub, out, rest), up.andThen(sub))

/** the rest of a run after an operation handed out: whoever answers it applies this, and the run goes on */
final class Resumption[F[_, _, +_], G[_, _, +_], A, B, S, T, S0, Z](k: Frames[G, A, B, S, T], m: Stack[F, G, B, S, S0, Z],
                                                                    sub: Widen[G, F])
  extends (A => Freer[F, S0, T, Z]):
  def apply(a: A): Freer[F, S0, T, Z] = Machine.go(Return(a), k, m, sub)

/** the machine's next state: a value `A` at index `T` is due at the segment `k`, at the level `G`, over the stack `m` */
sealed abstract class Next[F[_, _, +_], A, T, S0, Z]:
  type G[_, _, +_]
  type B
  type S
  def k: Frames[G, A, B, S, T]
  def m: Stack[F, G, B, S, S0, Z]
  def sub: Widen[G, F]

/** what a capture found: the captured continuation, and the machine's next state under its delimiter */
sealed abstract class Cut[F[_, _, +_], X, T, H[_, _, +_], S, Y, S0, Z] extends Next[F, Y, S, S0, Z]:
  def captured: Captured[H, X, T, S, Y]
  def up: Widen[H, G]

/** one loop, one rule per node; nothing else */
object Machine:

  /** run to a head form over `F`: `Return(z)`, or `Bind(op, rest)` for the first operation of `F` */
  def run[F[_, _, +_], S, R, A](p: Freer[F, S, R, A]): Freer[F, S, R, A] =
    go(p, Frames.End[F, A, S](), Stack.Done[F, A, S](), Widen.refl[F])

  @tailrec private[freer] def go[F[_, _, +_], G[_, _, +_], A, B, S, T, R, S0, Z](
      c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[F, G, B, S, S0, Z], sub: Widen[G, F]): Freer[F, S0, R, Z] =
    c match
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m, sub)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest, sub)
          case Stack.Delim(_, up, subOut, out, rest) => go(up(Return(a)), out, rest, subOut)
      case Bind(c0, f) => f match
        // a head form the machine itself handed out (its continuation is a Resumption), at the top with nothing
        // pending, is already the answer: a run over a run allocates nothing
        case _: Resumption[?, ?, ?, ?, ?, ?, ?, ?] => c0 match
          case Inject(_) | Perform(_) => k match
            case Frames.End() => m match
              case Stack.Done() => sub(c)
              case _ => go(c0, Frames.Frame(f, k), m, sub)
            case _ => go(c0, Frames.Frame(f, k), m, sub)
          case _ => go(c0, Frames.Frame(f, k), m, sub)
        case _ => go(c0, Frames.Frame(f, k), m, sub)
      case Delay(t) => go(t(), k, m, sub)
      // handed out: the node itself, lifted; the rest of the run as a function
      case Inject(_) => Bind(sub(c), Resumption(k, m, sub))
      case Perform(_) => Bind(sub(c), Resumption(k, m, sub))
      // `h <: G` by the covariant match: the witness is the identity, written where the compiler knows it
      case r: Reset[h, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[S2, R2, A2](p: Freer[h, S2, R2, A2]): Freer[G, S2, R2, A2] = p
        go(r.body, Frames.End(), Stack.Delim(r.p, up, sub, k, m), up.andThen(sub))
      case s: Shift0[h, ?, ?, ?, ?, ?] =>
        val n = cut(Piece.Hole(k), m, s.p, sub)
        go(n.up(s.f(n.captured)), n.k, n.m, n.sub)
      case r: Resume[h, ?, ?, ?, ?] =>
        val up = new Widen[h, G]:
          def apply[S2, R2, A2](p: Freer[h, S2, R2, A2]): Freer[G, S2, R2, A2] = p
        val n = r.k.under(up, sub, k, m)
        go(Return(r.x), n.k, n.m, n.sub)

  /** down the stack to the delimiter of `p` — by identity, and the identity carries its types — the piece growing
   * outward; none: `NoPrompt`. What comes back: the captured continuation, and the state under the delimiter */
  @tailrec private def cut[F[_, _, +_], G[_, _, +_], X, T, A, Tp, S0, Z, H[_, _, +_], S, Y](
      piece: Piece[G, X, T, A, Tp], m: Stack[F, G, A, Tp, S0, Z], p: Prompt[H, S, Y], sub: Widen[G, F]): Cut[F, X, T, H, S, Y, S0, Z] =
    m match
      case Stack.Delim(_: p.type, up, subOut, out, rest) => found(Captured(piece, p), up, subOut, out, rest)
      case Stack.Delim(q, up, subOut, out, rest) => cut(Piece.Under(piece, q, up, out), rest, p, subOut)
      case Stack.Run(out, rest) => cut(Piece.Over(piece, out), rest, p, sub)
      case Stack.Done() => throw NoPrompt(p.label)

  private def found[F[_, _, +_], G0[_, _, +_], X, T, H[_, _, +_], S, Y, B0, S20, S0, Z](
      c: Captured[H, X, T, S, Y], up0: Widen[H, G0], sub0: Widen[G0, F], out0: Frames[G0, Y, B0, S20, S],
      rest0: Stack[F, G0, B0, S20, S0, Z]): Cut[F, X, T, H, S, Y, S0, Z] =
    new Cut[F, X, T, H, S, Y, S0, Z]:
      type G[S1, R1, +A1] = G0[S1, R1, A1]
      type B = B0
      type S = S20
      def captured = c
      def up = up0
      def k = out0
      def m = rest0
      def sub = sub0

  /** a piece put back over a stack, outermost node first */
  @tailrec private[freer] def link[F[_, _, +_], G[_, _, +_], A0, T0, A, T, S0, Z](
      piece: Piece[G, A0, T0, A, T], m: Stack[F, G, A, T, S0, Z], sub: Widen[G, F]): Next[F, A0, T0, S0, Z] =
    piece match
      case Piece.Hole(k0) => linked(k0, m, sub)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m), sub)
      case Piece.Under(prev, p, up, out) => link(prev, Stack.Delim(p, up, sub, out, m), up.andThen(sub))

  private def linked[F[_, _, +_], G0[_, _, +_], A0, B0, S0, T0, S00, Z](
      k0: Frames[G0, A0, B0, S0, T0], m0: Stack[F, G0, B0, S0, S00, Z], sub0: Widen[G0, F]): Next[F, A0, T0, S00, Z] =
    new Next[F, A0, T0, S00, Z]:
      type G[S1, R1, +A1] = G0[S1, R1, A1]
      type B = B0
      type S = S0
      def k = k0
      def m = m0
      def sub = sub0
