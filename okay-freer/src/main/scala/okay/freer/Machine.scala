package okay.freer

import scala.annotation.tailrec
import Freer.*

/** a SEGMENT: `A => Freer[G, S, R, B]` as data, frames joined as `Bind` joins them. Contravariant in `A`: it consumes
 * a value */
enum Frames[G[_, _, +_], -A, B, S, R]:
  case End[G[_, _, +_], A, S]() extends Frames[G, A, A, S, S]
  case Frame[G[_, _, +_], A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T]) extends Frames[G, A, B, S, R]

/**
 * THE STACK: what closes a level computing `Freer[Row[F], S, R, B]` into the run's result, `Freer[F, S0, R, Z]`.
 * The answer index `R` is ONE for every node and for the result: an answer chains through a boundary as it chains
 * through a `Bind`, so an operation handed out stands at the run's answer and the hand-out needs no claim.
 */
enum Stack[F[_, _, +_], B, S, R, S0, Z]:
  /** the run's end: the level's value is the result */
  case Done[F[_, _, +_], B, S, R]() extends Stack[F, B, S, R, S, B]
  /** a plain boundary: the level's value flows on into `out` (a resumption spliced its segments above these) */
  case Run[F[_, _, +_], B, S, R, B2, S2, S0, Z](out: Frames[Row[F], B, B2, S2, S], rest: Stack[F, B2, S2, R, S0, Z])
    extends Stack[F, B, S, R, S0, Z]
  /** a delimiter: the level is its body; its value goes through `ret`, then on into `out` */
  case Delim[F[_, _, +_], X, S, T, R, Y, B, S2, S0, Z](p: Prompt[S, Y], ret: X => Freer[Row[F], S, T, Y],
                                                        out: Frames[Row[F], Y, B, S2, S], rest: Stack[F, B, S2, R, S0, Z])
    extends Stack[F, X, T, R, S0, Z]

/** a captured piece of the stack, built outward from the hole — value `A0` at index `T0` — to value `A` at `T`:
 * the segments and the boundaries between them, the delimiter captured to NOT among them (`Captured` holds it) */
enum Piece[F[_, _, +_], A0, T0, A, T]:
  case Hole[F[_, _, +_], A0, T0, B, S](k: Frames[Row[F], A0, B, S, T0]) extends Piece[F, A0, T0, B, S]
  case Over[F[_, _, +_], A0, T0, X, T2, B, S2](prev: Piece[F, A0, T0, X, T2], out: Frames[Row[F], X, B, S2, T2])
    extends Piece[F, A0, T0, B, S2]
  case Under[F[_, _, +_], A0, T0, X, S, T2, Y, B, S2](prev: Piece[F, A0, T0, X, T2], p: Prompt[S, Y],
                                                      ret: X => Freer[Row[F], S, T2, Y], out: Frames[Row[F], Y, B, S2, S])
    extends Piece[F, A0, T0, B, S2]

/**
 * A CAPTURED continuation: the piece from the hole up to the delimiter of `p`, and that delimiter's `ret` — an
 * `X => Freer[Row[F], S, T, Y]`, the shape `Control.Shift0` hands its body. Applied, it answers LAZILY: a bind off
 * the unit whose continuation (`Resume`) the machine recognises and splices, and any other interpreter forces,
 * which runs the machine on the piece alone — the continuation carries its own interpreter.
 */
final class Captured[F[_, _, +_], X, T, S, Y, Xd, Td](val piece: Piece[F, X, T, Xd, Td], val p: Prompt[S, Y],
                                                       val ret: Xd => Freer[Row[F], S, Td, Y])
  extends (X => Freer[Row[F], S, T, Y]):
  def apply(x: X): Freer[Row[F], S, T, Y] = Bind(Return(()), Resume(x, this))

  /** the piece linked onto the live stack, under the delimiter put back with `out`/`rest` beneath it */
  def under[B, S2, R, S0, Z](out: Frames[Row[F], Y, B, S2, S], rest: Stack[F, B, S2, R, S0, Z]): Linked[F, X, T, R, S0, Z] =
    Machine.link(piece, Stack.Delim(p, ret, out, rest))

  /** the piece alone: the delimiter put back over nothing, run to a head form */
  def alone(x: X): Freer[F, S, T, Y] =
    val l = Machine.link(piece, Stack.Delim(p, ret, Frames.End[Row[F], Y, S](), Stack.Done[F, Y, S, T]()))
    Machine.go(Return(x), l.k, l.m)

/** `k(x)` pending: forced by an interpreter, the piece runs alone; met by the machine, it is spliced */
final class Resume[F[_, _, +_], X, T, S, Y](val x: X, val captured: Captured[F, X, T, S, Y, ?, ?])
  extends (Unit => Freer[Row[F], S, T, Y]):
  def apply(u: Unit): Freer[Row[F], S, T, Y] = captured.alone(x)
  /** spliced: the live stack is typed at the run's answer, which is this `T` — the lazy form is `Bind(Return(()), r)`,
   * and a `Return` on the left of a `Bind` makes the bind's middle index its answer */
  def splice[B, S2, S0, Z](out: Frames[Row[F], Y, B, S2, S], rest: Stack[F, B, S2, T, S0, Z]): Next[F, T, S0, Z] =
    val l = captured.under(out, rest)
    Machine.next(Return(x), l.k, l.m)

/** the rest of a run after an operation handed out: applied by whoever answers the operation, the run goes on */
final class Resumption[F[_, _, +_], A, B, S, T, S0, Z](k: Frames[Row[F], A, B, S, T], m: Stack[F, B, S, T, S0, Z])
  extends (A => Freer[F, S0, T, Z]):
  def apply(a: A): Freer[F, S0, T, Z] = Machine.go(Return(a), k, m)

/** the machine's state after a step: a program, its segment, the stack under it */
sealed abstract class Next[F[_, _, +_], R, S0, Z]:
  type A
  type B
  type S
  type T
  def c: Freer[Row[F], T, R, A]
  def k: Frames[Row[F], A, B, S, T]
  def m: Stack[F, B, S, R, S0, Z]

/** a piece linked: the segment at its hole, and the stack under it */
sealed abstract class Linked[F[_, _, +_], A, T, R, S0, Z]:
  type B
  type S
  def k: Frames[Row[F], A, B, S, T]
  def m: Stack[F, B, S, R, S0, Z]

/** what a capture found: the captured continuation, and what lay under its delimiter */
sealed abstract class Found[F[_, _, +_], X, T, S, Y, R, S0, Z]:
  type B
  type S2
  def k: X => Freer[Row[F], S, T, Y]
  def out: Frames[Row[F], Y, B, S2, S]
  def rest: Stack[F, B, S2, R, S0, Z]

/**
 * THE MACHINE (specs/cont-core.md): one loop, five rules, over a program whose row is `Row[F]`. It answers
 * `Control`'s operations and hands every other out as a head form, `Bind(op, rest)`, for whoever handles `F`.
 */
object Machine:

  /** run to a head form over `F`: `Return(z)`, or `Bind(op, rest)` for the first operation of `F` */
  def run[F[_, _, +_], S, R, A](p: Freer[Row[F], S, R, A]): Freer[F, S, R, A] =
    go(p, Frames.End[Row[F], A, S](), Stack.Done[F, A, S, R]())

  @tailrec private[freer] def go[F[_, _, +_], A, B, S, T, R, S0, Z](c: Freer[Row[F], T, R, A], k: Frames[Row[F], A, B, S, T],
                                                                     m: Stack[F, B, S, R, S0, Z]): Freer[F, S0, R, Z] =
    c match
      // 3: a value: pop a frame; at the end of a segment pop the node under it
      case Return(a) => k match
        case Frames.Frame(f, rest) => go(f(a), rest, m)
        case Frames.End() => m match
          case Stack.Done() => Return(a)
          case Stack.Run(out, rest) => go(Return(a), out, rest)
          case Stack.Delim(_, ret, out, rest) => go(ret(a), out, rest)
      case Bind(c0, f) => f match
        // 2: a resumption in its lazy form, `Bind(Return(()), r)`: its piece spliced onto the live stack, the
        // delimiter put back; `Return` on the left makes the bind's middle index the run's answer
        case r: Resume[?, ?, ?, ?, ?] => c0 match
          case Return(_) =>
            val n = resumed(r, k, m)
            go(n.c, n.k, n.m)
          case _ => go(c0, Frames.Frame(f, k), m)
        // 1: push a frame, go into the left side
        case _ => go(c0, Frames.Frame(f, k), m)
      case Inject(op) => control(op) match
        case null => Bind(Inject(effect(op)), Resumption(k, m))
        case ctl =>
          val n = step(ctl, k, m)
          go(n.c, n.k, n.m)
      case Perform(op) => control(op) match
        case null => Bind(Perform(effect(op)), Resumption(k, answered[F, B, S, T, R, S0, Z](m)))
        case ctl =>
          val n = step(ctl, k, m)
          go(n.c, n.k, n.m)

  /** 4 and 5 */
  private def step[F[_, _, +_], A, B, S, T, R, S0, Z](ctl: Control[F, T, R, A], k: Frames[Row[F], A, B, S, T],
                                                       m: Stack[F, B, S, R, S0, Z]): Next[F, R, S0, Z] =
    ctl match
      // 4: push the delimiter, the segment empty, go into the body
      case Control.Reset(p, ret, body) => next(body, Frames.End(), Stack.Delim(p, ret, k, m))
      // 5: cut the stack at the delimiter of `p`; the body goes on in its place
      case Control.Shift0(p, f) =>
        val fd = cut(Piece.Hole(k), m, p)
        next(f(fd.k), fd.out, fd.rest)

  /**
   * THE UNION CAST, first half: an operation of `Row[F] = Ctl[F] + F` that is a `Control` is this machine's — the
   * row is closed under nesting, so every delimiter and capture on it is answered here — and its parameters are
   * the row's at this node. The class is what the runtime can test; `F` is abstract, so the arguments are not.
   */
  private def control[F[_, _, +_], S, R, A](op: Row[F][S, R, A]): Control[F, S, R, A] | Null = op match
    case c: Control[?, ?, ?, ?] => c.asInstanceOf[Control[F, S, R, A]]
    case _ => null

  /** THE UNION CAST, second half: an operation of `Row[F]` that `control` refused is `F`'s — a match on a union
   * does not narrow its fall-through */
  private def effect[F[_, _, +_], S, R, A](op: Row[F][S, R, A]): F[S, R, A] = op.asInstanceOf[F[S, R, A]]

  /** THE RESUMPTION CAST: a `Resume` is the function the `Bind` holds, `Unit => Freer[Row[F], S, T, Y]` by its own
   * extends clause, so its parameters are the ones the `Bind` was matched with: `S` the bind's own index (`T`
   * here), `T` the bind's middle index, which is the run's answer `R` since the left side is a `Return` */
  private def resumed[F[_, _, +_], A, B, S, T, R, S0, Z](r: Resume[?, ?, ?, ?, ?], k: Frames[Row[F], A, B, S, T],
                                                          m: Stack[F, B, S, R, S0, Z]): Next[F, R, S0, Z] =
    r.asInstanceOf[Resume[F, ?, R, T, A]].splice(k, m)

  /** THE ANSWER CAST: an operation moving the answer from `T` to `R` was handed out; whoever answers it with a
   * value takes the move on itself (as `runState` takes `Put`'s), so from the answer on the run answers `T`. `R`
   * is phantom in every node of the stack — no value of that type is held — so the re-indexing changes nothing */
  private def answered[F[_, _, +_], B, S, T, R, S0, Z](m: Stack[F, B, S, R, S0, Z]): Stack[F, B, S, T, S0, Z] =
    m.asInstanceOf[Stack[F, B, S, T, S0, Z]]

  /** walk down to the delimiter of `p`, the piece growing outward; none: `NoPrompt` */
  @tailrec private def cut[F[_, _, +_], X, T, A, Tp, R, S0, Z, S, Y](piece: Piece[F, X, T, A, Tp], m: Stack[F, A, Tp, R, S0, Z],
                                                                      p: Prompt[S, Y]): Found[F, X, T, S, Y, R, S0, Z] =
    m match
      case d @ Stack.Delim(p2, ret, out, rest) =>
        // THE PROMPT CAST: a prompt is allocated once, at one type, and `eq` is that allocation — so the delimiter
        // of `p` carries `p`'s `S` and `Y`. The two existentials under it are named `Any` only to stay one type
        if p2 eq p then found(piece, d.asInstanceOf[Stack.Delim[F, A, S, Tp, R, Y, Any, Any, S0, Z]])
        else cut(Piece.Under(piece, p2, ret, out), rest, p)
      case Stack.Run(out, rest) => cut(Piece.Over(piece, out), rest, p)
      case Stack.Done() => throw NoPrompt(p.label)

  private def found[F[_, _, +_], X, T, A, Tp, R, S0, Z, S, Y, B0, S20](piece: Piece[F, X, T, A, Tp],
                                                                        d: Stack.Delim[F, A, S, Tp, R, Y, B0, S20, S0, Z])
    : Found[F, X, T, S, Y, R, S0, Z] =
    new Found[F, X, T, S, Y, R, S0, Z]:
      type B = B0
      type S2 = S20
      val k = Captured(piece, d.p, d.ret)
      def out = d.out
      def rest = d.rest

  /** a piece put back over a stack, outermost node first: the segment at its hole and the stack under it */
  @tailrec private[freer] def link[F[_, _, +_], A0, T0, A, T, R, S0, Z](piece: Piece[F, A0, T0, A, T],
                                                                        m: Stack[F, A, T, R, S0, Z]): Linked[F, A0, T0, R, S0, Z] =
    piece match
      case Piece.Hole(k0) => linked(k0, m)
      case Piece.Over(prev, out) => link(prev, Stack.Run(out, m))
      case Piece.Under(prev, p, ret, out) => link(prev, Stack.Delim(p, ret, out, m))

  private def linked[F[_, _, +_], A0, B0, S0, T0, R, S00, Z](k0: Frames[Row[F], A0, B0, S0, T0],
                                                             m0: Stack[F, B0, S0, R, S00, Z]): Linked[F, A0, T0, R, S00, Z] =
    new Linked[F, A0, T0, R, S00, Z]:
      type B = B0
      type S = S0
      def k = k0
      def m = m0

  private[freer] def next[F[_, _, +_], A0, B0, S0, T0, R, S00, Z](c0: Freer[Row[F], T0, R, A0], k0: Frames[Row[F], A0, B0, S0, T0],
                                                                  m0: Stack[F, B0, S0, R, S00, Z]): Next[F, R, S00, Z] =
    new Next[F, R, S00, Z]:
      type A = A0
      type B = B0
      type S = S0
      type T = T0
      def c = c0
      def k = k0
      def m = m0
