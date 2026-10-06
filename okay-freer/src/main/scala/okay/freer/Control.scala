package okay.freer

/**
 * A delimiter's identity: by allocation, compared by `eq`; labelled for diagnostics. It NAMES THE ROW `F` its
 * delimiters are written over, `S` its answer index, `Y` its value: everything a `reset` or a `shift0` needs that
 * the body cannot say — a body's row does not solve `F` from `Row[F]` (a union gives nothing to solve from,
 * specs/freer-min.md Results 7 and 9), and a prompt is made once, so it is said once.
 */
final class Prompt[F[_, _, +_], S, Y](val label: String):
  override def toString: String = label

/** the row a program with control over `F` is written in: `Control`'s operations beside `F`'s, CLOSED under
 * nesting — a delimiter's body is over the same row, so `Machine.run` takes every delimiter off at once */
type Row[F[_, _, +_]] = Ctl[F] + F

/** `Control` over a base row, as a row */
type Ctl[F[_, _, +_]] = [S, R, A] =>> Control[F, S, R, A]

/**
 * The control signature over the base row `F`: λ$'s two operations (Materzok–Biernacki, shift0 and $), typed by
 * Danvy–Filinski's answer-type modification — the typing of specs/freer-kont.md. Prompts are NOT in the tree:
 * these are operations like any other, and the machine (Machine.scala) is their handler. A captured `k` is an
 * ordinary function; the machine's stack implements it.
 */
enum Control[F[_, _, +_], S, R, +A]:
  /** `ret $_p body`: delimit at `p`, `ret` the frame first above it (`reset p body = Return(_) $_p body`). Typed
   * as `Bind(body, ret)` is — the delimiter sits between the two */
  case Reset[F[_, _, +_], S, T, R, X, Y](p: Prompt[F, S, Y], ret: X => Freer[Row[F], S, T, Y], body: Freer[Row[F], T, R, X])
    extends Control[F, S, R, Y]

  /** capture up to `p`, the delimiter included: `k` is the segment as a function, `ret`'s shape; `f(k)` goes on in
   * the delimiter's place */
  case Shift0[F[_, _, +_], S, T, R, X, Y](p: Prompt[F, S, Y], f: (X => Freer[Row[F], S, T, Y]) => Freer[Row[F], S, R, Y])
    extends Control[F, T, R, X]

/**
 * `Return(_) $_p body` at the prompt's index. The body's row `G` is inferred BOTTOM-UP — no expected row reaches
 * a `for` written in place, so `flatMap` joins what the steps are — and its membership in the prompt's row is an
 * EVIDENCE, a `<:<` with no type variable in it, which is also the widening. Pushed down as an expected type, a
 * union row makes the compiler commit `flatMap`'s `H` to the union's first member (Results 9). An index-moving
 * body (`T ≠ R`) is `Control.Reset` through `perform`, its indexes named.
 */
def reset[F[_, _, +_], S, Y, G[_, _, +_]](p: Prompt[F, S, Y])(body: Freer[G, S, S, Y])
                                         (using ev: Freer[G, S, S, Y] <:< Freer[Row[F], S, S, Y]): Freer[Row[F], S, S, Y] =
  Freer.Perform(Control.Reset[F, S, S, S, Y, Y](p, y => Freer.Return(y), ev(body)))

/** `shift0[X](p)(k => …)`: capture to `p`'s delimiter, it included, at the prompt's index. The hole `X` is the one
 * thing nothing else says, so it is said first; the body's row is inferred as `reset`'s is. An index-moving
 * capture is `Control.Shift0` through `perform` */
def shift0[X]: Shift0At[X] = Shift0At[X]()

final class Shift0At[X]:
  def apply[F[_, _, +_], S, Y, G[_, _, +_]](p: Prompt[F, S, Y])(f: (X => Freer[Row[F], S, S, Y]) => Freer[G, S, S, Y])
                                          (using ev: Freer[G, S, S, Y] <:< Freer[Row[F], S, S, Y]): Freer[Row[F], S, S, X] =
    Freer.Perform(Control.Shift0[F, S, S, S, X, Y](p, k => ev(f(k))))

/** a capture naming a prompt with no delimiter on the machine running it */
final class NoPrompt(val wanted: String) extends RuntimeException(s"no delimiter of the prompt '$wanted' on this machine")
