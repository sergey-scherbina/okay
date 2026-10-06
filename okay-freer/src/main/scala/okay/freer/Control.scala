package okay.freer

/** a delimiter's identity: by allocation, compared by `eq`; `S` its answer index, `Y` its value; labelled for
 * diagnostics */
final class Prompt[S, Y](val label: String):
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
  case Reset[F[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], ret: X => Freer[Row[F], S, T, Y], body: Freer[Row[F], T, R, X])
    extends Control[F, S, R, Y]

  /** capture up to `p`, the delimiter included: `k` is the segment as a function, `ret`'s shape; `f(k)` goes on in
   * the delimiter's place */
  case Shift0[F[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], f: (X => Freer[Row[F], S, T, Y]) => Freer[Row[F], S, R, Y])
    extends Control[F, T, R, X]

/** `Return(_) $_p body` */
def reset[F[_, _, +_], S, R, Y](p: Prompt[S, Y])(body: Freer[Row[F], S, R, Y]): Freer[Row[F], S, R, Y] =
  Freer.Perform(Control.Reset[F, S, S, R, Y, Y](p, y => Freer.Return(y), body))

/** `ret $_p body` */
def dollar[F[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y])(ret: X => Freer[Row[F], S, T, Y])(body: Freer[Row[F], T, R, X])
  : Freer[Row[F], S, R, Y] =
  Freer.Perform(Control.Reset(p, ret, body))

/** capture to `p`'s delimiter, it included */
def shift0[F[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y])(f: (X => Freer[Row[F], S, T, Y]) => Freer[Row[F], S, R, Y])
  : Freer[Row[F], T, R, X] =
  Freer.Perform(Control.Shift0(p, f))

/** a capture naming a prompt with no delimiter on the machine running it */
final class NoPrompt(val wanted: String) extends RuntimeException(s"no delimiter of the prompt '$wanted' on this machine")
