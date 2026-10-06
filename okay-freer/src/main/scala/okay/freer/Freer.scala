package okay.freer

/** the empty row */
type Pure = [S, R, A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]
/** a unary signature as a row: its operations stand at the diagonal */
type Diag[F[+_]] = [S, R, A] =>> F[A]

/** a delimiter's identity, by allocation (`eq`, and the type test `_: p.type` IS `eq`). It names the row `G` of
 * the context it delimits, its answer index `S` and its value `Y`: what a capture's `k` is typed at */
final class Prompt[G[_, _, +_], S, Y](val label: String):
  override def toString: String = label

/**
 * `(A => S) => R` over the row `G`, or `R => (S, A)`: the indexed freer monad, with the machine's three nodes in
 * it. `G` is covariant: a program over a row is a program over any wider row, and `flatMap` joins the rows.
 * A continuation's row is never the enum's `G` (it sits contravariantly in `Shift0`): it is the PROMPT's.
 */
enum Freer[+G[_, _, +_], S, R, +A]:
  case Return[R, A](a: A) extends Freer[Pure, R, R, A]
  case Inject[G[_, _, +_], T, A](op: G[T, T, A]) extends Freer[G, T, T, A]
  case Perform[G[_, _, +_], S, R, A](op: G[S, R, A]) extends Freer[G, S, R, A]
  case Bind[G[_, _, +_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]
  case Delay[G[_, _, +_], S, R, A](t: () => Freer[G, S, R, A]) extends Freer[G, S, R, A]
  /** `reset_p body`: the delimiter. `ret $_p body` is derived (`dollar`) */
  case Reset[H[_, _, +_], S, R, Y](p: Prompt[H, S, Y], body: Freer[H, S, R, Y]) extends Freer[H, S, R, Y]
  /** capture to `p`'s delimiter, it included; `f(k)` goes on in its place */
  case Shift0[H[_, _, +_], S, T, R, X, Y](p: Prompt[H, S, Y], f: (X => Freer[H, S, T, Y]) => Freer[H, S, R, Y])
    extends Freer[H, T, R, X]
  /** `k(x)` pending: the machine splices the captured piece onto its stack, the delimiter put back */
  case Resume[H[_, _, +_], X, S, T, Y](x: X, k: Captured[H, X, T, S, Y]) extends Freer[H, S, T, Y]

  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))


def pure[A, R](a: A): Freer[Pure, R, R, A] = Freer.Return(a)
def inject[F[+_], T, A](op: F[A]): Freer[Diag[F], T, T, A] = Freer.Inject[Diag[F], T, A](op)
def perform[G[_, _, +_], S, R, A](op: G[S, R, A]): Freer[G, S, R, A] = Freer.Perform(op)
def delay[G[_, _, +_], S, R, A](t: => Freer[G, S, R, A]): Freer[G, S, R, A] = Freer.Delay(() => t)
def defer[G[_, _, +_], S, T, R, A, B](t: => Freer[G, T, R, A])(f: A => Freer[G, S, T, B]): Freer[G, S, R, B] =
  Freer.Bind(Freer.Delay(() => t), f)
/** `Return(_) $_p body` at the prompt's index: everything but the body comes from the prompt. An index-moving
 * delimiter is the node `Reset`, its indexes named */
def reset[H[_, _, +_], S, Y](p: Prompt[H, S, Y])(body: Freer[H, S, S, Y]): Freer[H, S, S, Y] =
  Freer.Reset(p, body)

/** `ret $_p body` (λ$), DERIVED: the body's value leaves the delimiter by an abort and meets `ret` outside, so a
 * `k` captured inside carries `ret` with the delimiter — `v $ E[S0 k. e] → e[k := λx. v $ E[x]]` — and `f(k)`
 * never meets `ret` (`v $ w → v w` is the abort). `reset` is the primitive, as in Materzok–Biernacki's reverse */
def dollar[H[_, _, +_], S, T, R, X, Y](p: Prompt[H, S, Y])(ret: X => Freer[H, S, T, Y])(body: Freer[H, T, R, X]): Freer[H, S, R, Y] =
  Freer.Reset(p, Freer.Bind(body, (x: X) => Freer.Shift0[H, S, S, T, Y, Y](p, _ => ret(x))))
/** `shift0[X](p)(k => …)` at the prompt's index: the hole `X` is the one thing nothing else says. An
 * index-moving capture is the node `Shift0`, its indexes named */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[_, _, +_], S, Y](p: Prompt[H, S, Y])(f: (X => Freer[H, S, S, Y]) => Freer[H, S, S, Y]): Freer[H, S, S, X] =
    Freer.Shift0[H, S, S, S, X, Y](p, f)

final class NoPrompt(val wanted: String) extends RuntimeException(s"no delimiter of the prompt '$wanted' on this machine")
