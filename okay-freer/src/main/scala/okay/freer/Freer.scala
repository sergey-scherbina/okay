package okay.freer

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

/** a delimiter's identity, by allocation (`eq`, and the type test `_: p.type` IS `eq`). It names the ROW `H` of the
 * context it delimits — `k` is the context's frames, so it is typed at the context's row, and nothing but the
 * delimiter's identity ties the two — and the answer `S` its body is WRITTEN at, which is what a capture in that
 * body delivers. `S` is for inference alone: an expected type does not reach a method's receiver, so a shift cannot
 * learn the answer at its hole from its context. The machine does not tie the delimiter to it: after a shift, the
 * delimiter is put back at the shift body's own answer — Gunter, Rémy and Riecke's prompt pins that, and loses
 * answer-type modification; this one does not */
final class Prompt[H[+_], S](val label: String):
  override def toString: String = label

/**
 * THE MINIMAL BASIS (specs/freer-min.md): the freer monad with Danvy–Filinski's shift and reset as nodes of the
 * same tree, typed as they typed them: `Freer[G, S, R, A]` is `(A => S) => R` — a program of value `A` whose
 * evaluation changes the answer type from `S` to `R`.
 *
 *  - `G` is what the program may do: a unary row, a union of signatures, built by `flatMap`, never declared;
 *  - `S`, `R` are the answer types. `Return` and `Inject` keep them (`A [S, S]`), `Bind` composes them end to end,
 *    `Shift` moves them: it is the one node that does;
 *  - `A` is the value.
 *
 * A delimiter's VALUE IS ITS FINAL ANSWER: `reset body` where `body : S [S, R]` answers `R`, at any answer type
 * outside. A capture's `k : X => T [U, U]`, for every `U`, is PURE — Danvy–Filinski's `τ/t → α/t` — and delivers the
 * answer at the hole, `T`; it is a polymorphic function, so a body may bind it at any answer, and it mentions
 * nothing of its delimiter but the row. The body of a shift runs INSIDE the delimiter put back (shift, not shift0),
 * with a value and initial answer of its own, `V`, and the final answer `R` of the context it replaces. A capture
 * goes to the NEAREST delimiter, which must be its prompt's: with answer types that move, a capture across another
 * delimiter needs the stack of answer types (the CPS hierarchy), and that is not in the basis. Seven nodes, no cast.
 */
enum Freer[+G[+_], S, R, +A]:
  case Return[R, A](a: A) extends Freer[Pure, R, R, A]
  /** an operation: on the diagonal, so a handler recovers the middle index of a matched `Bind` from the node */
  case Inject[G[+_], T, A](op: G[A]) extends Freer[G, T, T, A]
  case Bind[G[+_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]
  case Delay[G[+_], S, R, A](t: () => Freer[G, S, R, A]) extends Freer[G, S, R, A]
  /** `reset_p body`: the body's value is its initial answer `S`; the delimiter answers `R`, at any `U` outside */
  case Reset[H[+_], S, R, U](p: Prompt[H, S], body: Freer[H, S, R, S]) extends Freer[H, U, U, R]
  /** `shift_p (k => e)`: `k` is the context up to `p`'s delimiter, it included, pure, and delivers the answer at the
   * hole, `T`; `e` goes on inside the delimiter put back, answering `R` in the end. `T` is the prompt's `S` for a
   * shift written in the delimiter's body, and the shift body's own `V` for one written in a shift's body */
  case Shift[H[+_], Sp, T, R, X, V](p: Prompt[H, Sp], f: ([U] => X => Freer[H, U, U, T]) => Freer[H, V, R, V]) extends Freer[H, T, R, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[H[+_], X, T, U](x: X, k: Captured[H, X, T, ?]) extends Freer[H, U, U, T]

  def flatMap[H[+_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))

def pure[A, R](a: A): Freer[Pure, R, R, A] = Freer.Return(a)
def inject[F[+_], T, A](op: F[A]): Freer[F, T, T, A] = Freer.Inject(op)
def delay[G[+_], S, R, A](t: => Freer[G, S, R, A]): Freer[G, S, R, A] = Freer.Delay(() => t)
def defer[G[+_], S, T, R, A, B](t: => Freer[G, T, R, A])(f: A => Freer[G, S, T, B]): Freer[G, S, R, B] =
  Freer.Bind(Freer.Delay(() => t), f)
/** `reset_p body`: the body is written at the prompt's answer `S` */
def reset[H[+_], S, R, U](p: Prompt[H, S])(body: Freer[H, S, R, S]): Freer[H, U, U, R] = Freer.Reset(p, body)
/** `shift[X](p)(k => …)` written in `p`'s body: the hole `X` is the one thing nothing else says. A shift written in
 * a shift's body is the node `Shift`, its `T` named */
def shift[X]: ShiftAt[X] = ShiftAt[X]()
final class ShiftAt[X]:
  def apply[H[+_], S, R, V](p: Prompt[H, S])(f: ([U] => X => Freer[H, U, U, S]) => Freer[H, V, R, V]): Freer[H, S, R, X] =
    Freer.Shift[H, S, S, R, X, V](p, f)

/** a capture whose nearest delimiter is another prompt's: not in the basis (see `Freer`) */
final class NotNearest(val wanted: String, val found: String)
  extends RuntimeException(s"the capture to '$wanted' met the delimiter of '$found' first; a capture across a delimiter is not in the basis")
