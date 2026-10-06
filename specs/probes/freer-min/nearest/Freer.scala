package okay.freer

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

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
 * outside. A capture goes to the NEAREST delimiter, by definition: there is no prompt, nothing to name, nothing to
 * miss. Its `k : X => T [U, U]`, for every `U`, is PURE — Danvy–Filinski's `τ/t → α/t` — and delivers the answer
 * at the hole, `T`; it is a polymorphic function, so a body may bind it at any answer. `k` is the context's frames,
 * so it is typed at the context's ROW, which the shift does not know: the shift's body is written FOR EVERY row
 * `G` above its own `H`, and the machine instantiates it at the row of the delimiter it meets. The body runs
 * INSIDE the delimiter put back (shift, not shift0), with a value and initial answer of its own, `V`, and the
 * final answer `R` of the context it replaces. A capture across a delimiter, with answer types that move, needs the
 * stack of answer types (the CPS hierarchy), and that is not in the basis. Seven nodes, no cast, no prompt.
 */
enum Freer[+G[+_], S, R, +A]:
  case Return[R, A](a: A) extends Freer[Pure, R, R, A]
  /** an operation: on the diagonal, so a handler recovers the middle index of a matched `Bind` from the node */
  case Inject[G[+_], T, A](op: G[A]) extends Freer[G, T, T, A]
  case Bind[G[+_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]
  case Delay[G[+_], S, R, A](t: () => Freer[G, S, R, A]) extends Freer[G, S, R, A]
  /** `reset body`: the body's value is its initial answer `S`; the delimiter answers `R`, at any `U` outside */
  case Reset[H[+_], S, R, U](body: Freer[H, S, R, S]) extends Freer[H, U, U, R]
  /** `shift (k => e)`: `k` is the context up to the nearest delimiter, it included, pure, and delivers the answer at
   * the hole, `T`; `e` goes on inside the delimiter put back, answering `R` in the end — at the delimiter's row `G`,
   * whatever it is, above the body's own `H` */
  case Shift[H[+_], T, R, X, V](f: (k: Continue[H, X, T]) => Freer[k.Row, V, R, V]) extends Freer[H, T, R, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[H[+_], X, T, U](x: X, k: Captured[H, X, T, ?]) extends Freer[H, U, U, T]

  def flatMap[H[+_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))

/** the pure `k` a shift's body receives: `X => T [U, U]` for every `U`, at the ROW of the delimiter the capture
 * reached, `Row` — which the body knows only as lying above its own `H`, and writes its result at: a path-dependent
 * row, so the body is an ordinary lambda, for every row at once */
trait Continue[H[+_], X, T]:
  type Row[+A] >: H[A]
  def apply[U](x: X): Freer[Row, U, U, T]

def pure[A, R](a: A): Freer[Pure, R, R, A] = Freer.Return(a)
def inject[F[+_], T, A](op: F[A]): Freer[F, T, T, A] = Freer.Inject(op)
def delay[G[+_], S, R, A](t: => Freer[G, S, R, A]): Freer[G, S, R, A] = Freer.Delay(() => t)
def defer[G[+_], S, T, R, A, B](t: => Freer[G, T, R, A])(f: A => Freer[G, S, T, B]): Freer[G, S, R, B] =
  Freer.Bind(Freer.Delay(() => t), f)
def reset[H[+_], S, R, U](body: Freer[H, S, R, S]): Freer[H, U, U, R] = Freer.Reset(body)
/** `shift[X, T](k => …)`: the hole's type `X` and its answer `T` are the shift's own index, Danvy–Filinski's
 * annotation; nothing else says them, as an expected type does not reach a method's receiver. The body is written
 * at `k.Row`; one with operations of its own names them: `shift[X, T].in[H](k => …)` */
def shift[X, T]: ShiftAt[Pure, X, T] = ShiftAt[Pure, X, T]()
final class ShiftAt[H[+_], X, T]:
  def in[H2[+_]]: ShiftAt[H2, X, T] = ShiftAt[H2, X, T]()
  def apply[R, V](f: (k: Continue[H, X, T]) => Freer[k.Row, V, R, V]): Freer[H, T, R, X] = Freer.Shift(f)
