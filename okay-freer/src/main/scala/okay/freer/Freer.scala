package okay.freer

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

/** the delimiter in force: its ROW, exact — the one index that is. `EmptyTuple` where there is none */
final class Lvl[H[+_]]

/** the delimiter in force where a program is WRITTEN, and the answer `S` its body is written at: `reset` gives
 * its body one, `shift` reads both from it — an expected type does not reach a method's receiver, so a shift learns
 * its delimiter lexically. For inference alone; the machine never sees it */
final class In[+D, +S]
given top: In[EmptyTuple, Nothing] = In()

/**
 * THE MINIMAL BASIS (specs/freer-min.md): the freer monad with Danvy–Filinski's shift and reset as nodes of the
 * same tree, typed as they typed them: `Freer[G, D, S, R, A]` is `(A => S) => R` — a program of value `A` whose
 * evaluation changes the answer type from `S` to `R` — under the delimiter `D`.
 *
 *  - `G` is what the program may do: a unary row, a union of signatures, built by `flatMap`, never declared;
 *  - `D` is the delimiter in force, by its row: `Lvl[H]`, or `EmptyTuple` at the top. It is EXACT (invariant), and
 *    that is its one job: the row of the delimiter a capture reaches is the index, so `k` — the context's frames,
 *    typed at the context's row — is typed by the index, and nothing is named or compared. Only the NEAREST
 *    delimiter is in the index: nothing in the basis reaches past it, and a captured `k` brings its delimiter
 *    along, so it is a program under any `D` — the stack of all delimiters in force would pin it to the one it
 *    was captured under, for nothing;
 *  - `S`, `R` are the answer types. `Return` and `Inject` keep them (`A [S, S]`), `Bind` composes them end to end,
 *    `Shift` moves them: it is the one node that does;
 *  - `A` is the value.
 *
 * A delimiter's VALUE IS ITS FINAL ANSWER: `reset body` where `body : S [S, R]` under `Lvl[H]` answers `R` under
 * any `D`, at any answer type outside. A capture goes to the NEAREST delimiter, the index. Its `k : X => T [U, U]`,
 * for every `U` and under every `D`, is PURE — Danvy–Filinski's `τ/t → α/t` — and delivers the answer at the hole,
 * `T`; a polymorphic function, so a body may bind it at any answer, anywhere. The
 * body of a shift runs INSIDE the delimiter put back (shift, not shift0), with a value and initial answer of its
 * own, `V`, and the final answer `R` of the context it replaces. A capture across a delimiter, with answer types
 * that move, needs the stack of answer types too (the CPS hierarchy), and that is not in the basis. Seven nodes,
 * no cast, no prompt.
 */
enum Freer[+G[+_], D, S, R, +A]:
  case Return[D, R, A](a: A) extends Freer[Pure, D, R, R, A]
  /** an operation: on the diagonal, so a handler recovers the middle index of a matched `Bind` from the node */
  case Inject[G[+_], D, T, A](op: G[A]) extends Freer[G, D, T, T, A]
  case Bind[G[+_], D, S, T, R, A, B](m: Freer[G, D, T, R, A], k: A => Freer[G, D, S, T, B]) extends Freer[G, D, S, R, B]
  case Delay[G[+_], D, S, R, A](t: () => Freer[G, D, S, R, A]) extends Freer[G, D, S, R, A]
  /** `reset body`: the body's value is its initial answer `S`, under the delimiter; the delimiter answers `R`, at
   * any `U` outside, under any `D` */
  case Reset[H[+_], D, S, R, U](body: Freer[H, Lvl[H], S, R, S]) extends Freer[H, D, U, U, R]
  /** `shift (k => e)`: `k` is the context up to the nearest delimiter, it included, pure, a program under any
   * delimiter, and delivers the answer at the hole, `T`; `e` goes on inside the delimiter put back, answering `R` */
  case Shift[H[+_], T, R, X, V](f: ([U, E] => X => Freer[H, E, U, U, T]) => Freer[H, Lvl[H], V, R, V]) extends Freer[H, Lvl[H], T, R, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[H[+_], X, T, D, U](x: X, k: Captured[H, X, T, ?]) extends Freer[H, D, U, U, T]

  def flatMap[H[+_], S2, B](f: A => Freer[H, D, S2, S, B]): Freer[G + H, D, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, D, S, R, B] = Bind(this, a => Return(f(a)))

def pure[A, D, R](a: A): Freer[Pure, D, R, R, A] = Freer.Return(a)
def inject[F[+_], D, T, A](op: F[A]): Freer[F, D, T, T, A] = Freer.Inject(op)
def delay[G[+_], D, S, R, A](t: => Freer[G, D, S, R, A]): Freer[G, D, S, R, A] = Freer.Delay(() => t)
def defer[G[+_], D, S, T, R, A, B](t: => Freer[G, D, T, R, A])(f: A => Freer[G, D, S, T, B]): Freer[G, D, S, R, B] =
  Freer.Bind(Freer.Delay(() => t), f)
/** `reset[H, S](body)`: the body is written at the row `H` and the answer `S`, Danvy–Filinski's annotation of the
 * delimiter (a type in a context function's parameter is fixed before its body is typed, so both are named), with
 * the delimiter in force */
def reset[H[+_], S]: ResetAt[H, S] = ResetAt[H, S]()
final class ResetAt[H[+_], S]:
  def apply[D, R, U](body: In[Lvl[H], S] ?=> Freer[H, Lvl[H], S, R, S]): Freer[H, D, U, U, R] = Freer.Reset(body(using In()))
/** `shift[X](k => …)` written in a reset's body: the hole `X` is the one thing nothing else says; the delimiter is
 * the one in force, its answer the one the body is written at. A shift written in a shift's body is the node
 * `Shift`, its `T` named */
def shift[X]: ShiftAt[X] = ShiftAt[X]()
final class ShiftAt[X]:
  def apply[H[+_], S, R, V](using In[Lvl[H], S])(f: ([U, E] => X => Freer[H, E, U, U, S]) => Freer[H, Lvl[H], V, R, V]): Freer[H, Lvl[H], S, R, X] =
    Freer.Shift[H, S, R, X, V](f)
