package okay.freer

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

/** an entry of the delimiter stack: the delimiter's ROW, exact. The head of the stack is the nearest delimiter's */
final class Lvl[H[+_]]

/** the stack in force where a program is WRITTEN, and beside it the answers the delimiters' bodies are written at:
 * `reset` gives its body one, `shift0` reads the head from it and gives its own body the tail — an expected type
 * does not reach a method's receiver, so a shift learns its delimiter lexically. For inference alone; the machine
 * never sees it */
final class In[Σ <: Tuple, Ss <: Tuple]
given top: In[EmptyTuple, EmptyTuple] = In()

/**
 * THE BASIS WITH THE STACK OF ROWS (specs/freer-min.md, stage 14): the freer monad with shift0 and reset as nodes of
 * the same tree. `Freer[G, Σ, S, R, A]` is `(A => S) => R` — a program of value `A` whose evaluation changes the
 * answer type from `S` to `R` — under the delimiters `Σ`.
 *
 *  - `G` is what the program may do: a unary row, a union of signatures, built by `flatMap`, never declared;
 *  - `Σ` is the stack of the delimiters in force, each by its row, the nearest first. It is EXACT (invariant): the
 *    row of the delimiter a capture reaches is the head of the index, so `k` — the context's frames, typed at the
 *    context's row — is typed by the index, and nothing is named or compared;
 *  - `S`, `R` are the answer types of the NEAREST level. `Return` and `Inject` keep them, `Bind` composes them end
 *    to end, `Shift0` moves them: it is the one node that does;
 *  - `A` is the value.
 *
 * A delimiter's VALUE IS ITS FINAL ANSWER: `reset body` where `body : S [S, R]` under `Lvl[H] *: Σ` answers `R`
 * under `Σ`, at any answer type outside. A capture goes to the NEAREST delimiter, the head of the index. Its
 * `k : X => T [U, U]`, for every `U`, is PURE and is a program UNDER `Σ`, the stack outside the delimiter, which it
 * brings along: the piece's frames are typed at `Lvl[H] *: Σ`, and a tail that is exact pins them there — so the
 * body of a shift runs OUTSIDE the delimiter, under `Σ` too, in its place (shift0, not shift: a shift body that
 * used `k` inside the delimiter put back would run the frames one level deeper than their type), and its value is
 * the delimiter's answer `R`, the outer level's answer unchanged (`V`, `V`: answer-type modification is the nearest
 * level's; across a delimiter it needs the pairs of every level, the CPS hierarchy, and that is not in the basis).
 * Seven nodes, no cast, no prompt.
 */
enum Freer[+G[+_], Σ <: Tuple, S, R, +A]:
  case Return[Σ <: Tuple, R, A](a: A) extends Freer[Pure, Σ, R, R, A]
  /** an operation: on the diagonal, so a handler recovers the middle index of a matched `Bind` from the node */
  case Inject[G[+_], Σ <: Tuple, T, A](op: G[A]) extends Freer[G, Σ, T, T, A]
  case Bind[G[+_], Σ <: Tuple, S, T, R, A, B](m: Freer[G, Σ, T, R, A], k: A => Freer[G, Σ, S, T, B]) extends Freer[G, Σ, S, R, B]
  case Delay[G[+_], Σ <: Tuple, S, R, A](t: () => Freer[G, Σ, S, R, A]) extends Freer[G, Σ, S, R, A]
  /** `reset body`: the body's value is its initial answer `S`, under the delimiter; the delimiter answers `R`, at
   * any `U` outside */
  case Reset[H[+_], Σ <: Tuple, S, R, U](body: Freer[H, Lvl[H] *: Σ, S, R, S]) extends Freer[H, Σ, U, U, R]
  /** `shift0 (k => e)`: `k` is the context up to the nearest delimiter, it included, pure, a program under the
   * stack outside it, and delivers the answer at the hole, `T`; `e` runs in the delimiter's place, under that
   * stack, at the answer of the context it replaces, `k.Out` — which it knows as nothing else, so it is written for
   * every answer — and its value is the delimiter's answer `R` */
  case Shift0[H[+_], Σ <: Tuple, T, R, X](f: (k: Continue[H, Σ, X, T]) => Freer[H, Σ, k.Out, k.Out, R])
    extends Freer[H, Lvl[H] *: Σ, T, R, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[H[+_], X, T, Σ <: Tuple, U](x: X, k: Captured[H, X, T, ?, Σ]) extends Freer[H, Σ, U, U, T]

  def flatMap[H[+_], S2, B](f: A => Freer[H, Σ, S2, S, B]): Freer[G + H, Σ, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, Σ, S, R, B] = Bind(this, a => Return(f(a)))

/** the pure `k` a shift0's body receives: `X => T [U, U]` for every `U`, under the stack outside its delimiter.
 * `Out` is the answer of the context the body replaces: the body is written at it, and knows it as nothing else */
trait Continue[H[+_], Σ <: Tuple, X, T]:
  type Out
  def apply[U](x: X): Freer[H, Σ, U, U, T]

def pure[A, Σ <: Tuple, R](a: A): Freer[Pure, Σ, R, R, A] = Freer.Return(a)
def inject[F[+_], Σ <: Tuple, T, A](op: F[A]): Freer[F, Σ, T, T, A] = Freer.Inject(op)
def delay[G[+_], Σ <: Tuple, S, R, A](t: => Freer[G, Σ, S, R, A]): Freer[G, Σ, S, R, A] = Freer.Delay(() => t)
def defer[G[+_], Σ <: Tuple, S, T, R, A, B](t: => Freer[G, Σ, T, R, A])(f: A => Freer[G, Σ, S, T, B]): Freer[G, Σ, S, R, B] =
  Freer.Bind(Freer.Delay(() => t), f)
/** `reset[H, S](body)`: the body is written at the row `H` and the answer `S`, Danvy–Filinski's annotation of the
 * delimiter (a type in a context function's parameter is fixed before its body is typed, so both are named), under
 * the stack in force where the reset is written, with the delimiter on top */
def reset[H[+_], S]: ResetAt[H, S] = ResetAt[H, S]()
final class ResetAt[H[+_], S]:
  def apply[Σ <: Tuple, Ss <: Tuple, R, U](using In[Σ, Ss])(body: In[Lvl[H] *: Σ, S *: Ss] ?=> Freer[H, Lvl[H] *: Σ, S, R, S]): Freer[H, Σ, U, U, R] =
    Freer.Reset(body(using In()))
/** `shift0[X](k => …)` written in a reset's body: the hole `X` is the one thing nothing else says; the delimiter is
 * the head of the stack in force, its answer the one the body is written at. The shift's body is written inside
 * the delimiter but runs outside it, so it sees the stack under it */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[+_], Σ <: Tuple, S, Ss <: Tuple, R](using In[Lvl[H] *: Σ, S *: Ss])
           (f: In[Σ, Ss] ?=> (k: Continue[H, Σ, X, S]) => Freer[H, Σ, k.Out, k.Out, R]): Freer[H, Lvl[H] *: Σ, S, R, X] =
    Freer.Shift0[H, Σ, S, R, X](f(using In()))
