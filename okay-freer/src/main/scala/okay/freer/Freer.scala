package okay.freer

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

/** a level of the stack: its delimiter's ROW, exact, and its ANSWER PAIR — `(A => S) => R`: with a continuation
 * answering `S`, the level answers `R`. The head of the stack is the nearest level's; the top of a run is a level */
final class Lvl[H[+_], S, R]

/** an entry of the context a program is WRITTEN in: the delimiter's row and the answer its body is written at */
final class At[H[+_], S]
/** the context a body is WRITTEN in: `reset` gives its body one, `shift0` reads the head and gives its own body
 * the one outside. `C` is the delimiters in force, lexically; `H2`, `U`, `Σi` the level OUTSIDE the delimiter as
 * the index sees it — the body is written at them as abstract members, since an expected type does not reach a
 * method's receiver and a body's own index cannot say what is outside it; `reset` makes them its own parameters.
 * For inference alone; the machine never sees it */
sealed trait In[C <: Tuple]:
  type H2[+_]
  type U
  type Σi <: Tuple
  /** the level outside, as the index */
  type Out = Lvl[H2, U, U] *: Σi
  /** a program in the body this context is for, at the delimiter's row `H` and answer `S`, its answer unchanged */
  type Body[H[+_], S, A] = Freer[H, Lvl[H, S, S] *: Out, A]
  /** the context outside: for a shift's body, which runs there */
  def outer: In[?]
object In:
  /** the top: no delimiter in force, nothing outside */
  given top: In[EmptyTuple] with
    type H2[+A] = Nothing
    type U = Nothing
    type Σi = EmptyTuple
    def outer: In[?] = this

/**
 * THE BASIS, THE INDEX ONE STACK (specs/freer-min.md, stage 16): the freer monad with shift0 and reset as nodes of
 * the same tree. `Freer[G, Σ, A]` is a program of value `A` over the row `G` whose index is the stack of the levels
 * in force, each its row and its answer pair; `Bind` composes the head's pairs end to end, as states.
 *
 *  - `G` is what the program may do: a unary row, a union of signatures, built by `flatMap`, never declared;
 *  - `Σ` is never empty: the run is a level. Its head is the nearest delimiter's row — EXACT (invariant), so the
 *    row of the delimiter a capture reaches is the index, and nothing is named or compared — and the answer pair
 *    of that level; the levels below are diagonal through a shift's body (answer-type modification is the nearest
 *    level's; across a delimiter it needs the CPS hierarchy, not in the basis);
 *  - `A` is the value.
 *
 * A delimiter's VALUE IS ITS FINAL ANSWER: `reset body` where `body : S` at `Lvl[H, S, R]` answers `R`, at the level
 * outside, whose pair it leaves as it is. A capture goes to the NEAREST delimiter, the head of the index. Its `k`
 * is PURE, a program at the level outside, which it brings along, delivering the answer at the hole, `T`. The body
 * of a shift runs OUTSIDE the delimiter, in its place, at the level outside (shift0: the piece's frames are typed
 * at their levels, and an index that is exact pins them there), and its value is the delimiter's answer `R`.
 * `Return` and `Inject` keep the head's pair, as they must: a node claiming a move it does not make would be
 * believed by `reset`. Seven nodes, no cast, no prompt.
 */
enum Freer[+G[+_], Σ <: NonEmptyTuple, +A]:
  case Return[H[+_], R, Σ <: Tuple, A](a: A) extends Freer[Pure, Lvl[H, R, R] *: Σ, A]
  /** an operation: on the diagonal, so a handler recovers the middle of a matched `Bind` from the node */
  case Inject[G[+_], H[+_], T, Σ <: Tuple, A](op: G[A]) extends Freer[G, Lvl[H, T, T] *: Σ, A]
  case Bind[G[+_], H[+_], Σ <: Tuple, S, T, R, A, B](m: Freer[G, Lvl[H, T, R] *: Σ, A], k: A => Freer[G, Lvl[H, S, T] *: Σ, B])
    extends Freer[G, Lvl[H, S, R] *: Σ, B]
  case Delay[G[+_], Σ <: NonEmptyTuple, A](t: () => Freer[G, Σ, A]) extends Freer[G, Σ, A]
  /** `reset body`: the body's value is its initial answer `S`, at its own level; the delimiter answers `R` at the
   * level outside, at any `U`, which it leaves */
  case Reset[H[+_], H2[+_], Σ <: Tuple, S, R, U](body: Freer[H, Lvl[H, S, R] *: Lvl[H2, U, U] *: Σ, S])
    extends Freer[H, Lvl[H2, U, U] *: Σ, R]
  /** `shift0 (k => e)`: `k` is the context up to the nearest delimiter, it included, pure, a program at the level
   * outside, delivering the answer at the hole, `T`; `e` runs in the delimiter's place, at that level, and its
   * value is the delimiter's answer `R` */
  case Shift0[H[+_], H2[+_], Σ <: Tuple, T, R, X, U](f: (X => Freer[H, Lvl[H2, U, U] *: Σ, T]) => Freer[H, Lvl[H2, U, U] *: Σ, R])
    extends Freer[H, Lvl[H, T, R] *: Lvl[H2, U, U] *: Σ, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[H[+_], X, T, H2[+_], Σ <: Tuple, U](x: X, k: Captured[H, X, T, ?, H2, Σ, U]) extends Freer[H, Lvl[H2, U, U] *: Σ, T]

extension [G[+_], H[+_], Σ <: Tuple, T, R, A](m: Freer[G, Lvl[H, T, R] *: Σ, A])
  /** the head's pairs composed end to end: `m` from `T` to `R`, `f`'s from `S` to `T` */
  def flatMap[G2[+_], S, B](f: A => Freer[G2, Lvl[H, S, T] *: Σ, B]): Freer[G + G2, Lvl[H, S, R] *: Σ, B] = Freer.Bind(m, f)
  def map[B](f: A => B): Freer[G, Lvl[H, T, R] *: Σ, B] = Freer.Bind(m, a => Freer.Return(f(a)))

/** a program at the top, the run's level alone: its answer is its value, Danvy–Filinski's `⟨e⟩ : τ` */
type Top[G[+_], A] = Freer[G, Lvl[Pure, A, A] *: EmptyTuple, A]
/** the context of a fragment written for the body of a `reset[H, S]` at the top: `def f(using in: Under[H, S]): in.Body[H, S, A]` */
type Under[H[+_], S] = In[At[H, S] *: EmptyTuple]

def pure[A, H[+_], R, Σ <: Tuple](a: A): Freer[Pure, Lvl[H, R, R] *: Σ, A] = Freer.Return(a)
def inject[F[+_], H[+_], T, Σ <: Tuple, A](op: F[A]): Freer[F, Lvl[H, T, T] *: Σ, A] = Freer.Inject(op)
def delay[G[+_], Σ <: NonEmptyTuple, A](t: => Freer[G, Σ, A]): Freer[G, Σ, A] = Freer.Delay(() => t)
def defer[G[+_], H[+_], Σ <: Tuple, S, T, R, A, B](t: => Freer[G, Lvl[H, T, R] *: Σ, A])(f: A => Freer[G, Lvl[H, S, T] *: Σ, B]): Freer[G, Lvl[H, S, R] *: Σ, B] =
  Freer.Bind(Freer.Delay(() => t), f)
/** `reset[H, S](body)`: the body is written at the row `H` and the answer `S`, Danvy–Filinski's annotation of the
 * delimiter (a type in a context function's parameter is fixed before its body is typed, so both are named), in the
 * context where the reset is written, with the delimiter on top */
def reset[H[+_], S]: ResetAt[H, S] = ResetAt[H, S]()
final class ResetAt[H[+_], S]:
  def apply[C <: Tuple, H0[+_], Σ0 <: Tuple, R, U0](using o: In[C])
           (body: (in: In[At[H, S] *: C]) ?=> Freer[H, Lvl[H, S, R] *: in.Out, S]): Freer[H, Lvl[H0, U0, U0] *: Σ0, R] =
    val in = new In[At[H, S] *: C]:
      type H2[+A] = H0[A]
      type U = U0
      type Σi = Σ0
      def outer: In[?] = o
    Freer.Reset[H, H0, Σ0, S, R, U0](body(using in))
/** `shift0[X](k => …)` written in a reset's body: the hole `X` is the one thing nothing else says; the delimiter is
 * the head of the context, its answer the one the body is written at. The shift's body is written inside the
 * delimiter but runs outside it, so it sees the context under it */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[+_], S, C <: Tuple, R](using in: In[At[H, S] *: C])
           (f: In[?] ?=> (X => Freer[H, in.Out, S]) => Freer[H, in.Out, R]): Freer[H, Lvl[H, S, R] *: in.Out, X] =
    Freer.Shift0[H, in.H2, in.Σi, S, R, X, in.U](f(using in.outer))
