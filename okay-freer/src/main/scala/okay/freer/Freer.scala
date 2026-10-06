package okay.freer

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

/** a level of a stack: its delimiter's ROW, exact; the stacks OUTSIDE its delimiter, `D` — where the delimiter's
 * continuation lives, so where a capture to it is a program — constant along the level, as the row is; and its
 * ANSWER at this point. The head of a stack is the nearest level's; the top of a run is a level */
final class At[H[+_], D <: Tuple, S]

/**
 * THE HIERARCHY (specs/freer-min.md, stage 17): the freer monad with shift0 and reset as nodes of the same tree,
 * typed by TWO STACKS of answer types, a level each — Danvy–Filinski's `(A => S) => R` with `S` and `R` grown into
 * the stacks `I` and `O`: `Freer[G, I, O, A]` is a program of value `A` over the row `G` which, with a continuation
 * answering `I`, answers `O`. `Bind` composes them end to end, as states, at every level at once; `Return` and
 * `Inject` keep them; `Reset` and `Shift0` move the head's.
 *
 *  - `G` is what the program may do: a unary row, a union of signatures, built by `flatMap`, never declared;
 *  - `I`, `O`: never empty, the run is a level. The head is the nearest delimiter's row — EXACT (invariant), so the
 *    row of the delimiter a capture reaches is the index, and nothing is named or compared — and its `D` and
 *    answer; the levels below are the context outside, which a shift's body, running there, may move: answer-type
 *    modification across delimiters, the CPS hierarchy;
 *  - `A` is the value.
 *
 * A delimiter's VALUE IS ITS FINAL ANSWER: `reset body` where `body : S` from `At[H, D, S] *: D` to `At[H, D, R] *: O`
 * answers `R` outside, from `D` to `O`, as the body moved the outside. A capture goes to the NEAREST delimiter, the
 * head of the index. Its `k` is PURE, a program outside the delimiter, from `D` to the stacks at the hole, `I`,
 * delivering the answer at the hole, `T`. The body of a shift runs OUTSIDE the delimiter, in its place (shift0),
 * from `D` to the stacks the context expects, `O`, and its value is the delimiter's answer `R`. Seven nodes, no
 * cast, no prompt, and no node that looks at the head but the three that move it.
 */
enum Freer[+G[+_], I <: Tuple, O <: Tuple, +A]:
  case Return[Σ <: Tuple, A](a: A) extends Freer[Pure, Σ, Σ, A]
  /** an operation: on the diagonal, so a handler recovers the middle of a matched `Bind` from the node */
  case Inject[G[+_], Σ <: Tuple, A](op: G[A]) extends Freer[G, Σ, Σ, A]
  case Bind[G[+_], I <: Tuple, T <: Tuple, O <: Tuple, A, B](m: Freer[G, T, O, A], k: A => Freer[G, I, T, B]) extends Freer[G, I, O, B]
  case Delay[G[+_], I <: Tuple, O <: Tuple, A](t: () => Freer[G, I, O, A]) extends Freer[G, I, O, A]
  /** `reset body`: the body's value is its initial answer `S`, at its own level, whose `D` is the stacks outside;
   * the delimiter answers `R` outside, from `D` to `O`, as the body moved the outside */
  case Reset[H[+_], D <: Tuple, O <: Tuple, S, R](body: Freer[H, At[H, D, S] *: D, At[H, D, R] *: O, S]) extends Freer[H, D, O, R]
  /** `shift0 (k => e)`: `k` is the context up to the nearest delimiter, it included, pure, a program outside it from
   * `D` to the stacks at the hole `I`, delivering the answer at the hole `T`; `e` runs in the delimiter's place,
   * from `D` to what the context expects, `O`, and its value is the delimiter's answer `R` */
  case Shift0[H[+_], D <: Tuple, I <: Tuple, O <: Tuple, T, R, X](f: (X => Freer[H, D, I, T]) => Freer[H, D, O, R])
    extends Freer[H, At[H, D, T] *: I, At[H, D, R] *: O, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[H[+_], D <: Tuple, I <: Tuple, X, T](x: X, k: Captured[H, D, I, X, T, ?]) extends Freer[H, D, I, T]

  /** the stacks composed end to end: `this` from `I` to `O`, `f`'s from `I2` to `I` */
  def flatMap[G2[+_], I2 <: Tuple, B](f: A => Freer[G2, I2, I, B]): Freer[G + G2, I2, O, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, I, O, B] = Bind(this, a => Return(f(a)))

/** the run's level: no delimiter, nothing outside, answering `A` */
type Run[A] = At[Pure, EmptyTuple, A] *: EmptyTuple
/** a program at the top, the run's level alone: its answer is its value, Danvy–Filinski's `⟨e⟩ : τ` */
type Top[G[+_], A] = Freer[G, Run[A], Run[A], A]

/** the context a body is WRITTEN in: `reset` gives its body one, `shift0` reads it and gives its own body the one
 * outside. `H`, `S` the row and the answer of the delimiter this context is the body of; `D` the stacks outside
 * that delimiter; `Here` the stacks at this position — what a reset written here has outside. STRUCTURAL: a nested
 * context's `D` is the outer's `Here`, down to the top, so a body knows the levels outside it and may move them
 * (the hierarchy). An expected type does not reach a method's receiver, which is why a shift learns its delimiter
 * lexically. For inference alone; the machine never sees it */
sealed trait In[H[+_], S, Oc]:
  type D <: Tuple
  type Here = At[H, D, S] *: D
  /** the context outside, by its own type — a shift's body is written in it */
  def outer: Oc
  /** a program in the body this context is for, its stacks unchanged */
  type Body[A] = Freer[H, Here, Here, A]
/** the context of a fragment written for the body of a `reset[H, S]`: `def f(using in: Under[H, S]): in.Body[A]` */
type Under[H[+_], S] = In[H, S, ?]
/** the run's context, the top: no delimiter in force, nothing outside, the run's answer `A` */
type Root[A] = In[Pure, A, Unit] { type D = EmptyTuple }
/** `top[A](program)`: a program at the top, the run's answer `A` named — a context is made before its body is
 * typed, and no expected type reaches a receiver, so nothing else could say it */
def top[A]: TopAt[A] = TopAt[A]()
final class TopAt[A]:
  def apply[G[+_]](p: Root[A] ?=> Top[G, A]): Top[G, A] = p(using root[A])
def root[A]: Root[A] = new In[Pure, A, Unit]:
  type D = EmptyTuple
  def outer: Unit = ()

def pure[A, Σ <: Tuple](a: A): Freer[Pure, Σ, Σ, A] = Freer.Return(a)
def inject[F[+_], Σ <: Tuple, A](op: F[A]): Freer[F, Σ, Σ, A] = Freer.Inject(op)
def delay[G[+_], I <: Tuple, O <: Tuple, A](t: => Freer[G, I, O, A]): Freer[G, I, O, A] = Freer.Delay(() => t)
def defer[G[+_], I <: Tuple, T <: Tuple, O <: Tuple, A, B](t: => Freer[G, T, O, A])(f: A => Freer[G, I, T, B]): Freer[G, I, O, B] =
  Freer.Bind(Freer.Delay(() => t), f)
/** `reset[H, S](body)`: the body is written at the row `H` and the answer `S`, Danvy–Filinski's annotation of the
 * delimiter (a type in a context function's parameter is fixed before its body is typed, so both are named), in the
 * context where the reset is written, with the delimiter on top */
def reset[H[+_], S]: ResetAt[H, S] = ResetAt[H, S]()
final class ResetAt[H[+_], S]:
  def apply[H2[+_], S2, Oc2, O <: Tuple, R](using o: In[H2, S2, Oc2])
           (body: (in: In[H, S, o.type] { type D = o.Here }) ?=> Freer[H, At[H, o.Here, S] *: o.Here, At[H, o.Here, R] *: O, S]): Freer[H, o.Here, O, R] =
    val in = new In[H, S, o.type]:
      type D = o.Here
      def outer: o.type = o
    Freer.Reset[H, o.Here, O, S, R](body(using in))
/** `shift0[X](k => …)` written in a reset's body: the hole `X` is the one thing nothing else says; the delimiter is
 * the head of the context, its answer the one the body is written at. The shift's body is written inside the
 * delimiter but runs outside it, so it sees the context outside, and may move it. `k` leaves the outside as it is,
 * `D` to `D` (a piece that moved it could not be resumed twice); the node is general */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[+_], S, Oc, O <: Tuple, R](using in: In[H, S, Oc])
           (f: Oc ?=> (X => Freer[H, in.D, in.D, S]) => Freer[H, in.D, O, R])
    : Freer[H, At[H, in.D, S] *: in.D, At[H, in.D, R] *: O, X] =
    Freer.Shift0[H, in.D, in.D, O, S, R, X](f(using in.outer))
