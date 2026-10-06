package okay.freer

/** the empty row: no operation at all; `Return`'s row, and so a program of every row */
type Pure = [A] =>> Nothing
/** the row join: a union, pointwise */
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

/** a level of a stack: the ROW of its delimiter's body `H`, exact; the row OUTSIDE its delimiter `Hf` — what the
 * delimiter leaves, so what a shift's body, running outside, may do: a handler's body is at `E + G` and its
 * outside at `G`, the effect discharged; the stacks OUTSIDE its delimiter `D` — where the delimiter's continuation
 * lives, so where a capture to it is a program — constant along the level, as the rows are; and its ANSWER at this
 * point. The head of a stack is the nearest level's */
final class At[H[+_], Hf[+_], D <: Tuple, S]

/**
 * THE HIERARCHY, HANDLERS AS DELIMITERS (specs/freer-min.md, stage 19): the freer monad with shift0 and reset as
 * nodes of the same tree, typed by TWO STACKS of answer types, a level each — Danvy–Filinski's `(A => S) => R` with
 * `S` and `R` grown into the stacks `I` and `O`: `Freer[G, I, O, A]` is a program of value `A` over the row `G`
 * which, with a continuation answering `I`, answers `O`. `Bind` composes them end to end, as states, at every
 * level at once; `Return` keeps them; `Reset` and `Shift0` move the head's.
 *
 *  - `G` is what the program may do: a unary row, a union of signatures, built by `flatMap`, never declared. EVERY
 *    operation is a capture to the delimiter that handles it — a handler is a delimiter, an operation a shift to
 *    it, the handler's clause the shift's body, outside; there is no other node for an operation, since one would
 *    go past the handler;
 *  - `I`, `O`: the top is empty. The head is the nearest delimiter's rows — EXACT (invariant), so the row of the
 *    delimiter a capture reaches, and the row its body may use outside, are the index, and nothing is named or
 *    compared — its `D` and answer; the levels below are the context outside, which a shift's body, running there,
 *    may move: answer-type modification across delimiters, the CPS hierarchy;
 *  - `A` is the value.
 *
 * A delimiter's VALUE IS ITS FINAL ANSWER: `reset body` where `body : S` from `At[H, Hf, D, S] *: D` to
 * `At[H, Hf, D, R] *: O` answers `R` outside, at the row `Hf`, from `D` to `O`, as the body moved the outside. A
 * capture goes to the NEAREST delimiter, the head of the index. Its `k` is PURE, a program outside the delimiter,
 * at `Hf`, from `D` to the stacks at the hole, `I`, delivering the answer at the hole, `T`. The body of a shift runs
 * OUTSIDE the delimiter, in its place (shift0), at `Hf`, from `D` to the stacks the context expects, `O`, and its
 * value is the delimiter's answer `R`. Six nodes, no cast, no prompt.
 */
enum Freer[+G[+_], I <: Tuple, O <: Tuple, +A]:
  case Return[Σ <: Tuple, A](a: A) extends Freer[Pure, Σ, Σ, A]
  case Bind[G[+_], I <: Tuple, T <: Tuple, O <: Tuple, A, B](m: Freer[G, T, O, A], k: A => Freer[G, I, T, B]) extends Freer[G, I, O, B]
  case Delay[G[+_], I <: Tuple, O <: Tuple, A](t: () => Freer[G, I, O, A]) extends Freer[G, I, O, A]
  /** `reset body`: the body's value is its initial answer `S`, at its own level; the delimiter answers `R` outside,
   * at the row it leaves, `Hf`, from `D` to `O`, as the body moved the outside */
  case Reset[H[+_], Hf[+_], D <: Tuple, O <: Tuple, S, R](body: Freer[H, At[H, Hf, D, S] *: D, At[H, Hf, D, R] *: O, S])
    extends Freer[Hf, D, O, R]
  /** `shift0 (k => e)`: `k` is the context up to the nearest delimiter, it included, pure, a program outside it at
   * `Hf`, from `D` to the stacks at the hole `I`, delivering the answer at the hole `T`; `e` runs in the delimiter's
   * place, at `Hf`, from `D` to what the context expects, `O`, and its value is the delimiter's answer `R` */
  case Shift0[H[+_], Hf[+_], D <: Tuple, I <: Tuple, O <: Tuple, T, R, X](f: (X => Freer[Hf, D, I, T]) => Freer[Hf, D, O, R])
    extends Freer[H, At[H, Hf, D, T] *: I, At[H, Hf, D, R] *: O, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[Hf[+_], D <: Tuple, I <: Tuple, X, T](x: X, k: Captured[?, Hf, D, I, X, T, ?]) extends Freer[Hf, D, I, T]

  /** the stacks composed end to end: `this` from `I` to `O`, `f`'s from `I2` to `I` */
  def flatMap[G2[+_], I2 <: Tuple, B](f: A => Freer[G2, I2, I, B]): Freer[G + G2, I2, O, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, I, O, B] = Bind(this, a => Return(f(a)))

/** a program at the top: no delimiter in force, nothing outside, the stacks empty; its answer is its value,
 * Danvy–Filinski's `⟨e⟩ : τ` */
type Top[G[+_], A] = Freer[G, EmptyTuple, EmptyTuple, A]

/** the context a program is WRITTEN in, by the stacks at its position, `Here`: what a reset written here has
 * outside. STRUCTURAL, down to the top, where there is nothing: so a body knows the levels outside it and may move
 * them (the hierarchy), and a handler is found by walking it (`perform`). An expected type does not reach a
 * method's receiver, which is why a shift learns its delimiter lexically. For inference alone; the machine never
 * sees it */
sealed trait Ctx:
  type Here <: Tuple
/** the top: nothing outside */
object Root extends Ctx:
  type Here = EmptyTuple
given Root.type = Root
/** the context of the BODY of a delimiter: its row `H`, the row it leaves outside `Hf`, the answer `S` the body is
 * written at, and the context outside by its own type, `Oc` — a shift's body is written in it; the stacks outside
 * are the outer context's `Here`. `reset` and `handle` give their body one, `shift0` and `perform` read it */
sealed trait In[H[+_], Hf[+_], S, Oc <: Ctx] extends Ctx:
  type Row[+A] = H[A]
  type Out[+A] = Hf[A]
  val outer: Oc
  type D = outer.Here
  type Here = At[H, Hf, D, S] *: D
  /** a program in the body this context is for, its stacks unchanged */
  type Body[A] = Freer[H, Here, Here, A]
/** the context of a fragment written for the body of a `reset[H, S]`: `def f(using in: Under[H, S]): in.Body[A]` */
type Under[H[+_], S] = In[H, H, S, ?]

def pure[A, Σ <: Tuple](a: A): Freer[Pure, Σ, Σ, A] = Freer.Return(a)
def delay[G[+_], I <: Tuple, O <: Tuple, A](t: => Freer[G, I, O, A]): Freer[G, I, O, A] = Freer.Delay(() => t)
def defer[G[+_], I <: Tuple, T <: Tuple, O <: Tuple, A, B](t: => Freer[G, T, O, A])(f: A => Freer[G, I, T, B]): Freer[G, I, O, B] =
  Freer.Bind(Freer.Delay(() => t), f)
/** `reset[H, S](body)`: the body is written at the row `H` and the answer `S`, Danvy–Filinski's annotation of the
 * delimiter (a type in a context function's parameter is fixed before its body is typed, so both are named), in the
 * context where the reset is written, with the delimiter on top; it leaves the row as it is */
def reset[H[+_], S]: ResetAt[H, S] = ResetAt[H, S]()
final class ResetAt[H[+_], S]:
  def apply[O <: Tuple, R](using o: Ctx)
           (body: In[H, H, S, o.type] ?=> Freer[H, At[H, H, o.Here, S] *: o.Here, At[H, H, o.Here, R] *: O, S]): Freer[H, o.Here, O, R] =
    val in = new In[H, H, S, o.type]:
      val outer: o.type = o
    Freer.Reset[H, H, o.Here, O, S, R](body(using in))
/** `shift0[X](k => …)` written in a delimiter's body: the hole `X` is the one thing nothing else says; the delimiter
 * is the context's, its answer the one the body is written at. The shift's body is written inside the delimiter
 * but runs outside it, at the row the delimiter leaves, in the context outside, and may move it. `k` leaves the
 * outside as it is, `D` to `D` (a piece that moved it could not be resumed twice); the node is general */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[+_], Hf[+_], S, Oc <: Ctx, O <: Tuple, R](using in: In[H, Hf, S, Oc])
           (f: Oc ?=> (X => Freer[Hf, in.D, in.D, S]) => Freer[Hf, in.D, O, R]): Freer[H, At[H, Hf, in.D, S] *: in.D, At[H, Hf, in.D, R] *: O, X] =
    Freer.Shift0[H, Hf, in.D, in.D, O, S, R, X](f(using in.outer))

/** A HANDLER IS A DELIMITER: of the effect `E`, leaving the row `G`; the body's value `A`, the answer `Ans`. A deep
 * handler: `k` brings the delimiter along, so the handler stays in force through a resumption. The clauses run
 * outside the delimiter, in the context outside, `o`, at any stacks there */
trait Handler[E[+_], G[+_], A, Ans]:
  def ret(a: A): Ans
  def apply[X, Oc <: Ctx](using o: Oc)(op: E[X], k: X => Freer[G, o.Here, o.Here, Ans]): Freer[G, o.Here, o.Here, Ans]
/** the context of a handler's body: it handles `E`, at the row `H = E + G` */
sealed trait Handling[E[+_], H[+_], G[+_], Ans, Oc <: Ctx] extends In[H, G, Ans, Oc]:
  def handler: Handler[E, Out, ?, Ans]
/** `handle(h)(body)`: the delimiter of the handler `h`, its body at `E + G`, the effect `E` discharged outside */
def handle[E[+_], G[+_], A, Ans](h: Handler[E, G, A, Ans])(using o: Ctx)
          (body: Handling[E, E + G, G, Ans, o.type] ?=> Freer[E + G, At[E + G, G, o.Here, Ans] *: o.Here, At[E + G, G, o.Here, Ans] *: o.Here, A])
  : Freer[G, o.Here, o.Here, Ans] =
  val in = new Handling[E, E + G, G, Ans, o.type]:
    val outer: o.type = o
    def handler: Handler[E, G, ?, Ans] = h
  Freer.Reset[E + G, G, o.Here, o.Here, Ans, Ans](body(using in).map(h.ret))

/** `perform(op)`: a capture to the handler of `E` in the context — the nearest delimiter if it is the handler, or
 * through each delimiter between, which forwards: a shift to it whose body performs outside and resumes `k`
 * after. The handler is found in the context's structure, at compile time; none, no program */
def perform[E[+_], X, H[+_]](op: E[X])(using c: In[H, ?, ?, ?], p: Perform[E, H, c.type]): c.Body[X] = p(op, c)
/** how `E` is performed in a context `C` of row `H` */
trait Perform[E[+_], H[+_], C <: In[H, ?, ?, ?]]:
  def apply[X](op: E[X], c: C): Freer[H, c.Here, c.Here, X]
object Perform extends PerformLow:
  /** the context is the handler's: a shift to its delimiter, the clause the body */
  given direct[E[+_], H[+_], Ans, Oc <: Ctx, C <: Handling[E, H, ?, Ans, Oc]]: Perform[E, H, C] with
    def apply[X](op: E[X], c: C): Freer[H, c.Here, c.Here, X] =
      Freer.Shift0[H, c.Out, c.D, c.D, c.D, Ans, Ans, X](k => c.handler(using c.outer)(op, k))
sealed trait PerformLow:
  /** the context is another delimiter's, which leaves at least the row outside it: a shift to it, performed
   * outside, `k` resumed with the result */
  given forward[E[+_], H[+_], Hf[+_], S, H2[+A] <: Hf[A], Oc <: In[H2, ?, ?, ?], C <: In[H, Hf, S, Oc]]
      (using o: Perform[E, H2, Oc]): Perform[E, H, C] with
    def apply[X](op: E[X], c: C): Freer[H, c.Here, c.Here, X] =
      Freer.Shift0[H, Hf, c.D, c.D, c.D, S, S, X](k => o(op, c.outer).flatMap(x => k(x)))
