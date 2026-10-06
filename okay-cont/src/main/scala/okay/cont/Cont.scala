package okay.cont

/** a level of a stack: the stacks OUTSIDE its delimiter `D` — where the delimiter's continuation lives, so where a
 * capture to it is a program — constant along the level; and its ANSWER at this point. The head of a stack is the
 * nearest level's; the top is empty */
final class At[D <: Tuple, S]

/**
 * THE MONAD OF DELIMITED CONTINUATIONS, TWO STACKS (specs/freer-min.md, stage 26): `shift0` and `reset` as nodes of
 * one tree, typed by two stacks of answer types, a level each — Danvy–Filinski's `(A => S) => R` with `S` and `R`
 * grown into the stacks `I` and `O`: `Cont[I, O, A]` is a program of value `A` which, with a continuation
 * answering `I`, answers `O`. `Bind` composes them end to end, as states, at every level at once; `Return` keeps
 * them; `Reset`, `Shift0` and `Op` move the head's. No row: what a program may do is what its CONTEXT reaches — a
 * handler is a delimiter, an operation a capture to it (`Op`), and `perform` needs the handler in the context's
 * types; the index is the delimiters in force, each by its stacks outside and its answer.
 *
 * A delimiter's VALUE IS ITS FINAL ANSWER: `reset body` where `body : S` from `At[D, S] *: D` to `At[D, R] *: O`
 * answers `R` outside, from `D` to `O`, as the body moved the outside. A capture goes to the delimiter the node
 * names — the nearest for `Shift0`, `at` levels out for `Op`, the delimiters between crossed. Its `k` is PURE, a
 * program outside the delimiter, from `D` to the stacks at the hole, delivering the answer at the hole. The body
 * of a shift runs OUTSIDE the delimiter, in its place (shift0), and its value is the delimiter's answer. Seven
 * nodes, no cast, no prompt, no row.
 */
enum Cont[I <: Tuple, O <: Tuple, +A]:
  case Return[Σ <: Tuple, A](a: A) extends Cont[Σ, Σ, A]
  case Bind[I <: Tuple, T <: Tuple, O <: Tuple, A, B](m: Cont[T, O, A], k: A => Cont[I, T, B]) extends Cont[I, O, B]
  /** a program built when the machine gets to it: one step where a bind on a unit is three (2.8× on tail calls) */
  case Delay[I <: Tuple, O <: Tuple, A](t: () => Cont[I, O, A]) extends Cont[I, O, A]
  /** `reset body`: the body's value is its initial answer `S`, at its own level; the delimiter answers `R` outside,
   * from `D` to `O`, as the body moved the outside */
  case Reset[D <: Tuple, O <: Tuple, S, R](body: Cont[At[D, S] *: D, At[D, R] *: O, S]) extends Cont[D, O, R]
  /** `shift0 (k => e)`: `k` is the context up to the nearest delimiter, it included, pure, a program outside it from
   * `D` to the stacks at the hole `I`, delivering the answer at the hole `T`; `e` runs in the delimiter's place, from
   * `D` to what the context expects, `O`, and its value is the delimiter's answer `R` */
  case Shift0[D <: Tuple, I <: Tuple, O <: Tuple, T, R, X](f: (X => Cont[D, I, T]) => Cont[D, O, R])
    extends Cont[At[D, T] *: I, At[D, R] *: O, X]
  /** an OPERATION: a shift to its handler's delimiter, `at` levels out — the delimiters between are CROSSED, one
   * capture, one resumption; it moves nothing (the diagonal shift), which is what lets the machine prove each
   * delimiter it reaches is the one the node names: a level's OUT index is one along the level, and the
   * delimiter's record is tied to it. A clause that returns `k(x)` as its whole body is answered where the
   * operation was, the stack as it stands, nothing built. The operation and its clause are fields, no closure:
   * the clause is the handler's, made once with the reach, where the context is (`Perform`) */
  case Op[N <: Tuple, X, Dn <: Tuple, Ansn, E[+_]](at: Reach[N, Dn, Ansn], op: E[X], clause: Clause[E, Dn, Ansn]) extends Cont[N, N, X]
  /** `k(x)` pending: the machine puts the captured piece back under a delimiter of its own */
  case Resume[D <: Tuple, I <: Tuple, X, T](x: X, k: Captured[D, I, X, T, ?]) extends Cont[D, I, T]

  /** the stacks composed end to end: `this` from `I` to `O`, `f`'s from `I2` to `I` */
  def flatMap[I2 <: Tuple, B](f: A => Cont[I2, I, B]): Cont[I2, O, B] = Bind(this, f)
  def map[B](f: A => B): Cont[I, O, B] = Bind(this, a => Return(f(a)))

/** THE MACHINE AS A CONTROL CARRIER: a program at ONE level, the top's, whose answer moves from `S` to `R` —
 * Danvy–Filinski's `(A => S) => R`, the stacks of the level outside empty */
type Carrier[A, S, R] = Cont[At[EmptyTuple, S] *: EmptyTuple, At[EmptyTuple, R] *: EmptyTuple, A]

/** HOW FAR an operation reaches, from the index `N` it is performed at: its handler's delimiter is the nearest
 * (`Here`: the target's stacks outside and answer are `N`'s head's), or it is outside the nearest, whose outside
 * `D` IS the next level's index, and the reach goes on from there (`Out`) — each step a level crossed */
enum Reach[N <: Tuple, Dn <: Tuple, Ansn]:
  case Here[D <: Tuple, Ans]() extends Reach[At[D, Ans] *: D, D, Ans]
  case Out[Ans, N2 <: Tuple, Dn <: Tuple, Ansn](next: Reach[N2, Dn, Ansn]) extends Reach[At[N2, Ans] *: N2, Dn, Ansn]

/** a handler's clause as the machine calls it: at the handler's outside `D`, with the captured `k` */
trait Clause[E[+_], D <: Tuple, Ans]:
  def apply[X](op: E[X], k: X => Cont[D, D, Ans]): Cont[D, D, Ans]

/** a program at the top: no delimiter in force, nothing outside, the stacks empty; its answer is its value,
 * Danvy–Filinski's `⟨e⟩ : τ` */
type Top[A] = Cont[EmptyTuple, EmptyTuple, A]

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
/** the context of the BODY of a delimiter: the answer `S` the body is written at, and the context outside by its
 * own type, `Oc` — a shift's body is written in it; the stacks outside are the outer context's `Here`. `reset` and
 * `handle` give their body one, `shift0` and `perform` read it */
sealed trait In[S, Oc <: Ctx] extends Ctx:
  /** the answer the body is written at */
  type Sa = S
  val outer: Oc
  type D = outer.Here
  type Here = At[D, S] *: D
  /** a program in the body this context is for, its stacks unchanged */
  type Body[A] = Cont[Here, Here, A]
object In:
  /** the context of a body written at the answer `S` inside the context `o`: what `reset` and `handle` give
   * their body, and what a program folded at the top (`Prog.foldCont`) runs in */
  def at[S](using o: Ctx): In[S, o.type] = new In[S, o.type]:
    val outer: o.type = o
/** the context of a fragment written for the body of a `reset[S]`: `def f(using in: Under[S]): in.Body[A]` */
type Under[S] = In[S, ?]

def pure[A, Σ <: Tuple](a: A): Cont[Σ, Σ, A] = Cont.Return(a)
def delay[I <: Tuple, O <: Tuple, A](t: => Cont[I, O, A]): Cont[I, O, A] = Cont.Delay(() => t)
def defer[I <: Tuple, T <: Tuple, O <: Tuple, A, B](t: => Cont[T, O, A])(f: A => Cont[I, T, B]): Cont[I, O, B] =
  Cont.Bind(Cont.Delay(() => t), f)
/** `reset[S](body)`: the body is written at the answer `S`, Danvy–Filinski's annotation of the delimiter (a type in
 * a context function's parameter is fixed before its body is typed, so it is named), in the context where the
 * reset is written, with the delimiter on top */
def reset[S]: ResetAt[S] = ResetAt[S]()
final class ResetAt[S]:
  def apply[O <: Tuple, R](using o: Ctx)(body: In[S, o.type] ?=> Cont[At[o.Here, S] *: o.Here, At[o.Here, R] *: O, S]): Cont[o.Here, O, R] =
    Cont.Reset[o.Here, O, S, R](body(using In.at[S]))
/** `shift0[X](k => …)` written in a delimiter's body: the hole `X` is the one thing nothing else says; the delimiter
 * is the context's, its answer the one the body is written at. The shift's body is written inside the delimiter
 * but runs outside it, in the context outside, and may move it. `k` leaves the outside as it is, `D` to `D` (a
 * piece that moved it could not be resumed twice); the node is general */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[S, Oc <: Ctx, O <: Tuple, R](using in: In[S, Oc])
           (f: Oc ?=> (X => Cont[in.D, in.D, S]) => Cont[in.D, O, R]): Cont[At[in.D, S] *: in.D, At[in.D, R] *: O, X] =
    Cont.Shift0[in.D, in.D, O, S, R, X](f(using in.outer))

/** A HANDLER IS A DELIMITER: of the effect `E`; the body's value `A`, the answer `Ans`. Deep: `k` brings the
 * delimiter along, so the handler stays in force through a resumption. The clauses run outside the delimiter, in
 * the context outside, `o`, at any stacks there */
trait Handler[E[+_], A, Ans]:
  def ret(a: A): Ans
  def apply[X, Oc <: Ctx](using o: Oc)(op: E[X], k: X => Cont[o.Here, o.Here, Ans]): Cont[o.Here, o.Here, Ans]
/** a TAIL-RESUMPTIVE handler: each clause `k(value(op))`, the value of the operation alone — so the operation is
 * ANSWERED IN PLACE, where it is performed, when the machine gets there: no capture, no delimiter touched (Koka's,
 * Effekt's optimisation, here by the handler's declaration, chosen at compile time by the context's type) */
trait Answering[E[+_], A, Ans] extends Handler[E, A, Ans]:
  def value[X](op: E[X]): X
  final def apply[X, Oc <: Ctx](using o: Oc)(op: E[X], k: X => Cont[o.Here, o.Here, Ans]): Cont[o.Here, o.Here, Ans] = k(value(op))
/** the context of a handler's body: it handles `E` */
sealed trait Handling[E[+_], Ans, Oc <: Ctx] extends In[Ans, Oc]:
  def handler: Handler[E, ?, Ans]
/** the context of an answering handler's body: its type says so, and `perform` answers in place */
sealed trait Answers[E[+_], Ans, Oc <: Ctx] extends Handling[E, Ans, Oc]:
  def answering: Answering[E, ?, Ans]
/** `handle(h)(body)`: the delimiter of the handler `h`; what the body may perform is what its context reaches */
def handle[E[+_], A, Ans](h: Handler[E, A, Ans])(using o: Ctx)
          (body: Handling[E, Ans, o.type] ?=> Cont[At[o.Here, Ans] *: o.Here, At[o.Here, Ans] *: o.Here, A]): Cont[o.Here, o.Here, Ans] =
  val in = new Handling[E, Ans, o.type]:
    val outer: o.type = o
    def handler: Handler[E, ?, Ans] = h
  Cont.Reset[o.Here, o.Here, Ans, Ans](body(using in).map(h.ret))
/** `handle(h)(body)` for an answering handler: the body's context says it answers */
def handle[E[+_], A, Ans](h: Answering[E, A, Ans])(using o: Ctx)
          (body: Answers[E, Ans, o.type] ?=> Cont[At[o.Here, Ans] *: o.Here, At[o.Here, Ans] *: o.Here, A]): Cont[o.Here, o.Here, Ans] =
  val in = new Answers[E, Ans, o.type]:
    val outer: o.type = o
    def handler: Handler[E, ?, Ans] = h
    def answering: Answering[E, ?, Ans] = h
  Cont.Reset[o.Here, o.Here, Ans, Ans](body(using in).map(h.ret))

/** `perform(op)`: to the handler of `E` in the context — answered in place if it answers, one capture to its
 * delimiter if not, through the delimiters between. The handler is found in the context's types, at compile time;
 * none, no program */
def perform[E[+_], X](op: E[X])(using c: In[?, ?], p: Perform[E, c.type]): c.Body[X] = p(op)
/** how `E` is performed in a context `C`: `answered` (in place) where its handler answers, else `reaches` (one
 * capture) — chosen at compile time, by the given's priority. It is OF ITS CONTEXT, `c`, taken where it is
 * summoned: the reach to the handler and its clause are made once, here, not at each operation */
trait Perform[E[+_], C <: In[?, ?]]:
  val c: C
  def apply[X](op: E[X]): Cont[c.Here, c.Here, X]
object Perform extends PerformLow:
  given answered[E[+_], C <: In[?, ?]](using c0: C, a: Answered[E, C]): Perform[E, C] with
    val c: C = c0
    def apply[X](op: E[X]): Cont[c.Here, c.Here, X] = Cont.Delay(() => Cont.Return(a(op, c)))
sealed trait PerformLow:
  given reaches[E[+_], C <: In[?, ?]](using c0: C, r: Reaches[E, C]): Perform[E, C] with
    val c: C = c0
    val t: Target[E, c.Here] = r.target(c)
    def apply[X](op: E[X]): Cont[c.Here, c.Here, X] = Cont.Op[c.Here, X, t.Dn, t.Ansn, E](t.reach, op, t.clause)
/** the handler of `E` reached from a context `C`: where its delimiter is, and its clause */
trait Reaches[E[+_], C <: Ctx]:
  def target(c: C): Target[E, c.Here]
/** a handler's delimiter as reached from an index: the reach to it, and the clause, at the target's own outside */
sealed trait Target[E[+_], N <: Tuple]:
  type Dn <: Tuple
  type Ansn
  def reach: Reach[N, Dn, Ansn]
  def clause: Clause[E, Dn, Ansn]
object Reaches:
  /** the context is the handler's: its delimiter is the nearest */
  given here[E[+_], Ans, C <: Handling[E, Ans, ?]]: Reaches[E, C] with
    def target(c: C): Target[E, c.Here] = new Target[E, c.Here]:
      type Dn = c.D
      type Ansn = Ans
      val reach: Reach[c.Here, c.D, Ans] = Reach.Here[c.D, Ans]()
      val clause: Clause[E, c.D, Ans] = new Clause[E, c.D, Ans]:
        def apply[X](op: E[X], k: X => Cont[c.D, c.D, Ans]): Cont[c.D, c.D, Ans] = c.handler(using c.outer)(op, k)
  /** the context is another delimiter's: one level out, the outer context's reach after it */
  given out[E[+_], Oc <: In[?, ?], C <: In[?, Oc]](using o: Reaches[E, Oc]): Reaches[E, C] with
    def target(c: C): Target[E, c.Here] =
      val t = o.target(c.outer)
      new Target[E, c.Here]:
        type Dn = t.Dn
        type Ansn = t.Ansn
        val reach: Reach[c.Here, t.Dn, t.Ansn] = Reach.Out[c.Sa, c.D, t.Dn, t.Ansn](t.reach)
        val clause: Clause[E, t.Dn, t.Ansn] = t.clause
/** the value of `E` answered in place in the context `C`: its handler answers, or a context outside does */
trait Answered[E[+_], C <: Ctx]:
  def apply[X](op: E[X], c: C): X
object Answered:
  given here[E[+_], C <: Answers[E, ?, ?]]: Answered[E, C] with
    def apply[X](op: E[X], c: C): X = c.answering.value(op)
  given outside[E[+_], Oc <: Ctx, C <: In[?, Oc]](using o: Answered[E, Oc]): Answered[E, C] with
    def apply[X](op: E[X], c: C): X = o(op, c.outer)

/** STATE, answering in place: the state in a cell of the handler, one per `handle`, so `get` and `put` are answered
 * where they are performed, no capture. A resumption shares the cell: a body resumed twice sees ONE state, the
 * second resumption the first's last — not a replay. The replay is the answer type's (TestState, `PState`) */
enum State[S, +A]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]
final class StateCell[S, A](var state: S) extends Answering[[X] =>> State[S, X], A, (S, A)]:
  def ret(a: A): (S, A) = (state, a)
  def value[X](op: State[S, X]): X = op match
    case State.Get() => state
    case State.Put(s) => state = s
/** `state(s0)(body)`: the body with `get`/`put` answered from a cell starting at `s0`; the last state and the value */
def state[S, A](s0: S)(using o: Ctx)
         (body: Answers[[X] =>> State[S, X], (S, A), o.type] ?=> Cont[At[o.Here, (S, A)] *: o.Here, At[o.Here, (S, A)] *: o.Here, A])
  : Cont[o.Here, o.Here, (S, A)] =
  handle[[X] =>> State[S, X], A, (S, A)](StateCell[S, A](s0))(body)
