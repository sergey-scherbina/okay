package okay.std

import okay.{!, +, %, Effect, Handler, effect, pure}
import okay.core.{+:, Split}
import okay.one.Cont
import okay.one.handler.Stateful
import okay.{Cps}
import scala.annotation.tailrec


/**
 * The State effect: the signature is fixed at one state type S, and
 * both operations answer with the (current or new) state. For a state
 * that changes its TYPE mid-program, see PState below.
 */
/**
 * PARAMETERISED, so the derived test is by CLASS only: the operations
 * carry no runtime trace of S, and a BARE row may therefore hold ONE
 * State. Two — `State % Int + State % String` — misroute, loudly
 * (TestRowIdentity): the first handler answers both asks and the
 * second continuation gets a ClassCastException, rather than a
 * plausible wrong answer.
 *
 * THAT IS THE BARE ROW, NOT THE LIBRARY. Several instances of one
 * signature are had three ways, and docs/many-instances.md is the
 * whole story: `Tag` names them in the type (`Tag.Of["small", State %
 * Int]`, and `Tag.tag` puts an ALREADY WRITTEN program's operations
 * under a key), `Refs` makes them at run time when a type cannot list
 * them, and a fresh `Shift` prompt separates them dynamically.
 */
enum State[S, +A] derives Effect {
  /** read the current state */
  case Get() extends State[S, S]

  /** transition the state and answer from the OLD one, as ONE operation
   * (op-map-constructors, 2026-09-27): `f` gives the answer and the new
   * state. `update` and `swap` were a get, a set and a map. Every write is
   * this (state-get-update, 2026-10-02): `set` and `modify` build one, and
   * `Set`/`Modify` are gone from the signature, which is two operations. */
  case Update[S, B](f: S => (B, S)) extends State[S, B]
}

/** a value as a stateful computation */
extension [A](a: A)
  inline def state[S]: A ! State % S = pure(a)

object State {
  /** the state */
  def get[S]: S ! State % S = effect(Get[S, S]())
  /** replace the state, answering the new one */
  def set[S](s: S): S ! State % S = effect(Update[S, S](Put(s)))

  /** `set`'s function, a value: its `toString` reads in a trace */
  final case class Put[S](s: S) extends (S => (S, S)):
    def apply(old: S): (S, S) = (s, s)
    override def toString: String = s"Put($s)"

  /** `modify`'s function, a value */
  final case class Modified[S](f: S => S) extends (S => (S, S)):
    def apply(old: S): (S, S) = { val n = f(old); (n, n) }
    override def toString: String = s"Modified($f)"

  /** the state through `f`, answering the new one */
  def modify[S](f: S => S): S ! State % S = effect(Update[S, S](Modified(f)))
  /** the state through `f`, answering `f`'s value — one operation */
  def update[S, B](f: S => (B, S)): B ! State % S = effect(Update[S, B](f))
  /** the state through `f`, answering the old and the new */
  def swap[S](f: S => S): (S, S) ! State % S = update(s => { val next = f(s); ((s, next), next) })

  /** a program of State alone, run from `s`: the last state and the value */
  def run[S, A](s: S)(a: A ! State % S): (S, A) = handle(s)(a).run

  /** the handler as a value: a register of the machine, a `k` taken past it with the state as it was then */
  def apply[S](s: S): Handler[State % S, [A] =>> (S, A)] = new Handler[State % S, [A] =>> (S, A)]:
    def kernel[T <: okay.core.Row, A]: okay.one.handler.Handler[State % S, T, A, (S, A)] = new Cell[S, T, A](s)

  /** the kernel's `Stateful`, written out: a `Get` its value the state, the state as it is; an `Update` its function
   * applied ONCE, at `value`, its next state kept for `next`, which the machine asks right after — no tuple an
   * operation beyond the function's own, no closure */
  private final class Cell[S, T <: okay.core.Row, A](s0: S) extends okay.one.handler.Stateful[State % S, T, S, A, (S, A)]:
    private var nextS: S = s0
    def initial: S = s0
    def ret(s: S, a: A) = okay.one.Cont.pure((s, a))
    def value[X](s: S, op: State[S, X]): X = op match
      case _: Get[s, ?] => nextS = s; s.asInstanceOf[X]
      case u: Update[s, b] => val (b, n) = u.f(s); nextS = n; b.asInstanceOf[X]
    def next[X](s: S, op: State[S, X]): S = nextS

  /** `a`'s State from `s`, wherever it is among its effects: the last state and the value */
  def handle[S](s: S)[A, Es[_]](a: A ! Es)(using sp: Split[Es, State % S]): (S, A) ! sp.Rest = a.handle(State(s))

  /** a program over a PART of the state, run over the whole: `look` reads the part, `put` writes it back */
  def zoomWith[S, A, X, Es[_]](look: S => A, put: A => S => S)(p: X ! Es)(using sp: Split[Es, State % A])
  : X ! State % S + sp.Rest =
    p.translate[State % A, State % S]([Y] => (op: State[A, Y]) => (op match
      case Get() => update[S, A](s => (look(s), s))
      case Update(g) => update[S, Y](s => { val (b, a2) = g(look(s)); (b, put(a2)(s)) })): Y ! State % S)

  /** number the elements of a sequence, as a State program */
  def index[A](seq: Seq[A], from: Long = 0): (Long, Seq[(Long, A)]) = run(from):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- get[Long]; _ <- set[Long](n + 1) yield (n, a) +: xs
}

/**
 * Parameterised (type-changing) state, founded on the continuation
 * paramonad: a computation of A that changes the state TYPE from S to
 * S2, with the final answer R, is Cps[A, S2 => R, S => R] — the state
 * is threaded by the answer type, get and set are shifts, and Cps's
 * flatMap already composes the transitions S -> S2 -> S3 (typestate:
 * the compiler enforces the protocol order). Unlike the State effect
 * above, whose handler loop is tail-recursive, every operation is a
 * shift (its answer, a function, is applied by a loop — `Bounce`,
 * cont-fun-answer — so no host frame a step), and it measures 1.79x slower on the same
 * workload (HandlerBenchmark `statePara` 30.39 vs `stateEffect` 16.96
 * us/op, MIN of 3 rounds, JDK 26, 2026-09-30; 1.29x on 2026-09-17,
 * ~1.7x before that — the ratio moves with the JIT's inlining of the
 * re-entry road, and the typed protocol was what you bought). SINCE
 * pstate-threaded (2026-09-30) the same protocol as DATA, `Threaded`
 * below, runs through `State.handle`'s own loop shape at 1.07x
 * (`stateThreaded` 18.07 us/op): the type moves, and the price is one
 * unshared `Get` node a step. Buy the shift road for what only it can
 * do — a body that uses `k`, the profunctor `Zooming` — and the data
 * road for a protocol. A ZOOM by a lens is on the data road too since
 * 2026-10-02 (`Threaded.zoomWith`, okay-optics' `Threaded.zoom`):
 * operations of the program, no nested run (specs/cont-js-depth.md 3a).
 * (The numbers are the classic's, okay-freer's indexed tree; the data
 * road is the kernel's program since 2026-10-15, not yet measured.)
 */
object PState {
  /** read the state, leaving its type unchanged */
  inline def get[S, R]: Cps[S, S => R, S => R] = Cps.shift(k => getAt(k))

  /** write a state of a possibly different type; the old state is the value */
  inline def set[S, S2, R](s2: S2): Cps[S, S2 => R, S => R] = Cps.shift(k => setAt(k, s2))

  // The two bodies are GENERIC methods, not lambdas at the inline call
  // site (cont-stack-fastpath, 2026-09-27). Written inline, the lambda
  // is specialised to the caller's S: a Long state arrives boxed, is
  // unboxed into it and boxed AGAIN for each use. Whether C2 folds the
  // re-box away depends on how deep the continuation's call chain
  // inlines — at the pre-cont-stack base it did, after cont-stack's
  // Reentry one of get's two did not, and that one Long per operation
  // was ALL of statePara's +20 928 B/op (an exact class histogram,
  // specs/cont-stack.md Results). Erased, the state passes through as
  // the object it already is: nothing to re-box, whatever inlines.
  // Public because the expansion at a user's call site calls them; not
  // an API.
  def getAt[S, R](k: S => S => R): S => R = new GetAt[S, R](k)
  def setAt[S, S2, R](k: S => S2 => R, s2: S2): S => R = new SetAt[S, S2, R](k, s2)

  // the two bodies as `Bounce`s: the rest of the program, `k(s)`, answered with its state, never applied here
  private final class GetAt[S, R](k: S => S => R) extends Bounce[S, R]:
    type X = S
    def next(s: S): S => R = k(s)
    def arg(s: S): S = s
  private final class SetAt[S, S2, R](k: S => S2 => R, s2: S2) extends Bounce[S, R]:
    type X = S2
    def next(s: S): S2 => R = k(s)
    def arg(s: S): S2 = s2

  /**
   * A FUNCTION ANSWER APPLIED BY A LOOP (cont-fun-answer). A state-passing body `s => k(s)(s2)` applies the
   * rest inside its own frame, so applying the answer of n steps nests n host frames — past any stack, the
   * machine's switch included, since the nesting is outside the machine. A `Bounce` answers the next function
   * and its argument (`next`, then `arg`) instead of applying them, and its `apply` is the loop: one frame for
   * the whole chain, on every platform, for whoever applies the answer. A function that is not a `Bounce` ends
   * the chain. Write a body of this shape as one (`PState.get`/`set` are two).
   */
  abstract class Bounce[-S, +R] extends (S => R):
    /** the next function's argument type */
    type X
    /** the next function, not applied: called once a step, before `arg` */
    def next(s: S): X => R
    /** its argument */
    def arg(s: S): X
    final def apply(s: S): R = Bounce.loop[S, R](this, s)

  object Bounce:
    // the pair travels as the loop's two parameters, not an object a step; `Bounce` carries `Function1`'s
    // variances, so the type test is implied by the scrutinee's type and nothing is cast
    @tailrec def loop[A, R](f: A => R, a: A): R = f match
      case b: Bounce[A, R] => loop[b.X, R](b.next(a), b.arg(a))
      case g => g(a)

  /** run from an initial state to (final state, value) */
  inline def run[S, S2, A](s: S)(m: Cps[A, S2 => (S2, A), S => (S2, A)]): (S2, A) =
    (m / (a => s2 => (s2, a)))(s)

  /**
   * THE CARRIER: a typestate transition, seen as a profunctor in its
   * state (specs/optics.md stage 12, `optics-cont-profunctor`).
   *
   * `Cps[X, B => R, A => R]` computes an `X` and takes the state from
   * `A` to `B`. Read as `P[A, B]`, that is a profunctor — and an optic
   * IS a function `P[A1, A2] => P[S1, S2]` for every `P` with the
   * right structure, which is why `PState.zoom` is one line: the
   * optic run at this carrier.
   *
   * THE ALIAS IS ALL THAT IS LEFT HERE (core-modules stage 4). It
   * names a `Cps` and nothing else, so it stays in the core; the
   * `Optic.Strong` instance for it, and the `zoom` and `zoomCase`
   * spellings that need one, moved to okay-optics (`Zoom.scala`).
   * Callers write the same thing they always did.
   */
  type Zooming[X, R] = [A, B] =>> Cps[X, B => R, A => R]

  /**
   * THE THREADED ROAD (pstate-threaded, 2026-09-30; on the kernel 2026-10-15): the same typestate as DATA — the
   * operations of a program, answered by a handler holding the state with its TYPE moving, `State.handle`'s shape:
   * the kernel's `Stateful`, a register of the machine, each operation answered in place. `get` and `set` above are
   * shift bodies (the state rides in the answer type, `S => R`, every operation a re-entry); here nothing is captured.
   *
   * WHY NOT THE KERNEL'S `Atm`/`PState`: they are that shift road — a step's type names the run's final value
   * (`Hole[S, F]`, given by `Typestate[F]`), so a step is a program only inside one run. A `Threaded[A, S, R]` is a
   * value of no run (stored, zoomed into another, `okay-ui`'s wizard stepped by events between runs), and its
   * operations are data for a handler, not answers to be applied.
   *
   * `Indexed[A, S, R, T]` is the program: the kernel's program of `Op` before the row `T`, `A` computed, the state
   * moved from `R` to `S`. The indexes are the TYPE's alone (opaque, phantom), so every word here says how it moves
   * them and the compiler enforces the protocol order as the shift road's does: a `put` from a state the program is
   * not in does not type. The other operations, `T`'s, leave the state where it is (`Indexed.perform`), and
   * `Indexed.handle` answers `Op` alone — a protocol beside ordinary effects. `Threaded` is it over no other effect.
   * `Put` answers the OLD state, as `set` above does, so a program is spelt the same on both roads.
   */
  enum Op[+X]:
    /** read the state, leaving its type */
    case Get[S]() extends Op[S]
    /** replace the state, moving its type from `S` to `T`'s; the old state is the answer */
    case Put[S, T](t: T) extends Op[S]
    /** a zoom's entry: the whole, `S1`, the answer; the state the part `look` reads */
    case Focus[S1, A1](look: S1 => A1) extends Op[S1]
    /** a zoom's exit: the part put back into the whole as it was at the entry, `s1`; the inner value `x` the answer */
    case Restore[S1, A2, S2, X](s1: S1, put: (S1, A2) => S2, x: X) extends Op[X]

  /** a typestate program over the row `T`: `A` computed, the state moved from `R` to `S` */
  opaque type Indexed[+A, S, R, T <: okay.core.Row] <: AnyRef = okay.core.Eff[Op +: T, A]

  object Indexed:
    extension [A, S, R, T <: okay.core.Row](p: Indexed[A, S, R, T])
      /** the transitions composed: `R -> S`, then `S -> S2` — inline, the kernel's bind where it is written */
      inline def flatMap[B, S2](f: A => Indexed[B, S2, S, T]): Indexed[B, S2, R, T] = p.flatMap(f)
      inline def map[B](f: A => B): Indexed[B, S, R, T] = p.map(f)

    /** read the state, leaving its type: ONE node for every state type and row (`SharedOps.getT`) */
    def get[S, T <: okay.core.Row]: Indexed[S, S, S, T] = the[Indexed[S, S, S, T]](SharedOps.getT)
    /** replace the state, moving its type from `S` to `S2`; the old state is the answer */
    def put[S, S2, T <: okay.core.Row](s2: S2): Indexed[S, S2, S, T] = Cont.perform[Op, Op +: T, S](Op.Put[S, S2](s2))
    /** an operation of another effect, in `T`: the state where it is */
    def perform[E[_], S, T <: okay.core.Row, X](op: E[X])(using in: okay.core.In[E, T]): Indexed[X, S, S, T] =
      okay.core.Ops.perform[E, Op +: T, X](op)(using okay.core.In.There(in))

    /** `inner` over the PART `look` reads, run over the whole: the whole goes `S1 -> S2` exactly when the part goes
     * `A1 -> A2` (a four-parameter lens). Two operations of the program, no nested run: the inner program runs at the
     * state the handler holds, the whole kept in the rest of the program */
    def zoomWith[S1, S2, A1, A2, X, T <: okay.core.Row](look: S1 => A1, put: (S1, A2) => S2)(inner: Indexed[X, A2, A1, T])
        : Indexed[X, S2, S1, T] =
      Cont.perform[Op, Op +: T, S1](Op.Focus[S1, A1](look))
        .flatMap(s1 => inner.flatMap(x => Cont.perform[Op, Op +: T, X](Op.Restore[S1, A2, S2, X](s1, put, x))))

    /** `p`'s state handled from `r`: the last state and the value, a program over the row left, `T` */
    def handle[A, S, R, T <: okay.core.Row](p: Indexed[A, S, R, T])(r: R): okay.core.Eff[T, (S, A)] =
      p.handle(Cell[T, S, A](r))

    /** the state held as it was put, of the type the program's indexes say it is at that operation */
    private final class Cell[T <: okay.core.Row, S, A](r: Any) extends Stateful[Op, T, Any, A, (S, A)]:
      def initial: Any = r
      def ret(s: Any, a: A) = Cont.pure((the[S](s), a))
      def value[X](s: Any, op: Op[X]): X = op match
        case Op.Restore(_, _, x) => x
        case _ => the[X](s)   // `Get`, `Put` and `Focus` answer the state as it was
      def next[X](s: Any, op: Op[X]): Any = op match
        case _: Op.Get[?] => s
        case p: Op.Put[?, ?] => p.t
        case f: Op.Focus[s1, ?] => f.look(the[s1](s))
        case r: Op.Restore[?, a2, ?, ?] => r.put(r.s1, the[a2](s))

    /** THE CAST, the one of this road: the handler holds the state as `Any`, since its type moves and a `Stateful`'s
     * does not, and gives it back at the type the operation names. Right by construction: `Indexed` is opaque, and
     * every word making one moves its indexes as the operation moves the state — at a `Get[S]` the state is an `S`,
     * at a `Focus[S1, _]` an `S1`, at the end the `S` of the program's type. The shared `Get` node is the same claim
     * (a fieldless operation, one node for every state type and row: `PerformHere` names no place) */
    private def the[X](a: Any): X = a.asInstanceOf[X]

  /** a typestate program on the threaded road, over no other effect: `A` computed, the state moved from `R` to `S` */
  type Threaded[A, S, R] = Indexed[A, S, R, okay.core.Pure]

  object Threaded:
    inline def get[S]: Threaded[S, S, S] = Indexed.get[S, okay.core.Pure]
    inline def put[S, T](t: T): Threaded[S, T, S] = Indexed.put[S, T, okay.core.Pure](t)

    /**
     * A program over the PART of the state a lens reads, run over the whole (specs/cont-js-depth.md stage 3a): the
     * inner program starts at `look(s1)`, and its final part goes back with `put`. The shift road's `PState.zoom`
     * runs the inner program as a NESTED run (`p / …` inside a body); here it is two operations of the program, and
     * the kernel's machine keeps the outer program on its own (heap) stack.
     */
    def zoomWith[S1, S2, A1, A2, X](look: S1 => A1, put: (S1, A2) => S2)(inner: Threaded[X, A2, A1]): Threaded[X, S2, S1] =
      Indexed.zoomWith[S1, S2, A1, A2, X, okay.core.Pure](look, put)(inner)

    /** run from an initial state to (final state, value) */
    def run[A, S, R](p: Threaded[A, S, R])(r: R): (S, A) = Indexed.handle(p)(r).run
}
