package okay

import scala.annotation.tailrec
import okay.!.*

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
 * them, and a fresh `Delim` prompt separates them dynamically.
 */
enum State[S, +A] derives Effect {
  /** read the current state */
  case Get() extends State[S, S]

  /** replace the state, answering with the new one */
  case Set(s: S) extends State[S, S]

  /** replace the state by a function of it, answering the new one —
   * `modify` as ONE operation rather than a get then a set: half the
   * operations of every counter, and half the forwarding when another
   * handler stands between it and State's (specs/effect-row-cost.md
   * D1: 240 B a level against 96 for one set, measured exact) */
  case Modify(f: S => S) extends State[S, S]

  /** transition the state and answer from the OLD one, as ONE operation
   * (op-map-constructors, 2026-09-27): `f` gives the answer and the new
   * state. `update` and `swap` were a get, a set and a map. */
  case Update[S, B](f: S => (B, S)) extends State[S, B]
}

/** a value as a stateful computation */
extension [A](a: A)
  inline def state[S]: A ! State % S = pure(a)

object State {
  /** the current state — ONE shared node (specs/effect-op-cost.md):
   * `Get` carries no data, so every `get` is the same `Inject(Get())`
   * rather than a fresh pair, 32 B a call */
  // THE CAST, and the only one: `Get` has no fields, so after erasure
  // every `Get[S, S]` is the same object, and a program node is an
  // immutable value its handlers only read — sharing one across every
  // S changes nothing a program can observe. Inline, so the staging
  // macro sees `SharedOps.getNode` itself (DirectRow's shared-node table).
  inline def get[S]: S ! State % S = SharedOps.getNode.asInstanceOf[S ! State % S]



  /** replace the state */
  inline def set[S](s: S): S ! State % S = effect(Set(s))

  /**
   * apply f to the state, as ONE operation (`Modify`). It means a get
   * then a set, and until effect-row-recursion-cost (2026-09-26) it was
   * exactly that — two operations, a closure between them, and two
   * forwards through every handler standing between it and State's:
   * 240 B a level of a counting recursion against 96 now, measured
   * exact (specs/effect-row-cost.md). A handler of State's operations
   * answers `Modify(f)` with `f(s)` as the new state and the answer.
   *
   * It answers the NEW state, as both operations do — the file's one
   * convention, and worth keeping over the statement-shaped `Unit`
   * other libraries return: a caller who wants unit writes
   * `.map(_ => ())`, and one who wants the state would otherwise have
   * to ask for it again.
   */
  inline def modify[S](f: S => S): S ! State % S = effect(Modify(f))

  /**
   * a transition that ANSWERS something computed from the old state:
   * `f` sees the state and returns what to answer and what to leave
   * behind.
   *
   * `modify` cannot do this. It answers the new state, and what a
   * caller usually wants back is something the write destroys — the
   * value that WAS there. Spelt out, that is get, then set the
   * modified store, then answer out of the store already read; here
   * it is one step, and the answer type is whatever `f`'s first
   * component is.
   */
  inline def update[S, B](f: S => (B, S)): B ! State % S = effect(Update[S, B](f))

  /**
   * both states — what it was and what it is.
   *
   * The three answer three different questions, and the cost is the
   * reason there are three rather than one:
   *
   *   modify(f) : S          the new state — the common case, and it
   *                          allocates nothing
   *   update(f) : B          anything computed from the OLD state, in
   *                          one pass over it: the form that answers
   *                          what the write is about to destroy
   *   swap(f)   : (S, S)     both, for when the caller wants to
   *                          compare them
   *
   * `swap` is `update` with a pair for an answer, and a pair per call
   * is why `modify` does not simply answer one: most callers want the
   * new state or nothing at all, and they should not pay for a tuple
   * to say so.
   */
  inline def swap[S](f: S => S): (S, S) ! State % S =
    update(s => { val next = f(s); ((s, next), next) })

  /** run from an initial state to (final state, value) */
  inline def run[S, A](s: S)(a: A ! State % S): (S, A) = !.run(handle(s)(a))

  /**
   * the handler: a bespoke tail-recursive loop that threads the state
   * through itself. It cannot be a relay handler — the answer-
   * polymorphic ∀Y shape has nowhere to hold s — and a mutable cell
   * would break the purity of the residual tree, so the loop is the
   * honest form. A forwarded F-effect suspends with the current state
   * captured immutably, which keeps the residual re-runnable.
   *
   * TWO TYPE CLAUSES, separated by the state itself
   * (generalized-method-syntax, 2026-09-11): `S` has to be written at
   * most call sites, `A` and the forwarded row `F` are read off the
   * program. `State.handle[Int](0)(p)` rather than the three
   * arguments every call site used to spell out.
   */
  def handle[S](s: S)[A, F[+_]](a: A ! State % S + F)(using Distinct[State % S + F]): (S, A) ! F = {
    def _loop(s: S)(x: A ! State % S + F): (S, A) ! F = loop(s)(x)

    // `split`, not `<|>` (split-without-either): the two branches
    // beta-reduce into this match, no Either per operation. A
    // returning arm ascribes the loop's answer inside the branch,
    // where the constructor has refined the answer type to S.
    @tailrec def loop(s: S)(x: A ! State % S + F): (S, A) ! F = (x.resume: @unchecked) match
      case Return(a) => Return((s, a))
      case i @ Inject(e) => split[State[S, *], F](e) {
          case Get() => Return((s, s)): (S, A) ! F
          case Set(s) => Return((s, s)): (S, A) ! F
          case Modify(f) => { val next = f(s); Return((next, next)): (S, A) ! F }
          case Update(f) => { val (b, next) = f(s); Return((next, b)): (S, A) ! F }
        } { _ => forwarded[State[S, *], F](i).map((s, _)) }
      case Bind(i @ Inject(e), k) => split[State[S, *], F](e) {
          case Get() => loop(s)(k(s))
          case Set(s) => loop(s)(k(s))
          case Modify(f) => { val next = f(s); loop(next)(k(next)) }
          case Update(f) => { val (b, next) = f(s); loop(next)(k(b)) }
        } { _ => forwarded[State[S, *], F](i).flatMap(x => _loop(s)(k(x))) }

    loop(s)(a)
  }

  /**
   * A program written against a PART of the state, run against the
   * whole (specs/optics.md stage 3): the two functions say which
   * part, and nothing else about the state is touched.
   *
   * Not a handler — an INTERPRETATION of one effect into another, the
   * shape `docs/your-own-effect.md` names: every `Get` on the part
   * becomes a `Get` on the whole read through `look`, every `Set`
   * becomes a read, a `put` and a write. The forwarded arm carries
   * the rest of the row untouched, as `handle`'s does.
   *
   * WHY TWO FUNCTIONS AND NOT A LENS (core-modules stage 4). This was
   * `zoom(l: Lens[S, S, A, A])`, and it was the CORE's only reference
   * to optics — the one edge that kept `Optic.scala` from becoming a
   * module. Reading it settled what to do: of the whole lens it used
   * exactly `l.get` and `l.set`, so the interpretation was never
   * about optics at all. It says so now, and okay-optics gives back
   * the lens SPELLING as an extension, so `State.zoom(lens)(prog)`
   * still compiles character for character wherever that module is on
   * the classpath.
   */
  def zoomWith[S, A, X, F[+_]](look: S => A, put: A => S => S)(p: X ! State % A + F)(using Distinct[State % A + F]): X ! State % S + F = {
    // the part, read and written through the two functions, as a
    // program over the whole — what this interpretation is made of
    // every operation on the part is ONE `Update` on the whole
    // (op-map-constructors): it was a get or a modify followed by a map
    def readPart: A ! State % S + F = !.widen[A, State % S, F](update[S, A](s => (look(s), s)))
    def writePart(a: A): A ! State % S + F =
      !.widen[A, State % S, F](update[S, A](s => (a, put(a)(s))))
    def modifyPart(f: A => A): A ! State % S + F =
      !.widen[A, State % S, F](update[S, A](s => { val s2 = put(f(look(s)))(s); (look(s2), s2) }))
    def updatePart[B](g: A => (B, A)): B ! State % S + F =
      !.widen[B, State % S, F](update[S, B](s => { val (b, a2) = g(look(s)); (b, put(a2)(s)) }))

    def _loop(x: X ! State % A + F): X ! State % S + F = loop(x)
    @tailrec def loop(x: X ! State % A + F): X ! State % S + F = (x.resume: @unchecked) match
      case Return(v) => Return(v)
      // A LONE OPERATION IS A BIND WITH A PURE CONTINUATION, and the
      // arm below already knows that case. Written out here it would
      // need `A ! row <: X ! row` from the GADT refinement — Free is
      // invariant in its answer, so that is a cast, and this costs one
      // node instead of one.
      case Inject(e) => loop(Inject(e).flatMap(x => Return(x)))
      case Bind(Inject(e), k) => split[State[A, *], F](e) {
          case Get() => readPart.flatMap(a => _loop(k(a)))
          case Set(a) => writePart(a).flatMap(x => _loop(k(x)))
          case Modify(f) => modifyPart(f).flatMap(x => _loop(k(x)))
          case Update(g) => updatePart(g).flatMap(x => _loop(k(x)))
        } { e => Inject(e).flatMap(x => _loop(k(x))) }

    loop(p)
  }

  /** number the elements of a sequence, as a State program */
  def index[A](seq: Seq[A], from: Long = 0): (Long, Seq[(Long, A)]) = run(from):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- get; _ <- set(n + 1) yield (n, a) +: xs
}

/**
 * Parameterised (type-changing) state, founded on the continuation
 * paramonad: a computation of A that changes the state TYPE from S to
 * S2, with the final answer R, is Cont[A, S2 => R, S => R] — the state
 * is threaded by the answer type, get and set are shifts, and Cont's
 * flatMap already composes the transitions S -> S2 -> S3 (typestate:
 * the compiler enforces the protocol order). Unlike the State effect
 * above, whose handler loop is tail-recursive, running costs a stack
 * frame per operation, and it measures 1.29x slower on the same
 * workload (HandlerBenchmark: 21.23 vs 27.42 us/op, 3 forks,
 * re-measured 2026-09-17; this comment said ~1.7x, which no longer
 * held) — the typed protocol is what you buy.
 */
object PState {
  /** read the state, leaving its type unchanged */
  inline def get[S, R]: Cont[S, S => R, S => R] = shift(k => getAt(k))

  /** write a state of a possibly different type; the old state is the value */
  inline def set[S, S2, R](s2: S2): Cont[S, S2 => R, S => R] = shift(k => setAt(k, s2))

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
  def getAt[S, R](k: S => S => R): S => R = s => k(s)(s)
  def setAt[S, S2, R](k: S => S2 => R, s2: S2): S => R = s => k(s)(s2)

  /** run from an initial state to (final state, value) */
  inline def run[S, S2, A](s: S)(m: Cont[A, S2 => (S2, A), S => (S2, A)]): (S2, A) =
    (m / (a => s2 => (s2, a)))(s)

  /**
   * THE CARRIER: a typestate transition, seen as a profunctor in its
   * state (specs/optics.md stage 12, `optics-cont-profunctor`).
   *
   * `Cont[X, B => R, A => R]` computes an `X` and takes the state from
   * `A` to `B`. Read as `P[A, B]`, that is a profunctor — and an optic
   * IS a function `P[A1, A2] => P[S1, S2]` for every `P` with the
   * right structure, which is why `PState.zoom` is one line: the
   * optic run at this carrier.
   *
   * THE ALIAS IS ALL THAT IS LEFT HERE (core-modules stage 4). It
   * names a `Cont` and nothing else, so it stays in the core; the
   * `Optic.Strong` instance for it, and the `zoom` and `zoomCase`
   * spellings that need one, moved to okay-optics (`Zoom.scala`).
   * Callers write the same thing they always did.
   */
  type Zooming[X, R] = [A, B] =>> Cont[X, B => R, A => R]

  /**
   * THE THREADED ROAD (pstate-threaded, 2026-09-30): the same typestate
   * as DATA on the indexed tree, run by `State.handle`'s loop with the
   * TYPE moving. `get` and `set` above are shift bodies — the state
   * rides in the answer type (`S => R`), the runner is Cont's, and every
   * operation is a re-entry through a `Reentry`, which is the 1.29x
   * against `State.handle` that the header of this object records.
   * With the base's indexes invariant (freer-consumed-index) the other
   * reading types: `Op[S, R, X]` is an operation that moves the state
   * from `R` to `S` and answers `X`, the tree `Freer[Op, S, R, A]` is
   * the program, and `Threaded.run` holds the state in its hand and
   * continues at the type the operation gives it — `@tailrec`, no
   * continuation object, no room, nodes only. The compiler enforces
   * the protocol order exactly as it does for the shift road: `Put`
   * from a state the program is not in does not type.
   *
   * `Put` answers the OLD state, as `set` above does, so a program is
   * spelt the same on both roads and the two benchmark lanes fold the
   * same accumulator (HandlerBenchmark `statePara` / `stateThreaded`).
   */
  enum Op[S, R, +X]:
    /** read the state, leaving its type */
    case Get[S]() extends Op[S, S, S]
    /** replace the state, moving its type from `S` to `T`; the old state is the answer */
    case Put[S, T](t: T) extends Op[T, S, S]

  /** a typestate program on the threaded road: `A` computed, the state
   * moved from `R` to `S` */
  type Threaded[A, S, R] = Freer[Op, S, R, A]

  object Threaded:
    inline def get[S]: Threaded[S, S, S] = Freer.Inject[Op, S, S, S](Op.Get())
    inline def put[S, T](t: T): Threaded[S, T, S] = Freer.Inject[Op, T, S, S](Op.Put(t))

    /** run from an initial state to (final state, value): the loop
     * threads the state, typed by the GADT at every arm — `Return` gives
     * `S = R`, `Get` gives its state's type to the continuation, `Put`
     * hands the new state on */
    @tailrec def run[A, S, R](p: Threaded[A, S, R])(r: R): (S, A) =
      (p.resume: @unchecked) match
        case Freer.Return(a) => (r, a)
        case Freer.Inject(Op.Get()) => (r, r)
        case Freer.Inject(Op.Put(t)) => (t, r)
        case Freer.Bind(Freer.Inject(Op.Get()), k) => run(k(r))(r)
        case Freer.Bind(Freer.Inject(Op.Put(t)), k) => run(k(r))(t)
}
