package okay

import okay.Freer.{Return, Inject, Bind, Delay}
import scala.collection.LinearSeq

/**
 * Danvy-Filinski's one-prompt `shift`/`reset` with answer-type modification: `M[A, S, R]` is `(A => S) => R`.
 * Instances: `Cont` (data, stack-safe) and `Func` (closures). THIS FILE IS THE FACADE: the machine is
 * the stack of continuations (`Delimited`), typed per reset installation, so nothing here is claimed; ContMacro
 * turns what it can read into data first.
 */
trait Control[M[_, _, _]] extends ParaMonad[M]:
  def shift[A, S, R](f: (A => S) => R): M[A, S, R]
  extension [A, S, R](m: M[A, S, R])
    infix def /(k: A => S): R
  inline def reset[A, R](m: M[A, A, R]): R = m / identity

/** summons the instance at its precise type, so its inline operations resolve statically (Carette-Kiselyov-Shan staging) */
transparent inline def Control[M[_, _, _]]: Control[M] =
  compiletime.summonInline[Control[M]]

/** `Cont[A, R, R]`: the diagonal, an ordinary monad */
infix type />[A, R] = Cont[A, R, R]
/** what `reset` can delimit */
infix type ^[A, R] = Cont[A, A, R]
/**
 * `(A => S) => R` as data: a freer tree whose indexes are the answer types; `/` runs it.
 * Free is Cont whose shift body the handler chooses.
 */
type Cont[A, S, R] = Cont.Rep[A, S, R]

object Cont:

  /** opaque inside the object, not the package: package-wide it would pick up every program extension */
  opaque type Rep[A, S, R] = Freer[Sig, S, R, A]

  /** a value */
  def Pure[A, R](a: A): Rep[A, R, R] = Return(a)

  /**
   * `shift`: one leaf, an operation of the ATM machine (`Delimited.Op`). `ContMacro` picks the leaf's form at compile time:
   * a tail body is a value (`tailShift`/`tailPure`), an answer-using body a program over a lazy `k`
   * (`lazyLeaf`), anything else gets a strict `k` (`shiftLeaf`).
   */
  inline def shift[A, S, R](inline f: (A => S) => R)(using inline scope: Shifts): Rep[A, S, R] =
    ${ okay.macros.ContMacro.shift('f, 'scope) }

  /** delimit and run: `c / identity` */
  inline def reset[A, R](c: Rep[A, A, R]): R = run(c)(identity)

  /** an opaque body: run as it is, given a strict `k` (`Delimited.Resumption`) */
  def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] = Inject[Sig, S, R, A](Op.Strict(f, true))
  /** an opaque body never re-executed (`noReplay`, `safe` scopes): its `k` a barrier, whatever the run-time mode */
  def shiftLeafOnce[A, S, R](f: (A => S) => R): Rep[A, S, R] = Inject[Sig, S, R, A](Op.Strict(f, false))

  /**
   * WHAT A STRICT LEAF DOES AT RUN TIME when its `k` runs out of room (cont-safe-mode, specs/cont-js-depth.md stage 5):
   * `Auto` (the default) each platform's cheapest — re-execution on Scala.js, a fresh stack on the JVM and Native;
   * `Replay` re-execution everywhere (a body's part before its pending `k` runs again: pure or idempotent bodies);
   * `Safe` never re-execute — a fresh stack on the JVM and Native, the engine's stack on Scala.js, where the
   * guarantee is the compile-time one (`Cont.safe`). Set at start: `-Dokay.cont.mode=auto|replay|safe`, or here.
   */
  enum Mode:
    case Auto, Replay, Safe

  def mode: Mode = ContReplay.mode
  def setMode(m: Mode): Unit = ContReplay.set(m)

  /**
   * COMPILE-TIME SAFE SHIFTS in a scope: `import okay.Cont.safe.given`. Every `shift` body that uses `k` is CPS-
   * transformed — `k` is data, nothing waits on the host stack, nothing is re-executed, on every platform, side
   * effects run once and in order — or it is a compile error saying why. A body answering a program gets the lazy
   * `k`. The run-time mode cannot reach these bodies: no strict leaf is left.
   */
  final class Safe private[okay] () extends Shifts
  object safe:
    given Safe = Safe()

  /** COMPILE-TIME, a scope's opaque bodies NEVER re-executed (`import okay.Cont.noReplay.given`): they compile as
   * before, and their `k` is a barrier whatever the run-time mode — for a body with effects the macro cannot read */
  final class NoReplay private[okay] () extends Shifts
  object noReplay:
    given NoReplay = NoReplay()

  /** A SCOPE'S COMPILE-TIME CHOICE for its `shift`s, which `shift` takes as a parameter (so the import is a use):
   * `Default` with no import, `Safe` or `NoReplay` by one. Nothing at run time: the macro reads its type */
  sealed trait Shifts
  object Shifts:
    /** no choice in scope: the macro's default — a body it reads transformed, an opaque one a strict leaf. A given
     * of the IMPLICIT scope, so an imported `safe`/`noReplay` (the lexical scope, searched first) wins */
    object Default extends Shifts
    given default: Shifts = Default

  /**
   * an opaque body whose answer `S` is a PROGRAM and which calls `k` itself (cont-program-answer): its `k(a)`
   * returns at once — a `Delay` whose forcing runs `k`'s rest to the program that goes on, a run of its own.
   * The contract it changes: host side effects written after `k(a)` in the body run before `k`'s rest.
   */
  def programLeaf[A, S, R](f: (A => S) => R)(using p: Later[S]): Rep[A, S, R] =
    Inject[Sig, S, R, A](Op.Program(k => f(x => p.later(() => Delimited(Steps).force(k, x)))))

  /** `S` a program that can stand for itself unbuilt: a `Delay` of any `Freer`, `A ! F` among them */
  trait Later[S]:
    def later(make: () => S): S

  object Later:
    given freer[G[_, _, +_], S2, R2, X]: Later[Freer[G, S2, R2, X]] with
      def later(make: () => Freer[G, S2, R2, X]): Freer[G, S2, R2, X] = Delay(make)

  /** a tail body `k => { stats; k(v) }` as its value `v`, where `S` is `R`: a value, nothing else */
  def tailShiftSame[A, S, R](v: () => A)(using ev: S =:= R): Rep[A, S, R] =
    ev.flip.substituteCo[[s] =>> Rep[A, s, R]](Freer.delay[Sig, R, R, A](() => Return(v())))

  /** the same with no thunk, for a literal or a stable name */
  def tailPureSame[A, S, R](v: A)(using ev: S =:= R): Rep[A, S, R] =
    ev.flip.substituteCo[[s] =>> Rep[A, s, R]](Return(v))

  /** a tail body whose `S` is a proper subtype of `R`: `k`'s answer leaves as the body's */
  def tailShift[A, S, R](v: () => A)(using ev: S <:< R): Rep[A, S, R] =
    Freer.delay[Sig, S, R, A](() => tailPure(v()))

  /** the same with no thunk */
  def tailPure[A, S, R](v: A)(using ev: S <:< R): Rep[A, S, R] =
    lazyLeaf[A, S, R](k => Bind(Inject[Sig, R, R, S](Op.Resume[A, S, R](k, v)), (s: S) => Return[Sig, R, R](ev(s))))

  /**
   * an answer-using body (`k(1) + k(10)`) after `ContMacro`'s selective CPS transform (Rompf, Maier & Odersky,
   * ICFP 2009): a program over the lazy `k` answering `B`, at the level of the leaf whose body it is, which
   * answers `T` — the leaf's `R`. Built by `call` and `done`. Public for the macro's expansion; not an API.
   */
  opaque type Lazy[T, B] = Freer[Sig, T, T, B]

  /** the lazy `k` of an answer-using body: its captured continuation, from `A` to `S`, which only `call` applies */
  opaque type LazyK[A, S] = Delimited.Kont[Sig, A, S]

  /** the body's answer */
  def done[T, R](r: R): Lazy[T, R] = Return(r)

  /** `k(a)` then `rest`: `k` under a boundary of its own, which hands its `S` to `rest` (`Delimited.Resume`) */
  def call[A, S, T, R](k: LazyK[A, S], a: A, rest: S => Lazy[T, R]): Lazy[T, R] =
    Bind(Inject[Sig, T, T, S](Op.Resume[A, S, T](k, a)), rest)

  /**
   * `xs.map(f)` / `xs.foreach(f)` in an answer-using body whose `f` calls `k` (cont-stack-layer1-c (2)): `f` a
   * program over the lazy `k`, the elements in order, each a bind the machine runs. Public for the macro's expansion.
   */
  def traverse[T, X, B, R](xs: Iterable[X], f: X => Lazy[T, B], rest: List[B] => Lazy[T, R]): Lazy[T, R] =
    walk[T, X, B, List[B], R](xs.toList, Nil, (_, x) => f(x), goOn, (acc, b) => b :: acc, acc => rest(acc.reverse))

  /** `xs.foldLeft(z)(f)` in an answer-using body, `f` a program over the lazy `k`, the same way */
  def foldIn[T, X, B, R](xs: Iterable[X], z: B, f: (B, X) => Lazy[T, B], rest: B => Lazy[T, R]): Lazy[T, R] =
    walk[T, X, B, B, R](xs.toList, z, f, goOn, (_, b) => b, rest)

  /** a step of a loop over the lazy `k` (cont-stack-layer1-c, `while`): deferred to the machine, which forces it
   * in its own loop — an iteration that never calls `k` holds no host frame either */
  def later[T, R](step: () => Lazy[T, R]): Lazy[T, R] = Delay(step)

  /** `xs.exists(p)` (`want` true) / `xs.forall(p)` (`want` false) in an answer-using body: the elements in turn,
   * stopping at the first whose answer is `want` — `p` is not run for the elements after it */
  def existsIn[T, X, R](xs: Iterable[X], p: X => Lazy[T, Boolean], want: Boolean, rest: Boolean => Lazy[T, R]): Lazy[T, R] =
    walk[T, X, Boolean, Unit, R](LazyList.from(xs), (), (_, x) => p(x),
      (_, b) => if b == want then rest(want) else null, (_, _) => (), _ => rest(!want))

  /** `xs.find(p)` in an answer-using body, stopping at the first element `p` holds for */
  def findIn[T, X, R](xs: Iterable[X], p: X => Lazy[T, Boolean], rest: Option[X] => Lazy[T, R]): Lazy[T, R] =
    walk[T, X, Boolean, Unit, R](LazyList.from(xs), (), (_, x) => p(x),
      (x, b) => if b then rest(Some(x)) else null, (_, _) => (), _ => rest(None))

  /**
   * ONE WALK under every lowering of a collection in an answer-using body (cont-list-combinators-one-walk): the
   * elements in order, `step(s, x)` a program over the lazy `k` for each, whose answer either STOPS the walk
   * (`stop` answers the rest of the body) or goes on (`stop` answers null) with `s` advanced by `next`; at the
   * end, `end(s)`. RECURSION DEFERRED: the next element is a bind's continuation the machine runs, never a host
   * call. The caller picks the sequence: a `List` where every element is visited anyway (`map`, `foldLeft`), a
   * memoised `LazyList` where the walk may stop (`exists`, `find`) — a stop forces nothing after it, an infinite
   * receiver included. Either way a resumed `k` walks again from its own point, sharing no iterator.
   */
  private def walk[T, X, B, S, R](rem: LinearSeq[X], s: S, step: (S, X) => Lazy[T, B], stop: (X, B) => Lazy[T, R] | Null,
                                  next: (S, B) => S, end: S => Lazy[T, R]): Lazy[T, R] =
    if rem.isEmpty then end(s)
    else
      val x = rem.head
      Bind(step(s, x), (b: B) =>
        val done = stop(x, b)
        if done == null then walk[T, X, B, S, R](rem.tail, next(s, b), step, stop, next, end) else done.nn)

  /** a walk that never stops early (a lambda capturing nothing: one instance, made once) */
  private def goOn[X, B, T, R]: (X, B) => Lazy[T, R] | Null = (_, _) => null

  /** the leaf of an answer-using body: its program answers the leaf's `R`, at the leaf's level */
  def lazyLeaf[A, S, R](body: LazyK[A, S] => Lazy[R, R]): Rep[A, S, R] = Inject[Sig, S, R, A](Op.Lazily(body))
  /** a bind whose left side is a thunk forced by the machine: tail calls without JVM frames */
  def defer[A, B, S, T, R](thunk: () => Rep[A, T, R])(f: A => Rep[B, S, T]): Rep[B, S, R] =
    Freer.defer(thunk)(f)

  /** `defer` with nothing after it */
  def delay[A, S, R](thunk: () => Rep[A, S, R]): Rep[A, S, R] = Freer.delay(thunk)

  /** flatMap in prefix form: the extension and `Control[Cont]` both call this, so neither resolves into the other */
  def bind[A, B, S, S2, R](c: Rep[A, S, R])(f: A => Rep[B, S2, S]): Rep[B, S2, R] = Bind(c, f)

  /** map, as `Freer.map`'s node */
  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] = c.map(f)

  /**
   * apply to a continuation: the stack of continuations (`Delimited`), `k` the reset's `ret` — what it answers
   * goes back to whoever called a captured `k`, what a shift body answers leaves the reset. Typed throughout.
   */
  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R = Delimited(Steps).run(c, k)

  /** a Cont program performs no effect but its own */
  private[okay] type Sig = Op

  /** CONT'S OPERATIONS, an effect on the stack of continuations (`Delimited`): the leaves of shift in the forms
   * ContMacro picks, and the call of a lazy `k` */
  sealed trait Op[S, R, +A]

  object Op:
    /** an opaque body, given a strict `k`: a nested run, counted */
    final case class Strict[S, R, A](body: (A => S) => R, replayable: Boolean) extends Op[S, R, A]
    /** a body answering a program, given `k` itself, from which it builds its lazy `k` */
    final case class Program[S, R, A](body: Delimited.Kont[Op, A, S] => R) extends Op[S, R, A]
    /** an answer-using body after the CPS transform: a program over the lazy `k`, answering `R` at its level */
    final case class Lazily[S, R, A](body: Delimited.Kont[Op, A, S] => Freer[Op, R, R, R]) extends Op[S, R, A]
    /** `k(a)` as a node: `k` under a boundary of its own, which takes its `S` back here */
    final case class Resume[A, S, T](k: Delimited.Kont[Op, A, S], a: A) extends Op[T, T, S]

  /**
   * WHAT EACH DOES: Danvy & Filinski's shift/reset with answer-type modification. A body's answer leaves its
   * reset (its boundary in `Stack`); a call of `k` puts a boundary of its own under `k`, so what `k` answers
   * comes back to it.
   */
  private object Steps extends Delimited.Step[Op, Op]:
    def step[A, B, S, T, R, Z](op: Op[T, R, A], k: Frames[Op, A, B, S, T], m: Stack[Op, B, S, R, Z],
                               machine: Delimited[Op]): Delimited.Next[Op, Z] =
      op match
        case Op.Resume(k1, a) => k1.resume(a, k, m)
        case leaf =>
          val c = machine.closed(k, m)
          if c == null then throw IllegalStateException("a shift with no reset around it")
          leaf match
            // `replayable` false: never re-executed (`shiftLeafOnce`; one case, so the step stays small)
            case Op.Strict(body, replayable) => machine.strict(c, body, replayable)
            case Op.Program(body) => c.answer(body(c))
            case Op.Lazily(body) => c.instead(body(c))
            case Op.Resume(_, _) => throw IllegalStateException("unreachable: Resume is answered above")

  /**
   * is `c` already an answer? then go on from it with a tail call instead of a continuation node
   * (`Effects.handle`: a node a forwarded operation otherwise)
   */
  inline def onAnswer[A, S, B](c: Rep[A, S, S])(inline ifAnswer: A => B)
                                               (inline otherwise: => B): B =
    c match
      case Return(a) => ifAnswer(a)
      case _ => otherwise

  extension [A, S, R](c: Cont[A, S, R])
    def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = bind(c)(f)
    def map[B](f: A => B): Cont[B, S, R] = mapped(c)(f)
    infix def /(k: A => S): R = run(c)(k)
    // no `apply`: Generate.scala's `apply` wins in lexical scope; `c / k` is the spelling

  /**
   * `shift` with one type argument inside a direct block over Cont's diagonal; its own import, so
   * `shift: k => ...` elsewhere keeps resolving to the package-level one
   */
  object direct:

    /** the answer type of a diagonal block */
    trait AnswerOf[F[_]]:
      type R
      def apply[A](c: Cont[A, R, R]): F[A]

    object AnswerOf:
      given [R0]: AnswerOf[[X] =>> Cont[X, R0, R0]] with
        type R = R0
        def apply[A](c: Cont[A, R0, R0]): Cont[A, R0, R0] = c

    /** capture to the block's `reset`; `DummyImplicit` makes `shift[Int]` name `A` */
    inline def shift[A](using d: DummyImplicit)[F[_]]
                       (using inline ctx: DirectCtx[F])
                       (using a: AnswerOf[F])
                       (f: (A => a.R) => a.R): F[A] =
      a(Cont.shift[A, a.R, a.R](f))

  /** monadic reflection (Filinski, POPL 1994): any monad in direct style */
  object Monadic:

    extension [F[_] : Monad, A](m: F[A])
      /** the monadic value as a direct value */
      inline def reflect[B]: Cont[A, F[B], F[B]] =
        shift(k => m.flatMap(k))
      /** the symbolic `reflect` */
      inline def ?[B]: Cont[A, F[B], F[B]] = reflect[B]

    /** back into the monad */
    inline def reify[F[_], A, B](p: Cont[A, F[A], F[B]])(using M: Monad[F]): F[B] =
      p / (a => M.pure(a))

/** the stack-safe data instance */
given Control[Cont] with
  override inline def pure[A, R](a: A): A /> R = Cont.Pure(a)
  // the leaf, not the macro: a macro expanded here cycles with ContMacro
  override inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shiftLeaf(f)
  extension [A, S, R](m: Cont[A, S, R])
    // prefix form: `m / k` here would resolve to this override
    override inline infix def /(k: A => S): R = Cont.run(m)(k)
    override inline def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] =
      Cont.bind(m)(f)
    // overridden: the default `map` builds a `pure` per element
    override inline def map[B](f: A => B): Cont[B, S, R] = Cont.mapped(m)(f)

/** closures: the reference instance, fast, not stack-safe */
type Func[A, S, R] = (A => S) => R

given Control[Func] with
  override inline def pure[A, R](a: A): Func[A, R, R] = _(a)
  override inline def shift[A, S, R](f: (A => S) => R): Func[A, S, R] = f
  extension [A, S, R](m: Func[A, S, R])
    override inline infix def /(k: A => S): R = m(k)
    override inline def flatMap[B, S2](f: A => Func[B, S2, S]): Func[B, S2, R] =
      k => m(f(_)(k))
    // composed directly, no `pure` per element
    override inline def map[B](f: A => B): Func[B, S, R] = k => m(x => k(f(x)))
