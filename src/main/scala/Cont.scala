package okay

import okay.Freer.{Return, Bind, Delay}
import scala.collection.LinearSeq

/**
 * Danvy-Filinski's one-prompt `shift`/`reset` with answer-type modification: `M[A, S, R]` is `(A => S) => R`.
 * Instances: `Cont` (data, stack-safe) and `Func` (closures). THIS FILE IS THE FACADE AND THE BRIDGE: the
 * machine — its interface and its implementation — is Delimited.scala; here a body's `k` is a strict host
 * function `A => S` (the bridge into the machine: `shiftLeaf`, the room, the nested run), and ContMacro
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
   * `shift`: one leaf, a `shift0` to the run's root. `ContMacro` picks the leaf's form at compile time:
   * a tail body is a value (`tailShift`/`tailPure`), an answer-using body a program over a lazy `k`
   * (`lazyLeaf`), anything else gets a strict `k` (`shiftLeaf`).
   */
  inline def shift[A, S, R](inline f: (A => S) => R): Rep[A, S, R] = ${ okay.macros.ContMacro.shift('f) }

  /** delimit and run: `c / identity` */
  inline def reset[A, R](c: Rep[A, A, R]): R = run(c)(identity)

  /** an opaque body: run as it is, given a strict `k` (`Resumption`) */
  def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] =
    leaf[A, S, R](k => Return(f(Resumption(k))))

  /**
   * an opaque body whose answer `S` is a PROGRAM and which calls `k` itself (cont-program-answer): its `k(a)`
   * returns at once — a `Delay` holding a lazy run of `k`'s rest (the machine's `ownedFlat`), no nested run.
   * Any interpreter forces it: one bounded run, answering the program that goes on. A RUNNING machine steps
   * into it and continues into that program in its own loop, so the rest of the body is that machine's frame.
   * The contract it changes: host side effects written after `k(a)` in the body run before `k`'s rest.
   */
  def programLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] =
    leaf[A, S, R](k => Return(f(Later(k))))

  /** the lazy `k` of a program-answered body: the machine's `Delay` node standing for the program `S` (the claim:
   * `ContMacro` picks this leaf only when `S` is a `Freer`, and the machine steps into its own node in any row) */
  private final class Later[A, S](k: K[A, S]) extends (A => S):
    def apply(x: A): S = claim(Delay[Sig, Any, Any, Any](M.ownedFlat[Any, Any, S, P[Any]](k(x))(answerProgram)))

  /** a run of `k`'s rest to its answer, which is the program that goes on (the same claim as `Later`'s) */
  private val answerProgram: P[Any] => P[Any] = head => claim(answerOf(head))

  /** the one leaf: `shift0` to the run's root. The root answers each leaf at that leaf's own types, which no
   * one `Delimiter[Y, I]` can state: the clause and the node are claimed (memory cont-facade-over-free, trap 2) */
  private def leaf[A, S, R](clause: K[A, S] => P[R]): Rep[A, S, R] =
    claim(M.shift0[Any, Any, Any, Any, A](rootAt)(claim[Stack[NoEffect, A, Any, Any, Any] => P[Any]](clause))(using leafAt))

  /** a tail body `k => { stats; k(v) }` as its value `v`; `S <: R` (the evidence) makes the claim sound */
  def tailShift[A, S, R](v: () => A)(using @annotation.unused ev: S <:< R): Rep[A, S, R] =
    claim(Freer.delay[Sig, Any, Any, A](() => Return[Sig, Any, A](v())))

  /** the same with no thunk, for a literal or a stable name */
  def tailPure[A, S, R](v: A)(using @annotation.unused ev: S <:< R): Rep[A, S, R] =
    claim(Return[Sig, Any, A](v))

  /**
   * an answer-using body (`k(1) + k(10)`) after `ContMacro`'s selective CPS transform (Rompf, Maier & Odersky,
   * ICFP 2009): a program over the lazy `k`, built by `call` and `done`, answering `R`. Public for the macro's
   * expansion; not an API.
   */
  opaque type Lazy[R] = P[R]

  /** the lazy `k` of an answer-using body: its captured stack, from `A` to `S`, which only `call` applies */
  opaque type LazyK[A, S] = K[A, S]

  /** the body's answer */
  def done[R](r: R): Lazy[R] = Return(r)

  /** `k(a)` then `rest`: `k`'s nodes pushed by the machine */
  def call[A, S, R](k: LazyK[A, S], a: A, rest: S => Lazy[R]): Lazy[R] =
    Bind(k(a), rest)

  /**
   * `xs.map(f)` / `xs.foreach(f)` in an answer-using body whose `f` calls `k` (cont-stack-layer1-c (2)): `f` a
   * program over the lazy `k`, the elements in order, each a bind the machine runs. Public for the macro's expansion.
   */
  def traverse[X, B, R](xs: Iterable[X], f: X => Lazy[B], rest: List[B] => Lazy[R]): Lazy[R] =
    walk[X, B, List[B], R](xs.toList, Nil, (_, x) => f(x), goOn, (acc, b) => b :: acc, acc => rest(acc.reverse))

  /** `xs.foldLeft(z)(f)` in an answer-using body, `f` a program over the lazy `k`, the same way */
  def foldIn[X, B, R](xs: Iterable[X], z: B, f: (B, X) => Lazy[B], rest: B => Lazy[R]): Lazy[R] =
    walk[X, B, B, R](xs.toList, z, f, goOn, (_, b) => b, rest)

  /** a step of a loop over the lazy `k` (cont-stack-layer1-c, `while`): deferred to the machine, which forces it
   * in its own loop — an iteration that never calls `k` holds no host frame either */
  def later[R](step: () => Lazy[R]): Lazy[R] = Delay(step)

  /** `xs.exists(p)` (`want` true) / `xs.forall(p)` (`want` false) in an answer-using body: the elements in turn,
   * stopping at the first whose answer is `want` — `p` is not run for the elements after it */
  def existsIn[X, R](xs: Iterable[X], p: X => Lazy[Boolean], want: Boolean, rest: Boolean => Lazy[R]): Lazy[R] =
    walk[X, Boolean, Unit, R](LazyList.from(xs), (), (_, x) => p(x),
      (_, b) => if b == want then rest(want) else null, (_, _) => (), _ => rest(!want))

  /** `xs.find(p)` in an answer-using body, stopping at the first element `p` holds for */
  def findIn[X, R](xs: Iterable[X], p: X => Lazy[Boolean], rest: Option[X] => Lazy[R]): Lazy[R] =
    walk[X, Boolean, Unit, R](LazyList.from(xs), (), (_, x) => p(x),
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
  private def walk[X, B, S, R](rem: LinearSeq[X], s: S, step: (S, X) => Lazy[B], stop: (X, B) => Lazy[R] | Null,
                               next: (S, B) => S, end: S => Lazy[R]): Lazy[R] =
    if rem.isEmpty then end(s)
    else
      val x = rem.head
      Bind(step(s, x), (b: B) =>
        val done = stop(x, b)
        if done == null then walk[X, B, S, R](rem.tail, next(s, b), step, stop, next, end) else done.nn)

  /** a walk that never stops early */
  private val never: (Any, Any) => Null = (_, _) => null
  private def goOn[X, B, R]: (X, B) => Lazy[R] | Null = never

  /** the leaf of an answer-using body */
  def lazyLeaf[A, S, R](body: LazyK[A, S] => Lazy[R]): Rep[A, S, R] =
    leaf(body)
  /** a bind whose left side is a thunk forced by the machine: tail calls without JVM frames */
  def defer[A, B, S, T, R](thunk: () => Rep[A, T, R])(f: A => Rep[B, S, T]): Rep[B, S, R] =
    Freer.defer(thunk)(f)

  /** `defer` with nothing after it */
  def delay[A, S, R](thunk: () => Rep[A, S, R]): Rep[A, S, R] = Freer.delay(thunk)

  /** flatMap in prefix form: the extension and `Control[Cont]` both call this, so neither resolves into the other */
  def bind[A, B, S, S2, R](c: Rep[A, S, R])(f: A => Rep[B, S2, S]): Rep[B, S2, R] = Bind(c, f)

  /** map, as `Freer.map`'s node */
  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] = c.map(f)

  /** apply to a continuation: a root `$` whose `ret` is `k`; the run answers what the leaves' bodies answer, `R` */
  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R =
    val r = Root(k, StackSwitch.firstRoom)
    claim(answerOf(M.runHead[Any, Any, Any](M.dollar[Any, A, Any, Any](rootAt)(r)(claim[P[A]](c)))))

  // THE RUNNER: a Cont program runs on the frame machine under one root `$` per run; every leaf is a
  // `shift0` to it. A lazy `k` is pushed by the machine; a strict `k` is a nested run, counted, a fresh
  // stack at zero (`StackSwitch`).

  /** a Cont program performs no effect but its own */
  private[okay] type NoEffect = [S, R, X] =>> Nothing
  private[okay] type Sig = Cont0.Row[NoEffect]

  /** a program on the machine answering `X`, its answer-type indexes erased: Cont's live on the facade */
  private type P[+X] = Freer[Sig, Any, Any, X]
  /** a captured `k` from `A` to `S`, at the same erased indexes */
  private type K[A, S] = Stack[NoEffect, A, Any, Any, S]

  /** the machine, through its one door */
  private val M: Delimited.Machine[NoEffect] = Delimited.machine[NoEffect]

  /** the root prompt: one for every run */
  private val root: Prompt[Any] = new Prompt[Any]("Cont.run", "Cont.scala")
  /** the root as the machine's delimiter, at Cont's erased index */
  private val rootAt: Cont0.Delimiter[Any, Any] = Cont0.delimiter(root)
  /** where a leaf's capture says it was made */
  private val leafAt: At = At("Cont.shift")

  /** the root's `ret` (the user's `k`) and the run's room */
  private final class Root[A, S](val k: A => S, var room: Int) extends (A => P[S]):
    def apply(x: A): P[S] = Return(k(x))

  /** the strict `k`: `apply` runs `k`'s stack to a value, nested, counted (`force`) */
  private final class Resumption[A, S](k: K[A, S]) extends (A => S):
    def apply(x: A): S = force(k, x)

  /**
   * THE CLAIM, the file's only cast: Cont's answer types are the facade's, every node is built here, and the
   * machine runs them at erased indexes. Every crossing between the two goes through here — a leaf's clause and
   * node (`leaf`), a tail body's value (`tailShift`/`tailPure`), a program answer (`Later`), a run's tree and
   * answer (`run`) — and nowhere else.
   */
  private def claim[X](v: Any): X = v.asInstanceOf[X]

  /** a run's head form is its value: Cont has no other operation */
  private def answerOf[X](head: P[X]): X = head match
    case Return(v) => v
    case _ => throw IllegalStateException("a Cont program answered an operation: it has none")

  /** run `k` now, one level less of room; at zero on a fresh stack */
  private def force[A, S](k: K[A, S], x: A): S =
    val r = rootOf(k)
    val here = r.room - 1
    if here > 0 then nested(r, here, k, x)
    else StackSwitch.fresh(fresh => nested(r, fresh, k, x))

  /** `k` with `room` levels, the run's room restored after */
  private def nested[A, S](r: Root[?, ?], room: Int, k: K[A, S], x: A): S =
    val saved = r.room
    r.room = room
    try enter(k, x) finally r.room = saved

  /** `x` into `k`'s nodes, run to a value, through the door's resumption form */
  private def enter[A, S](k: K[A, S], x: A): S =
    answerOf(M.runHeadAt[A, Any, Any, S](k)(x))

  /** the root at the bottom of `k`: a strict `k` always ends at it (the machine reads its own stack) */
  private def rootOf(k: K[?, ?]): Root[?, ?] = M.retOf(k, root) match
    case r: Root[?, ?] => r
    case _ => throw IllegalStateException("a strict k without its run's root: only a leaf makes one, and a leaf cuts to the root")

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
