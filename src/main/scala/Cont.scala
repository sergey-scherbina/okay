package okay

import okay.Freer.{Return, Bind}

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
  inline def shift[A, S, R](inline f: (A => S) => R): Rep[A, S, R] = ${ ContMacro.shift('f) }

  /** delimit and run: `c / identity` */
  inline def reset[A, R](c: Rep[A, A, R]): R = run(c)(identity)

  /** an opaque body: run as it is, given a strict `k` (`Resumption`) */
  def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] =
    val body = f.asInstanceOf[(Any => Any) => Any]
    leaf((k: K) => Return(body(Resumption(k))))

  /** the one leaf: `shift0` to the run's root */
  private def leaf[A, S, R](clause: K => P): Rep[A, S, R] =
    typed(Delimited.machine[NoEffect].shift0[Any, Any, Any, Any, Any](rootAt)(clause)(using leafAt))

  /** a tail body `k => { stats; k(v) }` as its value `v`; `S <: R` (the evidence) makes the erasure claim sound */
  def tailShift[A, S, R](v: () => A)(using @annotation.unused ev: S <:< R): Rep[A, S, R] =
    typed(Freer.delay[Sig, Any, Any, Any](() => Return[Sig, Any, Any](v())))

  /** the same with no thunk, for a literal or a stable name */
  def tailPure[A, S, R](v: A)(using @annotation.unused ev: S <:< R): Rep[A, S, R] =
    typed(Return[Sig, Any, Any](v))

  /**
   * an answer-using body (`k(1) + k(10)`) after `ContMacro`'s selective CPS transform (Rompf, Maier & Odersky,
   * ICFP 2009): a program over the lazy `k`, built by `call` and `done`. Public for the macro's expansion; not an API.
   */
  opaque type Lazy[R] = Freer[Sig, Any, Any, Any]

  /** the body's answer */
  def done[R](r: R): Lazy[R] = Return(r)

  /** `k(a)` then `rest`: `k`'s nodes pushed by the machine */
  def call[A, S, R](k: A => S, a: A, rest: S => Lazy[R]): Lazy[R] =
    Bind(k.asInstanceOf[K](a), rest.asInstanceOf[Any => P])

  /** the leaf of an answer-using body */
  def lazyLeaf[A, S, R](body: (A => S) => Lazy[R]): Rep[A, S, R] =
    leaf(body.asInstanceOf[K => P])
  /** a bind whose left side is a thunk forced by the machine: tail calls without JVM frames */
  def defer[A, B, S, T, R](thunk: () => Rep[A, T, R])(f: A => Rep[B, S, T]): Rep[B, S, R] =
    Freer.defer(thunk)(f)

  /** `defer` with nothing after it */
  def delay[A, S, R](thunk: () => Rep[A, S, R]): Rep[A, S, R] = Freer.delay(thunk)

  /** flatMap in prefix form: the extension and `Control[Cont]` both call this, so neither resolves into the other */
  def bind[A, B, S, S2, R](c: Rep[A, S, R])(f: A => Rep[B, S2, S]): Rep[B, S2, R] = Bind(c, f)

  /** map, as `Freer.map`'s node */
  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] = c.map(f)

  /** apply to a continuation: a root `$` whose `ret` is `k` */
  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R =
    val r = Root(k.asInstanceOf[Any => Any], StackSwitch.firstRoom)
    answerOf(Frames.run[NoEffect, Any, Any, Any](Delimited.machine[NoEffect].dollar[Any, Any, Any, Any](rootAt)(r)(erased(c)))).asInstanceOf[R]

  // THE RUNNER: a Cont program runs on the frame machine under one root `$` per run; every leaf is a
  // `shift0` to it. A lazy `k` is pushed by the machine; a strict `k` is a nested run, counted, a fresh
  // stack at zero (`StackSwitch`).

  /** a Cont program performs no effect but its own */
  private[okay] type NoEffect = [S, R, X] =>> Nothing
  private[okay] type Sig = Cont0.Row[NoEffect]

  private type P = Freer[Sig, Any, Any, Any]
  private type K = Stack[NoEffect, Any, Any, Any, Any]

  /** the root prompt: one for every run */
  private val root: Prompt[Any] = new Prompt[Any]("Cont.run", "Cont.scala")
  /** the root as the machine's delimiter, at Cont's erased index */
  private val rootAt: Cont0.Delimiter[Any, Any] = Cont0.delimiter(root)
  /** where a leaf's capture says it was made */
  private val leafAt: At = At("Cont.shift")

  /** the root's `ret` (the user's `k`) and the run's room */
  private final class Root(val k: Any => Any, var room: Int) extends (Any => P):
    def apply(x: Any): P = Return(k(x))

  /** the strict `k`: `apply` runs `k`'s stack to a value, nested, counted (`force`) */
  private final class Resumption(k: K) extends (Any => Any):
    def apply(x: Any): Any = force(k, x)

  /** THE CLAIM: Cont's indexes are the facade's, every node built here, so the machine runs them erased */
  private def erased[A, S, R](c: Rep[A, S, R]): P = c.asInstanceOf[P]
  private def typed[A, S, R](p: P): Rep[A, S, R] = p.asInstanceOf[Rep[A, S, R]]

  /** a run's head form is its value: Cont has no other operation */
  private def answerOf(head: P): Any = head match
    case Return(v) => v
    case _ => throw IllegalStateException("a Cont program answered an operation: it has none")

  /** run `k` now, one level less of room; at zero on a fresh stack */
  private def force(k: K, x: Any): Any =
    val r = rootOf(k)
    val here = r.room - 1
    if here > 0 then nested(r, here, k, x)
    else StackSwitch.fresh(fresh => nested(r, fresh, k, x))

  /** `k` with `room` levels, the run's room restored after */
  private def nested(r: Root, room: Int, k: K, x: Any): Any =
    val saved = r.room
    r.room = room
    try enter(k, x) finally r.room = saved

  /** `x` into `k`'s nodes, run to a value */
  private def enter(k: K, x: Any): Any =
    answerOf(Frames.enterAt[NoEffect, Any, Any, Any, Any](x, k))

  /** the root at the bottom of `k`: a strict `k` always ends at it */
  @annotation.tailrec
  private def rootOf(k: Stack[NoEffect, ?, ?, ?, ?]): Root = k match
    case Stack.Dollar(p, r: Root, _) if p eq root => r
    case Stack.Dollar(_, _, below) => rootOf(below)
    case Stack.Run(_, below) => rootOf(below)
    case c: Stack.Cat[NoEffect, ?, ?, ?, ?, ?, ?] @unchecked => rootOf(Frames.uncat(c))
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
