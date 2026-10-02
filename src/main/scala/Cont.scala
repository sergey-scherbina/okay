package okay

import okay.Freer.{Return, Inject, Bind, Delay, Diag}
import scala.annotation.tailrec

/**
 * Danvy-Filinski's one-prompt `shift`/`reset` with answer-type modification: `M[A, S, R]` is `(A => S) => R`.
 * Instances: `Cont` (data, stack-safe) and `Func` (closures). The machine's interface is `Delimited`; `Control[Cont]` is built on it.
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

/**
 * THE MACHINE'S STACK (Dybvig, Peyton Jones & Sabry, JFP 2007): segments of frames split at
 * delimiters, so a capture takes segments and a resumption pushes them; frames are never copied.
 * `Frames` is one segment.
 */
enum Frames[F[_, _, +_], A, S, T, Z]:
  /** the empty segment */
  case End[F[_, _, +_], A, S]() extends Frames[F, A, S, S, A]

  /** a frame over the rest of the segment, joined as `Bind` joins */
  case Frame[F[_, _, +_], A, S, S2, T, Y, Z](f: A => Freer[Cont0.Row[F], S2, T, Y],
                                              rest: Frames[F, Y, S, S2, Z]) extends Frames[F, A, S, T, Z]

/** THE STACK: segments and delimiters. A captured `k` is one; applied, it is a resumption the machine pushes. */
enum Stack[F[_, _, +_], A, S, T, Z] extends (A => Freer[Cont0.Row[F], S, T, Z]):
  /** the bottom */
  case Done[F[_, _, +_], A, S]() extends Stack[F, A, S, S, A]

  /** a segment, then the rest */
  case Run[F[_, _, +_], A, S, S2, T, Y, Z](frames: Frames[F, A, S2, T, Y],
                                            below: Stack[F, Y, S, S2, Z]) extends Stack[F, A, S, T, Z]

  /**
   * THE DELIMITER `ret $ ·` (λ$): popped by `Return` (`ret` runs outside it), cut by `shift0`
   * (`k` carries it with `ret`). `reset` is `pure $ ·`.
   */
  case Dollar[F[_, _, +_], A, S, T, Y, Z](p: Cont0.Delimiter[Y, T], ret: A => Freer[Cont0.Row[F], T, T, Y],
                                          below: Stack[F, Y, S, T, Z]) extends Stack[F, A, S, T, Z]

  /** a resumption: `k` over the rest, O(1), taken apart as the loop reaches it */
  case Cat[F[_, _, +_], A, S, T, Y, S2, Z](k: Stack[F, A, S2, T, Y],
                                           below: Stack[F, Y, S, S2, Z]) extends Stack[F, A, S, T, Z]

  def apply(a: A): Freer[Cont0.Row[F], S, T, Z] = this match
    case Done() => Return(a)
    case _ => Delay(Frames.Resume(a, this))

object Frames:
  import Stack.{Done, Run, Dollar, Cat}

  /** `k(a)` as a `Delay`'s thunk: pushed by the machine, run by any other interpreter */
  final class Resume[F[_, _, +_], A, S, T, Z](val a: A, val k: Stack[F, A, S, T, Z]) extends (() => Freer[Cont0.Row[F], S, T, Z]):
    def apply(): Freer[Cont0.Row[F], S, T, Z] = Frames.enterAt[F, A, S, T, Z](a, k)

  /** one empty segment and one empty stack for every index (`Nil`'s pattern) */
  private val theEnd: End[Nothing, Any, Any] = End()
  private val theDone: Done[Nothing, Any, Any] = Done()
  private[okay] def noFrames[F[_, _, +_], A, S]: Frames[F, A, S, S, A] = theEnd.asInstanceOf[Frames[F, A, S, S, A]]
  private[okay] def noStack[F[_, _, +_], A, S]: Stack[F, A, S, S, A] = theDone.asInstanceOf[Stack[F, A, S, S, A]]

  /** a bind's continuation that is a `Stack`, or null */
  private[okay] def as[F[_, _, +_], A, S, T, Z](f: A => Freer[Cont0.Row[F], S, T, Z]): Stack[F, A, S, T, Z] = f match
    case st: Stack[?, ?, ?, ?, ?] => st.asInstanceOf[Stack[F, A, S, T, Z]]
    case _ => null

  private def resume[F[_, _, +_], S, T, Z](t: () => Freer[Cont0.Row[F], S, T, Z]): Resume[F, ?, S, T, Z] = t match
    case r: Resume[?, ?, ?, ?, ?] => r.asInstanceOf[Resume[F, ?, S, T, Z]]
    case _ => null

  /** two delimiters that are one object are one type (the generative-prompt axiom) */
  private final class Ident[Y, I, Y2, I2](val answer: Y =:= Y2, val index: I =:= I2)
  private val theSame = new Ident[Any, Any, Any, Any](<:<.refl, <:<.refl)
  private def identical[Y, I, Y2, I2](@annotation.unused a: Cont0.Delimiter[Y, I], @annotation.unused b: Cont0.Delimiter[Y2, I2]): Ident[Y, I, Y2, I2] =
    theSame.asInstanceOf[Ident[Y, I, Y2, I2]]

  private def runOf[F[_, _, +_], A, S, S2, T, Y, Z](fs: Frames[F, A, S2, T, Y], st: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = fs match
    case _: End[F, A, S2] @unchecked => st
    case _ => Run(fs, st)

  /** a `Cat` head as a non-`Cat` head (the rare paths: cut, installed, rootOf) */
  @tailrec private[okay] def uncat[F[_, _, +_], A, S, T, Z](st: Stack[F, A, S, T, Z]): Stack[F, A, S, T, Z] = st match
    case c: Cat[F, A, S, T, y, s2, Z] => c.k match
      case _: Done[F, A, T] @unchecked => uncat(c.below)
      case r: Run[F, A, `s2`, ?, T, ?, `y`] => Run(r.frames, Cat(r.below, c.below))
      case d: Dollar[F, A, `s2`, T, y1, `y`] => Dollar(d.p, d.ret, Cat(d.below, c.below))
      case i: Cat[F, A, `s2`, T, ?, ?, `y`] => uncat(Cat(i.k, Cat(i.below, c.below)))
    case _ => st

  /** `k` over `below`; `below` itself when `k` is empty */
  private def cat[F[_, _, +_], A, S, T, Y, S2, Z](k: Stack[F, A, S2, T, Y], below: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = k match
    case _: Done[F, A, T] @unchecked => below
    case _ => Cat(k, below)

  /** the prompts installed, for `NoPrompt` */
  @tailrec private def installed[F[_, _, +_]](st: Stack[F, ?, ?, ?, ?], acc: List[String] = Nil): List[String] = st match
    case c: Cat[F, ?, ?, ?, ?, ?, ?] @unchecked => installed(uncat(c), acc)
    case Run(_, below) => installed(below, acc)
    case Dollar(p, _, below) => installed(below, if p eq Cont0.boundary[Any, Any] then acc else p.label :: acc)
    case _ => acc.reverse

  /**
   * THE LOOP: run `p` to a head form — a value, or `Bind(op, stack)` for an operation nobody here answers.
   * Registers: focus, segment, stack.
   */
  def run[F[_, _, +_], S0, R, Z](p: Freer[Cont0.Row[F], S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] =
    machine[F, S0, R, Z, Z, S0](p, noStack[F, Z, S0])

  /** `k(a)` run now: the value straight into the registers */
  private[okay] def enterAt[F[_, _, +_], A, S0, R, Z](a: A, k: Stack[F, A, S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] =
    machine[F, S0, R, Z, A, R](Return[Cont0.Row[F], R, A](a), k)

  /** the machine: a focus over a stack */
  private def machine[F[_, _, +_], S0, R, Z, X, T](focus0: Freer[Cont0.Row[F], T, R, X], st0: Stack[F, X, S0, T, Z]): Freer[Cont0.Row[F], S0, R, Z] =
    type G = Cont0.Row[F]

    final class Next[X, T, S1, Y](val focus: Freer[G, T, R, X], val fs: Frames[F, X, S1, T, Y], val st: Stack[F, Y, S0, S1, Z])

    /** the general capture: walk to the delimiter, `k` the nodes above it with it */
    @tailrec def cut[X, Y, I, T, T2, C](sh: Cont0.Shift0[F, Y, I, T, R, X], all: Stack[F, X, S0, T, Z], st: Stack[F, C, S0, T2, Z], rev: Rev[F, X, T, T2, C]): Next[?, ?, ?, ?] = st match
      // no delimiter here: the capture goes out
      case Done() => null
      case c: Cat[F, C, S0, T2, ?, ?, Z] => cut(sh, all, uncat(c), rev)
      case r: Run[F, C, S0, ?, T2, ?, Z] => cut(sh, all, r.below, Rev.SnocRun(rev, r.frames))
      // the barrier (Flatt et al., ICFP 2007): `Delim.run`'s root, which no capture may cross
      case d: Dollar[F, C, S0, T2, ?, Z] if d.p eq Cont0.boundary[Any, Any] => throw NoPrompt(sh.at, sh.p.label, installed(all))
      case d: Dollar[F, C, S0, T2, y2, Z] =>
        if sh.p eq d.p then
          val same = identical(sh.p, d.p)
          val y = same.answer.flip
          val i = same.index.flip
          val k = y.liftCo[[a] =>> Stack[F, X, I, T, a]](
            i.liftCo[[t] =>> Stack[F, X, t, T, y2]](Rev.link(Rev.SnocDollar(rev, d.p, d.ret), noStack[F, y2, T2])))
          Next[Y, I, I, Y](sh.f(k), noFrames[F, Y, I],
            y.liftCo[[a] =>> Stack[F, a, S0, I, Z]](i.liftCo[[t] =>> Stack[F, y2, S0, t, Z]](d.below)))
        else
          cut(sh, all, d.below, Rev.SnocDollar(rev, d.p, d.ret))

    /** an operation of `F`, not of `Cont0` */
    def foreign(a: Freer[G, ?, ?, ?]): Boolean = a match
      case Inject(e) => !e.isInstanceOf[Cont0[?, ?, ?, ?]]
      case _ => false

    /** a capture: `nearest` for the usual one, else the walk */
    def capture[Y0, I0, X, T, S1, Y](sh: Cont0.Shift0[F, Y0, I0, T, R, X], fs: Frames[F, X, S1, T, Y], st: Stack[F, Y, S0, S1, Z]): Next[?, ?, ?, ?] = st match
      // the delimiter right under the live segment
      case d: Dollar[F, Y, S0, S1, y2, Z] if sh.p eq d.p => nearest(sh, fs, d.p, d.ret, d.below)
      // or at the head of a resumed `k`
      case c: Cat[F, Y, S0, S1, y, s2, Z] => c.k match
        case d: Dollar[F, Y, `s2`, S1, y2, `y`] if sh.p eq d.p => nearest(sh, fs, d.p, d.ret, cat(d.below, c.below))
        case _ => walk(sh, fs, st)
      case _ => walk(sh, fs, st)

    def walk[Y0, I0, X, T, S1, Y](sh: Cont0.Shift0[F, Y0, I0, T, R, X], fs: Frames[F, X, S1, T, Y], st: Stack[F, Y, S0, S1, Z]): Next[?, ?, ?, ?] =
      val all = runOf(fs, st)
      cut(sh, all, all, Rev.nil[F, X, T])

    /** `k` is the live segment over a copy of the delimiter. Inline: C2 refused it as a method at one arm. */
    inline def nearest[Y0, I0, X, T, S1, Y, y2](sh: Cont0.Shift0[F, Y0, I0, T, R, X], fs: Frames[F, X, S1, T, Y],
                                         p: Cont0.Delimiter[y2, S1], ret: Y => Freer[G, S1, S1, y2],
                                         below: Stack[F, y2, S0, S1, Z]): Next[?, ?, ?, ?] =
      val same = identical(sh.p, p)
      val y = same.answer.flip
      val i = same.index.flip
      val k = y.liftCo[[a] =>> Stack[F, X, I0, T, a]](i.liftCo[[t] =>> Stack[F, X, t, T, y2]](
        runOf(fs, Dollar[F, Y, S1, S1, y2, y2](p, ret, noStack[F, y2, S1]))))
      Next[Y0, I0, I0, Y0](sh.f(k), noFrames[F, Y0, I0],
        y.liftCo[[a] =>> Stack[F, a, S0, I0, Z]](i.liftCo[[t] =>> Stack[F, y2, S0, t, Z]](below)))

    /** a resumption: `k`'s head segment into the register, the rest of `k` over the live stack */
    def resume[A, T, S2, Y, S1, W](focus: Freer[G, T, R, A], k: Stack[F, A, S2, T, Y],
                                   fs: Frames[F, Y, S1, S2, W], st: Stack[F, W, S0, S1, Z]): Next[?, ?, ?, ?] =
      val live = runOf(fs, st)
      k match
        case kr: Run[F, A, S2, s3, T, y1, Y] => Next[A, T, s3, y1](focus, kr.frames, over(kr.below, live))
        case _ => Next[A, T, T, A](focus, noFrames[F, A, T], over(k, live))

    /** `k` over the live stack */
    def over[A, S2, T, Y](k: Stack[F, A, S2, T, Y], live: Stack[F, Y, S0, S2, Z]): Stack[F, A, S0, T, Z] = live match
      case _: Done[F, Y, S0] @unchecked => k
      case _ => cat(k, live)

    @tailrec def loop[X, T, S1, Y](focus: Freer[G, T, R, X], fs: Frames[F, X, S1, T, Y], st: Stack[F, Y, S0, S1, Z]): Freer[G, S0, R, Z] = focus match
      case b: Bind[G, T, ?, R, ?, X] => Frames.as(b.f) match
        case null => b.a match
          // a value under a bind: apply, no frame
          case r: Return[G, R, x0] => loop(b.f(r.a), fs, st)
          // an operation with nothing pushed: the head form already
          case a => fs match
            case _: End[F, X, S1] @unchecked => st match
              case _: Done[F, Y, S0] @unchecked if foreign(a) => focus
              case _ => loop(a, Frame(b.f, fs), st)
            case _ => loop(a, Frame(b.f, fs), st)
        // a stack as continuation: a resumption
        case ks =>
          val n = resume(b.a, ks, fs, st)
          loop(n.focus, n.fs, n.st)
      case r: Return[G, R, X] => fs match
        case fr: Frame[F, X, S1, s2, T, ?, Y] => loop(fr.f(r.a), fr.rest, st)
        case _: End[F, X, S1] @unchecked => st match
          // `$v`: pop the delimiter, run `ret`
          case d: Dollar[F, Y, S0, S1, y, Z] => loop(d.ret(r.a), noFrames[F, y, S1], d.below)
          // the next segment
          case rn: Run[F, Y, S0, ?, S1, ?, Z] => loop(focus, rn.frames, rn.below)
          // a `Cat`: its next node, in place
          case c: Cat[F, Y, S0, S1, y, s2, Z] => c.k match
            case _: Done[F, Y, S1] @unchecked => loop(focus, fs, c.below)
            case kr: Run[F, Y, `s2`, ?, S1, ?, `y`] => loop(focus, kr.frames, cat(kr.below, c.below))
            case d: Dollar[F, Y, `s2`, S1, y1, `y`] => loop(d.ret(r.a), noFrames[F, y1, S1], cat(d.below, c.below))
            case i: Cat[F, Y, `s2`, S1, ?, ?, `y`] => loop(focus, fs, Cat(i.k, Cat(i.below, c.below)))
          case _: Done[F, Y, S0] @unchecked => focus
      case d: Delay[G, T, R, X] => Frames.resume[F, T, R, X](d.thunk) match
        // a resumption is pushed, never forced
        case null => loop(d.thunk(), fs, st)
        case r: Resume[F, a, T, R, X] =>
          val n = resume(Return[G, R, a](r.a), r.k, fs, st)
          loop(n.focus, n.fs, n.st)
      // the two operations, by class
      case _ =>
        val e: G[T, R, X] = (focus: @unchecked) match
          case i: Inject[G, T, R, X] => i.a
          case i: Diag[G, R, X] => i.a
        e match
          case ds: Cont0.Dollar0[F, X, a, T, R] @unchecked =>
            loop[a, T, T, a](ds.body, noFrames[F, a, T], Dollar(ds.p, ds.ret, runOf(fs, st)))
          case sh: Cont0.Shift0[F, ?, ?, T, R, X] @unchecked => capture(sh, fs, st) match
            case n: Next[x, ?, ?, ?] => loop(n.focus, n.fs, n.st)
            // nobody here answers it: out, over the stack
            case null => Bind(focus, runOf(fs, st))
          case _ => Bind(focus, runOf(fs, st))

    val n = resume(focus0, st0, noFrames[F, Z, S0], noStack[F, Z, S0])
    loop(n.focus, n.fs, n.st)

/**
 * THE TWO OPERATIONS, λ$'s (Materzok & Biernacki): `Dollar0` is `ret $ body`, `Shift0` captures to its
 * delimiter (`k` with it) and its body takes the delimiter's place at the delimiter's index.
 */
enum Cont0[F[_, _, +_], T, R, +X]:
  case Dollar0[F[_, _, +_], Y, A, T, R](p: Cont0.Delimiter[Y, T],
                                        ret: A => Freer[Cont0.Row[F], T, T, Y],
                                        body: Freer[Cont0.Row[F], T, R, A]) extends Cont0[F, T, R, Y]
  case Shift0[F[_, _, +_], Y, I, T, R, X](p: Cont0.Delimiter[Y, I],
                                          f: Stack[F, X, I, T, Y] => Freer[Cont0.Row[F], I, R, Y],
                                          at: String) extends Cont0[F, T, R, X]

object Cont0:
  /** `Cont0` beside a signature `F` */
  type Row[F[_, _, +_]] = [T, R, X] =>> Cont0[F, T, R, X] | F[T, R, X]

  /** a fresh prompt, labelled with its line */
  def prompt[Y](using at: At): Prompt[Y] = new Prompt[Y]("prompt", at.where)

  /** a prompt with the index its delimiter is installed at: finding it by `eq` types `k` */
  opaque type Delimiter[Y, I] <: Prompt[Y] = Prompt[Y]

  /** THE INDEX CLAIM, made at the door that knows the index (Delim: Unit, Cont: Any, Stacked: the stack below) */
  def delimiter[Y, I](p: Prompt[Y]): Delimiter[Y, I] = p

  /** the barrier's prompt: `Delim.run` installs it, nobody can name it */
  private val theBoundary = new Prompt[Any]("boundary", "Delim.run")
  /** at any type: compared by `eq` only */
  def boundary[Y, I]: Delimiter[Y, I] = theBoundary.asInstanceOf[Delimiter[Y, I]]

  // the operators over these two are `Delimited`'s (Delimited.scala)

/** the reversed prefix the general cut builds, linked onto `Done`; frames shared */
private enum Rev[F[_, _, +_], A, T, S2, Y]:
  case Nil[F[_, _, +_], A, T]() extends Rev[F, A, T, T, A]
  case SnocRun[F[_, _, +_], A, T, S3, S2, Y0, Y](prev: Rev[F, A, T, S3, Y0], frames: Frames[F, Y0, S2, S3, Y]) extends Rev[F, A, T, S2, Y]
  case SnocDollar[F[_, _, +_], A, T, S2, Y0, Y](prev: Rev[F, A, T, S2, Y0], p: Cont0.Delimiter[Y, S2], ret: Y0 => Freer[Cont0.Row[F], S2, S2, Y]) extends Rev[F, A, T, S2, Y]

private object Rev:
  @tailrec def link[F[_, _, +_], A, S, T, S2, Y, Z](rev: Rev[F, A, T, S2, Y], st: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = rev match
    case Nil() => st
    case SnocRun(prev, frames) => link(prev, Stack.Run(frames, st))
    case SnocDollar(prev, p, ret) => link(prev, Stack.Dollar(p, ret, st))

  /** the empty prefix, one object */
  private val theNil: Nil[Nothing, Any, Any] = Nil()
  def nil[F[_, _, +_], A, T]: Rev[F, A, T, T, A] = theNil.asInstanceOf[Rev[F, A, T, T, A]]
