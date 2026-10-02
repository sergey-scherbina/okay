package okay

import okay.Freer.{Return, Inject, Bind, Delay, Diag}
import scala.annotation.tailrec

/**
 * THE MACHINE — its interface here, its implementation below (delimited-machine, 2026-10-02): continuations
 * are DATA (a captured segment, resumed by an instruction), indexed by their answer types, multi-shot.
 * The strict-`k` bridge, a continuation as a host function, is Cont.scala's, not this file's.
 *
 * The interface: Dybvig, Peyton Jones & Sabry's `MonadDelimitedCont` (JFP 2007) in λ$'s variant.
 * Primitives: `delimiter` (newPrompt), `dollar` (pushPrompt, with `ret`), `shift0` (withSubCont, but `k`
 * keeps the delimiter and `ret`), `resume` (pushSubCont: a computation inside `k`). Derived: `reset`,
 * `shift`, `abort`. Instances: `Delimited.machine` and the tests' reference. `Control` is the one-prompt
 * user level, built on this.
 */
trait Delimited[M[_, _, _]] extends ParaMonad[[A, S, R] =>> M[S, R, A]]:

  /** a delimiter's name: answer `Y`, installed at index `I` */
  type Delimiter[Y, I]

  /** a captured stack, applied as a function */
  type SubCont[A, S, T, Z] <: A => M[S, T, Z]

  /** a fresh delimiter */
  def delimiter[Y, I](using At): Delimiter[Y, I]

  // a value: `pure[A, R](a): M[R, R, A]`, ParaMonad's own (the order Atkey writes, the value first)

  /** sequencing */
  def bind[A, B, S, T, R](m: M[T, R, A])(f: A => M[S, T, B]): M[S, R, B]

  /** ParaMonad's `flatMap` is `bind` */
  extension [A, S, R](m: M[S, R, A])
    def flatMap[B, S2](f: A => M[S2, S, B]): M[S2, R, B] = bind[A, B, S2, S, R](m)(f)

  /**
   * THE DOOR (delimited-one-door, the operator's "одна дверь в машину"): run `m` to its HEAD FORM — a value,
   * or the first operation the machine does not answer (an operation of the carrier's own signature, or a
   * capture to a delimiter `m` does not hold, going out), with the rest of `m` as its continuation. Every run
   * of the machine is this: `run` below, a nested run, a resumed strict `k`. Observationally the identity:
   * `runHead(m)` computes what `m` computes, in any context (the reference's own `runHead` is `m` itself,
   * and the differential oracle drives both through it).
   */
  def runHead[T, R, A](m: M[T, R, A]): M[T, R, A]

  /** the same door entered from a captured stack: `runHeadAt(k)(a)` is `runHead(k(a))` without building `k(a)`
   * (a resumption node per call: +92 KB and 1.15x on statePara through the strict-`k` bridge, measured) */
  def runHeadAt[A, S, T, Z](k: SubCont[A, S, T, Z])(a: A): M[S, T, Z]

  /** run to a value (`runCC`): `runHead` under a boundary; a capture without its delimiter is `NoPrompt` */
  def run[A](m: M[A, A, A]): A

  /** `ret $ body`: `ret` runs outside the delimiter and rides in `k` */
  def dollar[Y, A, T, R](d: Delimiter[Y, T])(ret: A => M[T, T, Y])(body: M[T, R, A]): M[T, R, Y]

  /** capture to `d`, `k` with it; the body takes its place */
  def shift0[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X]

  /** run `m` inside `k`; `k(a)` is `resume(k)(pure(a))` */
  def resume[A, S, T, R, Z](k: SubCont[A, S, T, Z])(m: M[T, R, A]): M[S, R, Z]


  /** `pure $ body` */
  def reset[T, R, A](d: Delimiter[A, T])(body: M[T, R, A]): M[T, R, A] =
    dollar[A, A, T, R](d)(a => pure[A, T](a))(body)

  /** `shift0` with the body under `reset` (S k.e = S0 k.<e>) */
  def shift[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X] =
    shift0[Y, I, T, R, X](d)(k => reset[I, R, Y](d)(f(k)))

  /** leave `d` with a value */
  def abort[Y, T, X](d: Delimiter[Y, T])(value: Y)(using At): M[T, T, X] =
    shift0[Y, T, T, T, X](d)(_ => pure[Y, T](value))

object Delimited:

  /** the frame machine (its class in `object Frames`, beside the loop it alone may start) */
  type Machine[F[_, _, +_]] = Frames.Machine[F]

  /** one stateless machine for every `F` (phantom signature) */
  def machine[F[_, _, +_]]: Machine[F] = Frames.instance[F]

// ======================================================================
// THE MACHINE ITSELF (delimited-machine, 2026-10-02): the segmented stack,
// its loop, and the two operations `Cont0 = Shift0 | Reset0`. Moved here
// verbatim from Cont.scala, so this file is the whole machine — its
// interface above, its implementation below — and Cont.scala is the
// one-prompt facade and the strict-`k` bridge built on it.
// ======================================================================

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

  /** the frame machine: `dollar`/`shift0` are its operations, `resume(k)(m)` is `Bind(m, k)` */
  final class Machine[F[_, _, +_]] private[Frames] () extends Delimited[[S, R, A] =>> Freer[Cont0.Row[F], S, R, A]]:
    type Delimiter[Y, I] = Cont0.Delimiter[Y, I]
    type SubCont[A, S, T, Z] = Stack[F, A, S, T, Z]

    def delimiter[Y, I](using at: At): Cont0.Delimiter[Y, I] = Cont0.delimiter(Cont0.prompt[Y])

    def pure[A, R](a: A): Freer[Cont0.Row[F], R, R, A] = Return(a)

    def bind[A, B, S, T, R](m: Freer[Cont0.Row[F], T, R, A])(f: A => Freer[Cont0.Row[F], S, T, B]): Freer[Cont0.Row[F], S, R, B] =
      Bind(m, f)

    /** the loop, the only start of it outside this object */
    def runHead[T, R, A](m: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], T, R, A] = Frames.run[F, T, R, A](m)

    // the loop entered directly, not through `enterAt`: one call level more on the strict-k road cost +12 KB a
    // statePara op (the JIT's escape analysis gave up on the focus `Return`), measured exact (ProbeDoorBytes)
    def runHeadAt[A, S, T, Z](k: Stack[F, A, S, T, Z])(a: A): Freer[Cont0.Row[F], S, T, Z] =
      Frames.machine[F, S, T, Z, A, T](Return[Cont0.Row[F], T, A](a), k)

    /** under the barrier; any head form but a value is an unhandled operation of `F` */
    def run[A](m: Freer[Cont0.Row[F], A, A, A]): A =
      runHead[A, A, A](reset[A, A, A](Cont0.boundary[A, A])(m)) match
        case Return(a) => a
        case _ => throw IllegalStateException("Delimited.machine.run: an operation of F was left unhandled")

    def dollar[Y, A, T, R](d: Cont0.Delimiter[Y, T])(ret: A => Freer[Cont0.Row[F], T, T, Y])
                          (body: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], T, R, Y] =
      Inject[Cont0.Row[F], T, R, Y](Cont0.Dollar0[F, Y, A, T, R](d, ret, body))

    /** a `ret` that captures nothing: one object per call site */
    override def reset[T, R, A](d: Cont0.Delimiter[A, T])(body: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], T, R, A] =
      Inject[Cont0.Row[F], T, R, A](Cont0.Dollar0[F, A, A, T, R](d, (a: A) => Return[Cont0.Row[F], T, A](a), body))

    def shift0[Y, I, T, R, X](d: Cont0.Delimiter[Y, I])(f: Stack[F, X, I, T, Y] => Freer[Cont0.Row[F], I, R, Y])
                             (using at: At): Freer[Cont0.Row[F], T, R, X] =
      Inject[Cont0.Row[F], T, R, X](Cont0.Shift0[F, Y, I, T, R, X](d, f, at.where))

    def resume[A, S, T, R, Z](k: Stack[F, A, S, T, Z])(m: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], S, R, Z] =
      Bind[Cont0.Row[F], S, T, R, A, Z](m, k)

    /** a run as a VALUE (`Own`): a `Delay`'s thunk any interpreter forces to `runHead(program)`, read back by `out`,
     * and a running machine steps into in its own loop (shift-stacked-key) — the door's lazy form */
    def owned[T, R, A, B](program: Freer[Cont0.Row[F], T, R, A])(out: Freer[Cont0.Row[F], T, R, A] => B): () => B =
      new Own[F, T, R, A, B](program, out, false)

    /** `owned` for a run whose ANSWER is the program that goes on (a `k` whose answer is a program,
     * cont-program-answer): forced, `out` reads the answer; stepped into, the running machine continues into it */
    def ownedFlat[T, R, A, B](program: Freer[Cont0.Row[F], T, R, A])(out: Freer[Cont0.Row[F], T, R, A] => B): () => B =
      new Own[F, T, R, A, B](program, out, true)

    /** the `ret` of the nearest delimiter `d` in a captured `k`, or null: how Cont's strict `k` finds its run's
     * root (`Cont.rootOf`) without walking the machine's stack itself */
    private[okay] def retOf(k: Stack[F, ?, ?, ?, ?], d: AnyRef): AnyRef | Null = Frames.retOf[F](k, d)

  private val theMachine: Machine[[S, R, X] =>> Nothing] = Machine()
  /** one stateless machine for every `F` (phantom signature) */
  private[okay] def instance[F[_, _, +_]]: Machine[F] = theMachine.asInstanceOf[Machine[F]]

  /** `k(a)` as a `Delay`'s thunk: pushed by the machine, run by any other interpreter */
  final class Resume[F[_, _, +_], A, S, T, Z](val a: A, val k: Stack[F, A, S, T, Z]) extends (() => Freer[Cont0.Row[F], S, T, Z]):
    def apply(): Freer[Cont0.Row[F], S, T, Z] = Frames.enterAt[F, A, S, T, Z](a, k)

  /**
   * A RUN AS A VALUE (shift-stacked-key, handle-frames): a `Delay`'s thunk holding what a running machine STEPS
   * INTO (`program`), and what anything else does when it forces it (`apply`). A run nested in a run — a keyed
   * `reset` in another's continuation, a handler in a handler's — is then one loop, not a stack frame.
   */
  abstract class Pending[F[_, _, +_], S, T, Z, B] extends (() => B):
    def program: Freer[Cont0.Row[F], S, T, Z]

  /** a machine run: forced, `program` on a machine of its own (`out` reads the head form back at the caller's type);
   * made only by the door's `owned` */
  final class Own[F[_, _, +_], S, T, Z, B] private[Frames] (val program: Freer[Cont0.Row[F], S, T, Z],
                                                            out: Freer[Cont0.Row[F], S, T, Z] => B,
                                                            val flat: Boolean) extends Pending[F, S, T, Z, B]:
    def apply(): B = out(Frames.run[F, S, T, Z](program))

  /** a `Pending` thunk's program at the running machine's row, or null */
  private def own[F[_, _, +_], S, T, Z](t: () => Freer[Cont0.Row[F], S, T, Z]): Freer[Cont0.Row[F], S, T, Z] | Null = t match
    // THE ONE CLAIM: the run's program is at the row of the program that holds its Delay (its residual
    // was typed into that row), and its delimiter indices are its own, closed by the frame it starts with
    // A FLAT run answers the program that goes on (Cont's program-answered `k`, cont-program-answer): stepped
    // into, the machine runs it and continues into its answer, in the same loop
    case o: Own[?, ?, ?, ?, ?] =>
      if o.flat then Bind(o.program.asInstanceOf[Freer[Cont0.Row[F], S, T, Any]], continueInto[F, S, S, Z])
      else o.program.asInstanceOf[Freer[Cont0.Row[F], S, T, Z]]
    // any other run (a handler, handle-frames): its program, a frame
    case o: Pending[?, ?, ?, ?, ?] => o.program.asInstanceOf[Freer[Cont0.Row[F], S, T, Z]]
    case _ => null

  /** a flat run's answer, the program itself, continued: one function for every index */
  private val theInto: Any => Any = (p: Any) => p
  private def continueInto[F[_, _, +_], S, T, Z]: Any => Freer[Cont0.Row[F], S, T, Z] =
    theInto.asInstanceOf[Any => Freer[Cont0.Row[F], S, T, Z]]

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
  @tailrec private def uncat[F[_, _, +_], A, S, T, Z](st: Stack[F, A, S, T, Z]): Stack[F, A, S, T, Z] = st match
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

  /** the `ret` of the nearest delimiter `d` in `st`, or null */
  @tailrec private def retOf[F[_, _, +_]](st: Stack[F, ?, ?, ?, ?], d: AnyRef): AnyRef | Null = st match
    case Dollar(p, ret, below) => if p eq d then ret else retOf(below, d)
    case Run(_, below) => retOf(below, d)
    case c: Cat[F, ?, ?, ?, ?, ?, ?] @unchecked => retOf(uncat(c), d)
    case _ => null

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
  private def run[F[_, _, +_], S0, R, Z](p: Freer[Cont0.Row[F], S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] =
    machine[F, S0, R, Z, Z, S0](p, noStack[F, Z, S0])

  /** `k(a)` run now: the value straight into the registers */
  private def enterAt[F[_, _, +_], A, S0, R, Z](a: A, k: Stack[F, A, S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] =
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
      // the barrier (Flatt et al., ICFP 2007): `Shift.run`'s root, which no capture may cross
      // ... except an operation on its way to its handler's frame, which an inner run forwarded as well
      case d: Dollar[F, C, S0, T2, ?, Z] if (d.p eq Cont0.boundary[Any, Any]) && !sh.p.isInstanceOf[Cont0.Handling[?]] =>
        throw NoPrompt(sh.at, sh.p.label, installed(all))
      case d: Dollar[F, C, S0, T2, y2, Z] =>
        if sh.p eq d.p then found(sh, d.p, Rev.link(Rev.SnocDollar(rev, d.p, d.ret), noStack[F, y2, T2]), d.below)
        else cut(sh, all, d.below, Rev.SnocDollar(rev, d.p, d.ret))

    /** the nearest handler frame on the stack that takes `op`, or null */
    @tailrec def handlerFor(op: Any, st: Stack[F, ?, ?, ?, ?]): Cont0.Handling[?] | Null = st match
      case c: Cat[F, ?, ?, ?, ?, ?, ?] @unchecked => handlerFor(op, uncat(c))
      case Run(_, below) => handlerFor(op, below)
      case Dollar(p, _, below) => p match
        case h: Cont0.Handling[?] if h.takes(op) => h
        case _ => handlerFor(op, below)
      case _ => null

    /** the operation as the `shift0` to its frame, the clause its body */
    def asShift0[T, X](h: Cont0.Handling[?], op: Any): Cont0.Shift0[F, Any, Any, T, R, X] =
      // THE CLAIM handle-frames makes: the frame's answer and index are the handler's own, which built both
      // the frame and the clause; the machine only carries them
      Cont0.Shift0[F, Any, Any, T, R, X](Cont0.delimiter[Any, Any](h.asInstanceOf[Prompt[Any]]),
        k => h.clause(op, k.asInstanceOf[Any => Any]).asInstanceOf[Freer[G, Any, R, Any]], h.label)

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
      found(sh, p, runOf(fs, Dollar[F, Y, S1, S1, y2, y2](p, ret, noStack[F, y2, S1])), below)

    /**
     * A capture that found its delimiter `p`, in both of its roads (`nearest`, the general `cut`): `k` — the
     * stack up to and including `p` — and the stack `below` it, both at `p`'s own indexes, re-typed at the
     * shift's by the generative-prompt claim (one object, one type), and the body run with `k` in that place.
     * Inline, as `nearest` is: this is the code both had written out, once.
     */
    inline def found[Y0, I0, X, T, y2, i2](sh: Cont0.Shift0[F, Y0, I0, T, R, X], p: Cont0.Delimiter[y2, i2],
                                           k: Stack[F, X, i2, T, y2], below: Stack[F, y2, S0, i2, Z]): Next[?, ?, ?, ?] =
      val same = identical(sh.p, p)
      val y = same.answer.flip
      val i = same.index.flip
      Next[Y0, I0, I0, Y0](sh.f(y.liftCo[[a] =>> Stack[F, X, I0, T, a]](i.liftCo[[t] =>> Stack[F, X, t, T, y2]](k))),
        noFrames[F, Y0, I0], y.liftCo[[a] =>> Stack[F, a, S0, I0, Z]](i.liftCo[[t] =>> Stack[F, y2, S0, t, Z]](below)))

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
        // a resumption is pushed, never forced; a run is stepped into, never started
        case null => own[F, T, R, X](d.thunk) match
          case null => loop(d.thunk(), fs, st)
          case p: Freer[G, T, R, X] @unchecked => loop(p, fs, st)
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
          // an operation of `F`: a handler frame below takes it, or out
          case op =>
            val h = if Cont0.Handling.ever then handlerFor(op, st) else null
            if h == null then Bind(focus, runOf(fs, st))
            else capture(asShift0[T, X](h, op), fs, st) match
              case n: Next[x, ?, ?, ?] => loop(n.focus, n.fs, n.st)
              case null => Bind(focus, runOf(fs, st))

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

  /** THE INDEX CLAIM, made at the door that knows the index (Shift: Unit, Cont: Any, Stacked: the stack below) */
  def delimiter[Y, I](p: Prompt[Y]): Delimiter[Y, I] = p

  /**
   * A HANDLER'S DELIMITER (handle-frames, specs/handle-frames.md): the frame `ret $ body` of a handler that
   * runs on the machine. It says which operations are its own; for one of them the machine makes the
   * operation a `shift0` to this frame whose body is `clause` — `k` the continuation up to and including the
   * frame, so a resumption re-installs it (a deep handler). Erased at the boundary: the subclass knows its
   * types and makes the one claim.
   */
  abstract class Handling[Y](name: String) extends Prompt[Y](name, "handler"):
    // from now on a machine looks for a frame before forwarding an operation
    if !Handling.ever then Handling.ever = true
    def takes(op: Any): Boolean
    def clause(op: Any, k: Any => Any): Any

  object Handling:
    /** a frame was ever pushed in this process: until then a machine forwards an operation without looking */
    @volatile private[okay] var ever: Boolean = false

  /** the barrier's prompt: `Shift.run` installs it, nobody can name it */
  private val theBoundary = new Prompt[Any]("boundary", "Shift.run")
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
