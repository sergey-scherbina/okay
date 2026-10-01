package okay

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/**
 * Delimited control as an effect, multi-prompt (Dybvig, Peyton Jones & Sabry, JFP 2007): a prompt is a
 * first-class tag carrying its answer type; captures name it. The doors here build `Cont0` operations;
 * the machine is `Delimited.machine` (Cont.scala, Delimited.scala).
 */

/** a delimiter's name: answer type `R`, identity by allocation, labelled for diagnostics */
final class Prompt[R](val what: String, val where: String):
  /** `what @ where`, joined only when asked */
  def label: String = s"$what @ $where"
  override def toString: String = label

/** a `Cont0` operation seen from a unary row; the doors build it at `Cont0.Row[Lift[F]]` and re-type it (`in`/`out`) */
type Delim[+A] = Cont0[?, ?, ?, A]

/** a capture naming a prompt not installed on this machine, with the ones that are */
final class NoPrompt(val from: String, val wanted: String, val installed: List[String])
  extends RuntimeException(NoPrompt.say(from, wanted, installed))

object NoPrompt:
  def say(from: String, wanted: String, installed: List[String]): String =
    val stack =
      if installed.isEmpty then "  (none: this machine has no delimiter installed)"
      else installed.map("  " + _).mkString("\n")
    s"""|the capture at $from named the prompt '$wanted',
        |which is not on the stack of the machine running it.
        |Installed here, innermost first:
        |$stack
        |
        |ONE `Delim.run` PER PROGRAM. A machine owns one prompt stack,
        |so a delimiter installed by an INNER run cannot be reached
        |from the outer one, or the other way about. The combinators
        |that run a machine are `delimited`, `collect`, `resumable`;
        |the ones that install a delimiter on the machine already
        |running are `scope`, `collecting`, `pausing`. See
        |docs/continuations-in-practice.md, "The second rule: one
        |machine".""".stripMargin

object Delim {

  /** evidence that the row has no `Delim` yet: a second machine in one row cannot see the first's prompts */
  @implicitNotFound("this row already contains Delim, so this would start a SECOND machine, and a capture cannot cross from one machine's prompt stack to another's.\nUse the nested form, which installs a delimiter on the machine already running:\n  delimited -> scope,   collect -> collecting,   resumable -> pausing\n(docs/continuations-in-practice.md, \"The second rule: one machine\")")
  final class OneMachine[F[+_]] private[Delim] ()
  object OneMachine:
    /** membership by `<:<` on a union, not `Row.In`: the latter crashes dotty on an abstract row */
    given fresh[F[+_]](using NotGiven[Delim[Any] <:< F[Any]]): OneMachine[F] =
      new OneMachine[F]()

  /** a fresh prompt, labelled with its line */
  def prompt[R](using at: At): Prompt[R] = named[R]("prompt")

  /** a prompt labelled by its door and line */
  private def named[R](what: String)(using at: At): Prompt[R] =
    new Prompt[R](what, at.where)

  /** the row the machine runs a `Delim + F` program in, and its programs */
  type Ro[F[+_]] = Cont0.Row[Freer.Lift[F]]
  type U[F[+_], A] = Freer[Ro[F], Unit, Unit, A]

  /** THE DOORS' CLAIM: a `Delim + F` program is a `U[F, A]` at the same erasure (only the machine reads `Cont0`) */
  private[okay] def in[F[+_], A](p: A ! Delim + F): U[F, A] = p.asInstanceOf[U[F, A]]
  private[okay] def out[F[+_], A](p: U[F, A]): A ! Delim + F = p.asInstanceOf[A ! Delim + F]
  private def inF[F[+_], A, B](f: A => B ! Delim + F): A => U[F, B] = f.asInstanceOf[A => U[F, B]]
  private def clause[F[+_], A, R](f: (A => R ! Delim + F) => R ! Delim + F): Stack[Freer.Lift[F], A, Unit, Unit, R] => U[F, R] =
    f.asInstanceOf[Stack[Freer.Lift[F], A, Unit, Unit, R] => U[F, R]]

  /** every unstacked program is at `Unit`, so every delimiter it installs is */
  private[okay] def atUnit[R](p: Prompt[R]): Cont0.Delimiter[R, Unit] = Cont0.delimiter(p)

  /** `reset` at `p` */
  def push[R, F[+_]](p: Prompt[R])(body: R ! Delim + F): R ! Delim + F =
    out(Delimited.machine[Freer.Lift[F]].reset[Unit, Unit, R](atUnit(p))(in(body)))

  /** `ret $ body` at `p`: `ret` runs outside, a `shift0` to `p` takes it along */
  def dollar[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! Delim + F)(body: R0 ! Delim + F): R ! Delim + F =
    out(Delimited.machine[Freer.Lift[F]].dollar[R, R0, Unit, Unit](atUnit(p))(inF(ret))(in(body)))


  /** capture to `p`; the body runs under `p`, `k` re-installs it */
  def shift[R, A, F[+_]](p: Prompt[R])
                        (f: (A => R ! Delim + F) => R ! Delim + F)(using at: At): A ! Delim + F =
    out(Delimited.machine[Freer.Lift[F]].shift[R, Unit, Unit, Unit, A](atUnit(p))(clause(f)))

  /** the body consumes `p`; `k` re-installs it */
  def shift0[R, A, F[+_]](p: Prompt[R])
                         (f: (A => R ! Delim + F) => R ! Delim + F)(using at: At): A ! Delim + F =
    out(Delimited.machine[Freer.Lift[F]].shift0[R, Unit, Unit, Unit, A](atUnit(p))(clause(f)))

  /** a fresh prompt, a block under it, run */
  def reset[R, F[+_]](body: Prompt[R] => R ! Delim + F)
                     (using om: OneMachine[F], at: At): R ! F =
    val p = named[R]("reset")(using at)
    run(push(p)(body(p)))

  /** evidence that a delimiter is installed: only `delimited`/`scope` make one, so a capture through it cannot miss */
  final class Prompted[R] private[Delim] (val prompt: Prompt[R]):
    /** the prompt's answer type */
    type Res = R

  /** a delimiter installed on the machine already running (the nested half of `delimited`) */
  def scope[R, F[+_]](body: Prompted[R] ?=> R ! Delim + F)(using At): R ! Delim + F =
    scopeAs("scope")(body)

  /** `scope` labelled by its door */
  private def scopeAs[R, F[+_]](what: String)(body: Prompted[R] ?=> R ! Delim + F)
                               (using At): R ! Delim + F =
    val p = named[R](what)
    push(p)(body(using new Prompted[R](p)))

  /** a fresh delimiter and the machine: the root of a `Prompted` block */
  def delimited[R, F[+_]](body: Prompted[R] ?=> R ! Delim + F)
                         (using om: OneMachine[F], at: At): R ! F =
    run(scopeAs("delimited")(body))(using om)

  /** `dollar` with the evidence in scope */
  def dollar[R0, R, F[+_]](ret: R0 => R ! Delim + F)(body: Prompted[R] ?=> R0 ! Delim + F)
                          (using at: At): R ! Delim + F =
    val p = named[R]("dollar")
    dollar[R0, R, F](p)(ret)(body(using new Prompted[R](p)))

  /** capture to the delimiter in force */
  def shift[R, A, F[+_]](using in: Prompted[R])
                          (f: (A => R ! Delim + F) => R ! Delim + F)(using At): A ! Delim + F =
    shift[R, A, F](in.prompt)(f)

  /** `shift` in a direct block, one type argument; the answer type and row come from the evidence and the block */
  inline def shift[A](using in: Prompted[?])[F[_]]
                         (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                         (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    Delimited.machine[Freer.Lift[Pure]].shift[in.Res, Unit, Unit, Unit, A](Cont0.delimiter[in.Res, Unit](in.prompt))(
      f.asInstanceOf[Stack[Freer.Lift[Pure], A, Unit, Unit, in.Res] => U[Pure, in.Res]]).asInstanceOf[A ! rw.R]

  /** the 0-variant */
  def shift0[R, A, F[+_]](using in: Prompted[R])
                           (f: (A => R ! Delim + F) => R ! Delim + F)(using At): A ! Delim + F =
    shift0[R, A, F](in.prompt)(f)

  /** the 0-variant in a direct block */
  inline def shift0[A](using in: Prompted[?])[F[_]]
                          (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                          (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    Delimited.machine[Freer.Lift[Pure]].shift0[in.Res, Unit, Unit, Unit, A](Cont0.delimiter[in.Res, Unit](in.prompt))(
      f.asInstanceOf[Stack[Freer.Lift[Pure], A, Unit, Unit, in.Res] => U[Pure, in.Res]]).asInstanceOf[A ! rw.R]

  /** abort to the delimiter in force */
  def abort[R, A, F[+_]](using in: Prompted[R])(value: R)(using At): A ! Delim + F =
    abort[R, A, F](in.prompt)(value)

  // THE PATTERNS: four shapes that earn a capture, each named for what it does. They capture with `shift0`:
  // their bodies never capture to the same prompt again.

  /** 1 · leave early with an answer */
  inline def exit(using in: Prompted[?])[F[_]]
                 (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                 (value: in.Res): Unit ! rw.R =
    shift0[Unit](using in)(_ => okay.pure(value))

  /** 2 · a push API read as a pull: the evidence `emit` captures to, sealed */
  sealed abstract class Emitting[A]:
    type Elem = A
    /** the prompt's answer: a list for `collect`, a state-passing function for `collectUntil` */
    type Res
    // not private: `emit` is inline and reaches it
    val in: Prompted[Res]
    /** what one emit does with the rest of the producer */
    def onEmit[X[+_]](a: A)(k: Unit => Res ! X): Res ! X

  /** `collect`: cons on the way back */
  private final class Listing[A](prompted: Prompted[List[A]]) extends Emitting[A]:
    type Res = List[A]
    val in: Prompted[List[A]] = prompted
    def onEmit[X[+_]](a: A)(k: Unit => List[A] ! X): List[A] ! X = k(()).map(a :: _)

  /** `collectUntil`: the fold's state passed down through the prompt's answer; stops without resuming */
  private final class Stopping[A, S, R, G[+_]](prompted: Prompted[S => R ! G], fo: FoldUntil[A, S, R])
    extends Emitting[A]:
    type Res = S => R ! G
    val in: Prompted[S => R ! G] = prompted
    def onEmit[X[+_]](a: A)(k: Unit => (S => R ! G) ! X): (S => R ! G) ! X =
      okay.pure((s: S) => {
        val s2 = fo.add(s, a)
        if fo.done(s2) then okay.pure[G, R](fo.end(s2))
        else Stopping.atRow[R, X, G](k(()).flatMap(f => Stopping.atRow[R, G, X](f(s2))))
      })

  private object Stopping:
    /** THE ONE CAST: the block's row and `collectUntil`'s are one row the types cannot join */
    def atRow[R, X[+_], Y[+_]](p: R ! X): R ! Y = p.asInstanceOf[R ! Y]

  /** everything the body emitted, in order */
  def collect[A, F[+_]](body: Emitting[A] ?=> Unit ! Delim + F)
                       (using om: OneMachine[F], at: At): List[A] ! F =
    run(collectAs("collect")(body))(using om)

  /** the nested half of `collect` */
  def collecting[A, F[+_]](body: Emitting[A] ?=> Unit ! Delim + F)
                          (using At): List[A] ! Delim + F =
    collectAs("collecting")(body)

  private def collectAs[A, F[+_]](what: String)(body: Emitting[A] ?=> Unit ! Delim + F)
                                 (using At): List[A] ! Delim + F =
    scopeAs[List[A], F](what)(
      body(using new Listing[A](summon[Prompted[List[A]]]))
        .map(_ => List.empty[A]))

  /** emit into a `FoldUntil` that may stop the producer early */
  def collectUntil[A, S, R, F[+_]](using fo: FoldUntil[A, S, R])
                                   (body: Emitting[A] ?=> Unit ! Delim + F)
                                   (using om: OneMachine[F], at: At): R ! F =
    if fo.done(fo.init) then okay.pure(fo.end(fo.init))
    else run(collectUntilAs("collectUntil")(fo)(body))(using om)

  /** the nested half of `collectUntil` */
  def collectingUntil[A, S, R, F[+_]](using fo: FoldUntil[A, S, R])
                                      (body: Emitting[A] ?=> Unit ! Delim + F)
                                      (using At): R ! Delim + F =
    if fo.done(fo.init) then okay.pure(fo.end(fo.init))
    else collectUntilAs("collectingUntil")(fo)(body)

  private def collectUntilAs[A, S, R, F[+_]](what: String)(fo: FoldUntil[A, S, R])
                                            (body: Emitting[A] ?=> Unit ! Delim + F)
                                            (using At): R ! Delim + F =
    type G[+X] = (Delim + F)[X]
    scopeAs[S => R ! G, F](what)(
      body(using new Stopping[A, S, R, G](summon[Prompted[S => R ! G]], fo))
        // the producer ended: the state's own answer
        .map(_ => (s: S) => okay.pure[G, R](fo.end(s))))
      // the first emit's function applied to the start
      .flatMap(f => f(fo.init))

  /** emit one value into the `collect` in force */
  inline def emit(using e: Emitting[?])[F[_]]
                 (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                 (a: e.Elem): Unit ! rw.R =
    shift0[Unit](using e.in)(k => e.onEmit(a)(k))

  /** 3 · stop in the middle, carry on later: asking with the rest as `resume`, or finished */
  enum Paused[Q, A, R, G[+_]]:
    /** a question and the rest of the program */
    case Ask[Q, A, R, G[+_]](question: Q, resume: A => Paused[Q, A, R, G] ! G,
                             at: String)
      extends Paused[Q, A, R, G]
    case Done[Q, A, R, G[+_]](value: R) extends Paused[Q, A, R, G]

  /** a dialogue over `Delim + F` */
  type Dialogue[Q, A, R, F[+_]] = Paused[Q, A, R, Delim + F]

  object Paused:
    extension [Q, A, R, G[+_]](p: Paused[Q, A, R, G])
      /** the answer, if finished */
      def finished: Option[R] = p match
        case Done(r) => Some(r)
        case _ => None
      /** the question, if asking */
      def asking: Option[Q] = p match
        case Ask(q, _, _) => Some(q)
        case _ => None
      /** where it asked */
      def where: Option[String] = p match
        case Ask(_, _, at) => Some(at)
        case _ => None

  /** evidence for a block that may pause; question and answer types as members */
  final class Asking[Q, A, R, G[+_]] private[Delim] (prompted: Prompted[Paused[Q, A, R, G]]):
    type Qst = Q
    type Ans = A
    type Fin = R
    type Row[+X] = G[X]
    /** not private: `pause` is inline */
    val in: Prompted[Paused[Qst, Ans, Fin, Row]] = prompted

  /** run `body` until it pauses or finishes */
  def resumable[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                               (using om: OneMachine[F], at: At): Dialogue[Q, A, R, F] ! F =
    run(pausingAs("resumable")(body))(using om)

  /** the nested half of `resumable` */
  def pausing[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                             (using At): Dialogue[Q, A, R, F] ! Delim + F =
    pausingAs("pausing")(body)

  private def pausingAs[Q, A, R, F[+_]](what: String)
                                       (body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                                       (using At): Dialogue[Q, A, R, F] ! Delim + F =
    scopeAs[Dialogue[Q, A, R, F], F](what)(
      body(using new Asking[Q, A, R, Delim + F](summon[Prompted[Dialogue[Q, A, R, F]]]))
        .map(Paused.Done[Q, A, R, Delim + F](_)))

  /** ask and wait for the answer, in a direct block */
  inline def pause(using s: Asking[?, ?, ?, ?])[F[_]]
                  (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                  (q: s.Qst): s.Ans ! rw.R =
    shift[s.Ans](using s.in)(k => okay.pure(Paused.Ask(q,
      // THE ONE CAST: only `resumable` makes an `Asking`, so the block's row is its row
      k.asInstanceOf[s.Ans => Paused[s.Qst, s.Ans, s.Fin, s.Row] ! s.Row],
      at.where)))

  /** `pause` outside a direct block, for `for` and natural transformations */
  def ask[Q, A, R, F[+_]](q: Q)(using s: Asking[Q, A, R, Delim + F], at: At)
                         : A ! Delim + F =
    shift[Paused[Q, A, R, Delim + F], A, F](using s.in)(k =>
      okay.pure(Paused.Ask(q, k, at.where)))

  /** answer every question with `answer`, to the end */
  def drive[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F])(answer: Q => A ! F)
                           (using OneMachine[F]): R ! F =
    p match
      case Paused.Done(r) => okay.pure(r)
      case Paused.Ask(q, resume, _) =>
        answer(q).flatMap(a =>
          run(resume(a)).flatMap(drive[Q, A, R, F](_)(answer)))

  /**
   * persisting a dialogue: keep the JOURNAL of answers, not the continuation (a closure), and re-derive
   * where it stands by replaying the program
   */
  type Journal[A] = List[A]

  /** one more answer, appended to the journal */
  def answer[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F], j: Journal[A])(a: A)(using OneMachine[F])
                            : (Dialogue[Q, A, R, F], Journal[A]) ! F =
    p match
      case Paused.Ask(_, resume, _) => run(resume(a)).map(next => (next, j :+ a))
      case done => okay.pure((done, j))     // nobody asked; nothing to record

  /** where the dialogue stands, from its program and its journal */
  def replay[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                            (using OneMachine[F], Replayable[Delim + F], At)
                            (j: Journal[A]): Dialogue[Q, A, R, F] ! F =
    j.foldLeft(resumable[Q, A, R, F](body)): (acc, a) =>
      acc.flatMap:
        case Paused.Ask(_, resume, _) => run(resume(a))
        case done => okay.pure(done)        // more answers than questions

  /** 4 · do something on the way back: the rest of the block, then `f` on its answer */
  inline def onReturn(using in: Prompted[?])[F[_]]
                     (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                     (f: in.Res => in.Res): Unit ! rw.R =
    shift0[Unit](using in)(k => k(()).map(f))

  /** `Delim`'s operations are `Cont0`'s */
  given Effect[Delim] = new Effect[Delim]:
    def test(x: Any): Boolean = x.isInstanceOf[Cont0[?, ?, ?, ?]]

  /** run on the machine under the barrier: a capture with no delimiter is `NoPrompt` */
  def run[R, F[+_]](prog: R ! Delim + F)(using OneMachine[F]): R ! F =
    Stacked.bounded[R, F](in(prog))

  /** run without the barrier: a capture with no delimiter goes out, for a machine outside */
  def runNested[R, F[+_]](prog: R ! Delim + F)(using Row.In[Delim, F]): R ! F =
    Stacked.residual[R, F](Frames.run[Freer.Lift[F], Unit, Unit, R](in(prog)))

  /** drop the continuation and answer `value` at `p` */
  def abort[R, A, F[+_]](p: Prompt[R])(value: R)(using At): A ! Delim + F =
    out(Delimited.machine[Freer.Lift[F]].abort[R, Unit, A](atUnit(p))(value))


  // THE PROMPT STACK IN THE TYPE: the installed prompts are a lexical given, so a shift to a prompt not on
  // it (none, foreign, escaped) is a compile error, not `NoPrompt`.
  object Stacked:
    import scala.annotation.unused
    import okay.Freer.Return

    /** the stack in force, as a type member */
    final class Stack[S0 <: Tuple]:
      type S = S0

    /** what `reset` hands its body: the prompt and the stack it made (`import s.given`); open for Lexical */
    class In[R, S <: Tuple](val p: Prompt[R]):
      given stack: Stack[p.type *: S] = new Stack[p.type *: S]

    /** a plain delimiter */
    final class Reset[R, S <: Tuple](p0: Prompt[R]) extends In[R, S](p0)

    /** `p` is on the stack, with `Below` the stack under it */
    @implicitNotFound("prompt ${P} is not on the prompt stack ${S}: a shift names the prompt of a reset it is INSIDE (Delim.Stacked.reset { s => import s.given; … shift(s.p) … }) — not one that has returned, and not one another reset made")
    sealed trait Has[S <: Tuple, P]:
      /** the stack below `p` */
      type Below <: Tuple
    object Has:
      type Aux[S <: Tuple, P, B <: Tuple] = Has[S, P] { type Below = B }
      given here[P, S <: Tuple]: Aux[P *: S, P, S] = new Has[P *: S, P] { type Below = S }
      given there[P, Q, S <: Tuple, B <: Tuple](using Aux[S, P, B]): Aux[Q *: S, P, B] =
        new Has[Q *: S, P] { type Below = B }

    // A stacked program is `Freer` over the same row, at the prompt stack as its index.

    /** the row a stacked program runs in */
    type Row[F[+_]] = Ro[F]

    /** a program under the stack `S` */
    type Under[F[+_], A, S <: Tuple] = Freer[Row[F], S, S, A]


    /** THE EMBEDDINGS ARE IDENTITIES: the same nodes at two types, so crossing costs nothing */
    inline def under[F[+_], A](p: A ! Delim + F)(using st: Stack[?]): Under[F, A, st.S] = at[F, A, st.S](p)

    /** at a stack named by the caller */
    def at[F[+_], A, S <: Tuple](p: A ! Delim + F): Under[F, A, S] = p.asInstanceOf[Under[F, A, S]]

    /** a stacked program as an unstacked one */
    def erase[F[+_], A, S <: Tuple](p: Under[F, A, S]): A ! Delim + F = p.asInstanceOf[A ! Delim + F]

    /** THE CLAIM: presence is monotone, so a program typed at a stack runs where more prompts are installed */
    private[okay] def rebase[F[+_], A, S1, R1, S2, R2](p: Freer[Row[F], S1, R1, A]): Freer[Row[F], S2, R2, A] =
      p.asInstanceOf[Freer[Row[F], S2, R2, A]]

    /** the same for a continuation, re-typed, never wrapped */
    private def rebaseF[F[+_], A, B, S1, R1, S2, R2](f: A => Freer[Row[F], S1, R1, B]): A => Freer[Row[F], S2, R2, B] =
      f.asInstanceOf[A => Freer[Row[F], S2, R2, B]]

    /** under the barrier */
    private[okay] def bounded[R, F[+_]](prog: Freer[Row[F], Unit, Unit, R]): R ! F =
      residual[R, F](Frames.run[Freer.Lift[F], Unit, Unit, R](Delimited.machine[Freer.Lift[F]].reset[Unit, Unit, R](Cont0.boundary[R, Unit])(prog)))

    /** the head form as the residual program */
    private[okay] def residual[R, F[+_]](head: Freer[Row[F], Unit, Unit, R]): R ! F =
      head.asInstanceOf[R ! F]

    /** run a program written under the empty stack */
    def run[R, F[+_]](prog: Under[F, R, EmptyTuple])(using OneMachine[F]): R ! F =
      bounded[R, F](rebase(prog))


    /** a fresh prompt on the empty stack, the body under it, run */
    def delimited[R, F[+_]](body: (s: Reset[R, EmptyTuple]) => Under[F, R, s.p.type *: EmptyTuple])
                           (using om: OneMachine[F], at: At): R ! F =
      val s = new Reset[R, EmptyTuple](named[R]("delimited")(using at))
      run[R, F](Delimited.machine[Freer.Lift[F]].reset[EmptyTuple, EmptyTuple, R](Cont0.delimiter(s.p))(rebase(body(s))))(using om)

    /** a fresh prompt on the stack in force, for the body only */
    def reset[R, F[+_]](using st: Stack[?])
                       (body: (s: Reset[R, st.S]) => Under[F, R, s.p.type *: st.S])
                       (using at: At): Under[F, R, st.S] =
      val s = new Reset[R, st.S](named[R]("reset")(using at))
      Delimited.machine[Freer.Lift[F]].reset[st.S, st.S, R](Cont0.delimiter(s.p))(rebase(body(s)))

    /** capture to `p`, which must be on the stack; the body gets `p *: B` */
    def shift[R, A, F[+_]](p: Prompt[R])(using st: Stack[?])[B <: Tuple](using @unused ev: Has.Aux[st.S, p.type, B])
                          (f: Stack[p.type *: B] ?=> (A => Under[F, R, p.type *: B]) => Under[F, R, p.type *: B])
                          (using at: At): Under[F, A, st.S] =
      given Stack[p.type *: B] = new Stack[p.type *: B]
      Delimited.machine[Freer.Lift[F]].shift[R, B, st.S, st.S, A](Cont0.delimiter(p))(k => rebase(f(rebaseF(k))))

    /** the body runs with `p` consumed, under `B` */
    def shift0[R, A, F[+_]](p: Prompt[R])(using st: Stack[?])[B <: Tuple](using @unused ev: Has.Aux[st.S, p.type, B])
                           (f: Stack[B] ?=> (A => Under[F, R, B]) => Under[F, R, B])
                           (using at: At): Under[F, A, st.S] =
      given Stack[B] = new Stack[B]
      Delimited.machine[Freer.Lift[F]].shift0[R, B, st.S, st.S, A](Cont0.delimiter(p))(k => rebase(f(rebaseF(k))))

    /** drop the continuation and answer `value` at `p` */
    def abort[R, A, F[+_]](p: Prompt[R])(using st: Stack[?])[B <: Tuple](using @unused ev: Has.Aux[st.S, p.type, B])
                          (value: R)(using at: At): Under[F, A, st.S] =
      Delimited.machine[Freer.Lift[F]].shift0[R, B, st.S, st.S, A](Cont0.delimiter(p))(_ => rebase(Return[Row[F], B, R](value)))

    /** `dollar`, stacked */
    def dollar[R0, R, F[+_]](using st: Stack[?])(ret: R0 => Under[F, R, st.S])
                            (body: (s: In[R, st.S]) => Under[F, R0, s.p.type *: st.S])
                            (using at: At): Under[F, R, st.S] =
      val s = new In[R, st.S](named[R]("dollar")(using at))
      Delimited.machine[Freer.Lift[F]].dollar[R, R0, st.S, st.S](Cont0.delimiter(s.p))(ret)(rebase(body(s)))
}

/** where a capture was written, as a compile-time constant: a resumed continuation has no useful JVM stack trace */
final case class At(where: String) extends AnyVal:
  override def toString: String = where

object At:

  /** when no position is known */
  val unknown: At = At("<unknown>")

  /** the call site's file and line */
  inline given here: At = ${ hereImpl }

  // the macro behind `here`
  def hereImpl(using scala.quoted.Quotes): scala.quoted.Expr[At] =
    import scala.quoted.*
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion
    val name =
      try pos.sourceFile.name
      catch case _: Throwable => "<unknown>"
    val line = pos.startLine + 1
    val where = Expr(s"$name:$line")
    '{ At($where) }
