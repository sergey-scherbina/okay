package okay

import okay.Row.plus
import scala.quoted.*
import scala.annotation.implicitNotFound

/**
 * A continuation as an effect (specs/shift-effect.md): `Shift % R` in the row is a capture to the nearest
 * `reset` of answer `R`, and `reset` is its handler. Its operations are Shift's and run on Shift's machine;
 * no value of this type is made.
 */
sealed trait Shift[R, +A]

/** Danvy-Filinski's capture: the body runs under its `reset`, so it may capture to it again; `k` re-installs it */
def shift[R, A, F[+_]](f: (A => R ! Shift % R + F) => R ! Shift % R + F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
  Shift.ofDyn(Shift.shift[R, A, F](k.prompt)(Shift.clauseDyn(f)))

/** the body runs outside its `reset`; `k` re-installs it */
def shift0[R, A, F[+_]](f: (A => R ! F) => R ! F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
  Shift.ofDyn(Shift.shift0[R, A, F](k.prompt)(Shift.clauseDyn(f)))

/** inside a block — a `reset`, or `Shift.delimited`/`scope`/`collect` — the delimiter, its answer and the row
 * come from the block's evidence, so only the value type is named */
def shift[A](using in: Shift.Prompted[?])(f: (A => in.Res ! in.K + in.F) => in.Res ! in.K + in.F)(using at: At): A ! in.K + in.F =
  Shift.asRow(in)(Shift.shift[in.Res, A, in.F](in.prompt)(Shift.clauseDyn(f)))

/** inside a block, the 0-variant: the body runs outside the delimiter, in the row outside it */
def shift0[A](using in: Shift.Prompted[?])(f: (A => in.Res ! in.F) => in.Res ! in.F)(using at: At): A ! in.K + in.F =
  Shift.asRow(in)(Shift.shift0[in.Res, A, in.F](in.prompt)(Shift.clauseDyn(f)))

/**
 * delimit, and answer every capture of answer `R`. The body sees the block's evidence (`Shift.Prompted`, keyed
 * by `R`), so a `shift`, `exit` or `emit` written in it names only its value type; a program built elsewhere
 * passes as it is.
 */
def reset[R, F[+_]](body: Shift.Prompted.Aux[R, Shift % R, F] ?=> R ! Shift % R + F)
                   (using k: Shift.Key[R], d: Distinct[Shift % R + F], n: Shift.Machine[F]): R ! F =
  val pushed = Shift.push[R, F](k.prompt)(Shift.toDyn(body(using Shift.Prompted.keyed[R, F](k))))
  // a row that still holds a capture's effect is run by the machine outside
  if n.inner then Shift.innerDyn(pushed) else Shift.runReset[R, F](pushed)

/** `reset` as a value: `p.handle(Reset[R])` */
object Reset:
  def apply[R](using k: Shift.Key[R]): Handler.Full[Shift % R, R, [A] =>> R, Shift.Machine] =
    new Handler.Full[Shift % R, R, [A] =>> R, Shift.Machine]:
      def run[A, F[+_]](p: A ! Shift % R + F)(using a: A <:< R, d: Distinct[Shift % R + F], n: Shift.Machine[F]): R ! F =
        reset[R, F](a.substituteCo[[X] =>> X ! Shift % R + F](p))

/** a delimiter's name: answer type `R`, identity by allocation, labelled for diagnostics */
final class Prompt[R](val what: String, val where: String):
  /** `what @ where`, joined only when asked */
  def label: String = s"$what @ $where"
  override def toString: String = label


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
        |A capture reaches a delimiter installed on the machine running it
        |and not yet returned. A door that runs a machine (`delimited`,
        |`collect`, `resumable`, `Shift.run`) nests on a machine already
        |running in its row (`Shift.Machine`), so the prompt was never
        |installed here, has returned (a continuation or a prompt kept
        |past its block), or belongs to a program another machine ran.
        |See docs/continuations/12-one-machine.md.""".stripMargin

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

/**
 * ONE EFFECT, `Shift % K` (specs/shift-merge.md): what was `Shift` — prompts as values, any number, run by one
 * machine — is the dynamic form `Shift % ?`; the static form keys a delimiter by a type (the answer type, as
 * the top-level `shift`/`reset` above do). Everything below the first half is what was `object Shift`.
 */
object Shift {

  /** a fresh prompt, labelled with its line */
  def prompt[R](using at: At): Prompt[R] = named[R]("prompt")

  /** a prompt labelled by its door and line */
  private def named[R](what: String)(using at: At): Prompt[R] =
    new Prompt[R](what, at.where)

  /** the row the machine runs a `Shift % ? + F` program in, and its programs */
  type Ro[F[+_]] = Cont0.Row[Freer.Lift[F]]
  type U[F[+_], A] = Freer[Ro[F], Unit, Unit, A]

  /** THE DOORS' CLAIM: a `Shift % ? + F` program is a `U[F, A]` at the same erasure (only the machine reads `Cont0`) */
  private[okay] def in[F[+_], A](p: A ! Shift % ? + F): U[F, A] = p.asInstanceOf[U[F, A]]
  private[okay] def out[F[+_], A](p: U[F, A]): A ! Shift % ? + F = p.asInstanceOf[A ! Shift % ? + F]
  private def inF[F[+_], A, B](f: A => B ! Shift % ? + F): A => U[F, B] = f.asInstanceOf[A => U[F, B]]
  private def clause[F[+_], A, R](f: (A => R ! Shift % ? + F) => R ! Shift % ? + F): Stack[Freer.Lift[F], A, Unit, Unit, R] => U[F, R] =
    f.asInstanceOf[Stack[Freer.Lift[F], A, Unit, Unit, R] => U[F, R]]

  /** every unstacked program is at `Unit`, so every delimiter it installs is */
  private[okay] def atUnit[R](p: Prompt[R]): Cont0.Delimiter[R, Unit] = Cont0.delimiter(p)

  /** `reset` at `p` */
  def push[R, F[+_]](p: Prompt[R])(body: R ! Shift % ? + F): R ! Shift % ? + F =
    out(Delimited.machine[Freer.Lift[F]].reset[Unit, Unit, R](atUnit(p))(in(body)))

  /** `ret $ body` at `p`: `ret` runs outside, a `shift0` to `p` takes it along */
  def dollar[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! Shift % ? + F)(body: R0 ! Shift % ? + F): R ! Shift % ? + F =
    out(Delimited.machine[Freer.Lift[F]].dollar[R, R0, Unit, Unit](atUnit(p))(inF(ret))(in(body)))


  /** capture to `p`; the body runs under `p`, `k` re-installs it */
  def shift[R, A, F[+_]](p: Prompt[R])
                        (f: (A => R ! Shift % ? + F) => R ! Shift % ? + F)(using at: At): A ! Shift % ? + F =
    out(Delimited.machine[Freer.Lift[F]].shift[R, Unit, Unit, Unit, A](atUnit(p))(clause(f)))

  /** the body consumes `p`; `k` re-installs it */
  def shift0[R, A, F[+_]](p: Prompt[R])
                         (f: (A => R ! Shift % ? + F) => R ! Shift % ? + F)(using at: At): A ! Shift % ? + F =
    out(Delimited.machine[Freer.Lift[F]].shift0[R, Unit, Unit, Unit, A](atUnit(p))(clause(f)))

  /** a fresh prompt, a block under it, run */
  def reset[R, F[+_]](body: Prompt[R] => R ! Shift % ? + F)
                     (using om: Machine[F], at: At): R ! F =
    val p = named[R]("reset")(using at)
    run(push(p)(body(p)))

  /**
   * THE BLOCK'S EVIDENCE, one for every block (specs/shift-merge.md): a `reset` keyed by its answer type and
   * a `delimited`/`scope`/`dollar` with a fresh prompt both make one, so `exit`, `emit` and the one-argument
   * `shift` work in either. It carries the delimiter (`prompt`), its answer (`Res`), the effect its captures
   * are typed at (`K`: `Shift % R` for a keyed `reset`, `Shift % ?` for a prompt by value) and the row outside
   * the block (`F`). Only a block makes one, so a capture through it cannot miss.
   */
  @implicitNotFound("no reset around this shift: inside `reset { … }` (or `Shift.delimited`, `scope`, `collect`) a shift names only its value type, `shift[A](k => …)`; elsewhere name all three, `shift[R, A, F](k => …)`")
  sealed abstract class Prompted[R]:
    /** the prompt's answer type */
    type Res = R
    /** the effect a capture to this delimiter is typed at */
    type K[+A]
    /** the row outside the block */
    type F[+A]
    def prompt: Prompt[R]

  object Prompted:
    type Aux[R, K0[+_], F0[+_]] = Prompted[R] { type K[+A] = K0[A]; type F[+A] = F0[A] }
    /** a fresh prompt's block: captures typed `Shift % ?` */
    private[okay] def dynamic[R, F0[+_]](p: Prompt[R]): Aux[R, Shift % ?, F0] = new Prompted[R]:
      type K[+A] = Shift[?, A]
      type F[+A] = F0[A]
      def prompt: Prompt[R] = p
    /** a `reset`'s block, keyed by its answer type: captures typed `Shift % R` */
    private[okay] def keyed[R, F0[+_]](k: Key[R]): Aux[R, Shift % R, F0] = new Prompted[R]:
      type K[+A] = Shift[R, A]
      type F[+A] = F0[A]
      def prompt: Prompt[R] = k.prompt

  /** a delimiter installed on the machine already running (the nested half of `delimited`) */
  def scope[R, F[+_]](body: Prompted.Aux[R, Shift % ?, F] ?=> R ! Shift % ? + F)(using At): R ! Shift % ? + F =
    scopeAs("scope")(body)

  /** `scope` labelled by its door */
  private def scopeAs[R, F[+_]](what: String)(body: Prompted.Aux[R, Shift % ?, F] ?=> R ! Shift % ? + F)
                               (using At): R ! Shift % ? + F =
    val p = named[R](what)
    push(p)(body(using Prompted.dynamic[R, F](p)))

  /** a fresh delimiter and the machine: the root of a `Prompted` block */
  def delimited[R, F[+_]](body: Prompted.Aux[R, Shift % ?, F] ?=> R ! Shift % ? + F)
                         (using om: Machine[F], at: At): R ! F =
    run(scopeAs("delimited")(body))(using om)

  /** `dollar` with the evidence in scope */
  def dollar[R0, R, F[+_]](ret: R0 => R ! Shift % ? + F)(body: Prompted.Aux[R, Shift % ?, F] ?=> R0 ! Shift % ? + F)
                          (using at: At): R ! Shift % ? + F =
    val p = named[R]("dollar")
    dollar[R0, R, F](p)(ret)(body(using Prompted.dynamic[R, F](p)))

  /** capture to the delimiter in force */
  def shift[R, A, F[+_]](using in: Prompted[R])
                          (f: (A => R ! Shift % ? + F) => R ! Shift % ? + F)(using At): A ! Shift % ? + F =
    shift[R, A, F](in.prompt)(f)

  /** `shift` in a direct block, one type argument; the answer type and row come from the evidence and the block */
  inline def shift[A](using in: Prompted[?])[F[_]]
                         (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                         (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    Delimited.machine[Freer.Lift[Pure]].shift[in.Res, Unit, Unit, Unit, A](Cont0.delimiter[in.Res, Unit](in.prompt))(
      f.asInstanceOf[Stack[Freer.Lift[Pure], A, Unit, Unit, in.Res] => U[Pure, in.Res]]).asInstanceOf[A ! rw.R]

  /** the 0-variant */
  def shift0[R, A, F[+_]](using in: Prompted[R])
                           (f: (A => R ! Shift % ? + F) => R ! Shift % ? + F)(using At): A ! Shift % ? + F =
    shift0[R, A, F](in.prompt)(f)

  /** the 0-variant in a direct block */
  inline def shift0[A](using in: Prompted[?])[F[_]]
                          (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                          (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    Delimited.machine[Freer.Lift[Pure]].shift0[in.Res, Unit, Unit, Unit, A](Cont0.delimiter[in.Res, Unit](in.prompt))(
      f.asInstanceOf[Stack[Freer.Lift[Pure], A, Unit, Unit, in.Res] => U[Pure, in.Res]]).asInstanceOf[A ! rw.R]

  /** abort to the delimiter in force */
  def abort[R, A, F[+_]](using in: Prompted[R])(value: R)(using At): A ! Shift % ? + F =
    abort[R, A, F](in.prompt)(value)

  // THE PATTERNS: four shapes that earn a capture, each named for what it does. They capture with `shift0`:
  // their bodies never capture to the same prompt again.

  /**
   * WHICH ROW A PATTERN'S CAPTURE IS TYPED AT (specs/shift-merge.md): inside a `direct` block the block's —
   * a helper taking the evidence abstractly writes `!Shift.emit(a)` and the mark sees its own row — and
   * elsewhere the evidence's, `K + F`, which a `for` over the block reads. A given with a priority rather
   * than an inline `summonFrom`: the latter's type variable leaked into the block's typing (measured).
   */
  sealed trait RowFor[P <: Prompted[?]]:
    type R[+X]

  object RowFor extends RowForEvidence:
    type Aux[P <: Prompted[?], R0[+_]] = RowFor[P] { type R[+X] = R0[X] }
    // INLINE, with an inline `ctx`: the block's evidence exists only while `direct` expands, and a reference to
    // it surviving into the program is refused by the macro (measured)
    inline given direct[P <: Prompted[?], F[_]](using inline ctx: DirectCtx[F], rw: Reader.RowOf[F]): Aux[P, rw.R] =
      at[P, rw.R]
    /** one class for every row, not one per inline site */
    def at[P <: Prompted[?], R0[+_]]: Aux[P, R0] = new RowFor[P] { type R[+X] = R0[X] }

  trait RowForEvidence:
    given evidence[R0, K0[+_], F0[+_], P <: Prompted.Aux[R0, K0, F0]]: RowFor.Aux[P, K0 + F0] =
      RowFor.at[P, K0 + F0]

  /** a capture to `in`'s delimiter typed at the row `r` chose: the same `Cont0` operation at the same
   * erasure, which only the machine reads — the claim `asRow` makes, made for the chosen row */
  private def atRow[A, P <: Prompted[?]](q: A ! Shift % ? + Pure)(using r: RowFor[P]): A ! r.R = q.asInstanceOf[A ! r.R]

  /** 1 · leave the block in force early, with its answer `value`: what follows is dropped. In any block — a
   * `reset` or a `delimited`/`scope` — in a `for` or a `direct` block alike (`RowFor`) */
  def exit[A](using in: Prompted[?])(value: in.Res)(using r: RowFor[in.type], at: At): A ! r.R =
    atRow[A, in.type](Shift.shift0[in.Res, A, Pure](in.prompt)(_ => okay.pure(value)))

  /** 2 · a push API read as a pull: the evidence `emit` captures to, sealed */
  sealed abstract class Emitting[A]:
    type Elem = A
    /** the prompt's answer: a list for `collect`, a state-passing function for `collectUntil` */
    type Res
    /** the row outside the block */
    type F[+X]
    val in: Prompted.Aux[Res, Shift % ?, F]
    /** what one emit does with the rest of the producer */
    def onEmit[X[+_]](a: A)(k: Unit => Res ! X): Res ! X

  object Emitting:
    /** the evidence with the row outside the block named: what a helper emitting from outside the block asks */
    type Aux[A, F0[+_]] = Emitting[A] { type F[+X] = F0[X] }

  /** `collect`: cons on the way back */
  private final class Listing[A, F0[+_]](prompted: Prompted.Aux[List[A], Shift % ?, F0]) extends Emitting[A]:
    type Res = List[A]
    type F[+X] = F0[X]
    val in: Prompted.Aux[List[A], Shift % ?, F0] = prompted
    def onEmit[X[+_]](a: A)(k: Unit => List[A] ! X): List[A] ! X = k(()).map(a :: _)

  /** `collectUntil`: the fold's state passed down through the prompt's answer; stops without resuming */
  private final class Stopping[A, S, R, G[+_], F0[+_]](prompted: Prompted.Aux[S => R ! G, Shift % ?, F0], fo: FoldUntil[A, S, R])
    extends Emitting[A]:
    type Res = S => R ! G
    type F[+X] = F0[X]
    val in: Prompted.Aux[S => R ! G, Shift % ?, F0] = prompted
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
  def collect[A, F[+_]](body: Emitting.Aux[A, F] ?=> Unit ! Shift % ? + F)
                       (using om: Machine[F], at: At): List[A] ! F =
    run(collectAs("collect")(body))(using om)

  /** the nested half of `collect` */
  def collecting[A, F[+_]](body: Emitting.Aux[A, F] ?=> Unit ! Shift % ? + F)
                          (using At): List[A] ! Shift % ? + F =
    collectAs("collecting")(body)

  private def collectAs[A, F[+_]](what: String)(body: Emitting.Aux[A, F] ?=> Unit ! Shift % ? + F)
                                 (using At): List[A] ! Shift % ? + F =
    scopeAs[List[A], F](what)(
      body(using new Listing[A, F](summon[Prompted.Aux[List[A], Shift % ?, F]]))
        .map(_ => List.empty[A]))

  /** emit into a `FoldUntil` that may stop the producer early */
  def collectUntil[A, S, R, F[+_]](using fo: FoldUntil[A, S, R])
                                   (body: Emitting.Aux[A, F] ?=> Unit ! Shift % ? + F)
                                   (using om: Machine[F], at: At): R ! F =
    if fo.done(fo.init) then okay.pure(fo.end(fo.init))
    else run(collectUntilAs("collectUntil")(fo)(body))(using om)

  /** the nested half of `collectUntil` */
  def collectingUntil[A, S, R, F[+_]](using fo: FoldUntil[A, S, R])
                                      (body: Emitting.Aux[A, F] ?=> Unit ! Shift % ? + F)
                                      (using At): R ! Shift % ? + F =
    if fo.done(fo.init) then okay.pure(fo.end(fo.init))
    else collectUntilAs("collectingUntil")(fo)(body)

  private def collectUntilAs[A, S, R, F[+_]](what: String)(fo: FoldUntil[A, S, R])
                                            (body: Emitting.Aux[A, F] ?=> Unit ! Shift % ? + F)
                                            (using At): R ! Shift % ? + F =
    type G[+X] = (Shift % ? + F)[X]
    scopeAs[S => R ! G, F](what)(
      body(using new Stopping[A, S, R, G, F](summon[Prompted.Aux[S => R ! G, Shift % ?, F]], fo))
        // the producer ended: the state's own answer
        .map(_ => (s: S) => okay.pure[G, R](fo.end(s))))
      // the first emit's function applied to the start
      .flatMap(f => f(fo.init))

  /** emit one value into the `collect` in force: its row chosen as `exit`'s is (`RowFor`) */
  def emit(using e: Emitting[?])(a: e.Elem)(using r: RowFor[e.in.type], at: At): Unit ! r.R =
    atRow[Unit, e.in.type](Shift.shift0[e.Res, Unit, Pure](e.in.prompt)(k => e.onEmit[Shift % ? + Pure](a)(k)))

  /** 3 · stop in the middle, carry on later: asking with the rest as `resume`, or finished */
  enum Paused[Q, A, R, G[+_]]:
    /** a question and the rest of the program */
    case Ask[Q, A, R, G[+_]](question: Q, resume: A => Paused[Q, A, R, G] ! G,
                             at: String)
      extends Paused[Q, A, R, G]
    case Done[Q, A, R, G[+_]](value: R) extends Paused[Q, A, R, G]

  /** a dialogue over `Shift % ? + F` */
  type Dialogue[Q, A, R, F[+_]] = Paused[Q, A, R, Shift % ? + F]

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
  final class Asking[Q, A, R, G[+_]] private[Shift] (prompted: Prompted[Paused[Q, A, R, G]]):
    type Qst = Q
    type Ans = A
    type Fin = R
    type Row[+X] = G[X]
    /** not private: `pause` is inline */
    val in: Prompted[Paused[Qst, Ans, Fin, Row]] = prompted

  /** run `body` until it pauses or finishes */
  def resumable[Q, A, R, F[+_]](body: Asking[Q, A, R, Shift % ? + F] ?=> R ! Shift % ? + F)
                               (using om: Machine[F], at: At): Dialogue[Q, A, R, F] ! F =
    run(pausingAs("resumable")(body))(using om)

  /** the nested half of `resumable` */
  def pausing[Q, A, R, F[+_]](body: Asking[Q, A, R, Shift % ? + F] ?=> R ! Shift % ? + F)
                             (using At): Dialogue[Q, A, R, F] ! Shift % ? + F =
    pausingAs("pausing")(body)

  private def pausingAs[Q, A, R, F[+_]](what: String)
                                       (body: Asking[Q, A, R, Shift % ? + F] ?=> R ! Shift % ? + F)
                                       (using At): Dialogue[Q, A, R, F] ! Shift % ? + F =
    scopeAs[Dialogue[Q, A, R, F], F](what)(
      body(using new Asking[Q, A, R, Shift % ? + F](summon[Prompted[Dialogue[Q, A, R, F]]]))
        .map(Paused.Done[Q, A, R, Shift % ? + F](_)))

  /** ask and wait for the answer, in a direct block */
  inline def pause(using s: Asking[?, ?, ?, ?])[F[_]]
                  (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                  (q: s.Qst): s.Ans ! rw.R =
    shift[s.Ans](using s.in)(k => okay.pure(Paused.Ask(q,
      // THE ONE CAST: only `resumable` makes an `Asking`, so the block's row is its row
      k.asInstanceOf[s.Ans => Paused[s.Qst, s.Ans, s.Fin, s.Row] ! s.Row],
      at.where)))

  /** `pause` outside a direct block, for `for` and natural transformations */
  def ask[Q, A, R, F[+_]](q: Q)(using s: Asking[Q, A, R, Shift % ? + F], at: At)
                         : A ! Shift % ? + F =
    shift[Paused[Q, A, R, Shift % ? + F], A, F](using s.in)(k =>
      okay.pure(Paused.Ask(q, k, at.where)))

  /** answer every question with `answer`, to the end */
  def drive[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F])(answer: Q => A ! F)
                           (using Machine[F]): R ! F =
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
  def answer[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F], j: Journal[A])(a: A)(using Machine[F])
                            : (Dialogue[Q, A, R, F], Journal[A]) ! F =
    p match
      case Paused.Ask(_, resume, _) => run(resume(a)).map(next => (next, j :+ a))
      case done => okay.pure((done, j))     // nobody asked; nothing to record

  /** where the dialogue stands, from its program and its journal */
  def replay[Q, A, R, F[+_]](body: Asking[Q, A, R, Shift % ? + F] ?=> R ! Shift % ? + F)
                            (using Machine[F], Replayable[Shift % ? + F], At)
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

  /** `Shift`'s operations are `Cont0`'s */
  given Effect[Shift % ?] = new Effect[Shift % ?]:
    def test(x: Any): Boolean = x.isInstanceOf[Cont0[?, ?, ?, ?]]

  /** run on the machine under the barrier: a capture with no delimiter is `NoPrompt` */
  def run[R, F[+_]](prog: R ! Shift % ? + F)(using m: Machine[F]): R ! F =
    // a machine outside runs the program, its captures included
    if m.inner then innerDyn(prog) else Stacked.bounded[R, F](in(prog))

  /** run without the barrier: a capture with no delimiter goes out, for a machine outside */
  def runNested[R, F[+_]](prog: R ! Shift % ? + F)(using Row.In[Shift % ?, F]): R ! F =
    Stacked.residual[R, F](Frames.run[Freer.Lift[F], Unit, Unit, R](in(prog)))

  /** drop the continuation and answer `value` at `p` */
  def abort[R, A, F[+_]](p: Prompt[R])(value: R)(using At): A ! Shift % ? + F =
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
    @implicitNotFound("prompt ${P} is not on the prompt stack ${S}: a shift names the prompt of a reset it is INSIDE (Shift.Stacked.reset { s => import s.given; … shift(s.p) … }) — not one that has returned, and not one another reset made")
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
    inline def under[F[+_], A](p: A ! Shift % ? + F)(using st: Stack[?]): Under[F, A, st.S] = at[F, A, st.S](p)

    /** at a stack named by the caller */
    def at[F[+_], A, S <: Tuple](p: A ! Shift % ? + F): Under[F, A, S] = p.asInstanceOf[Under[F, A, S]]

    /** a stacked program as an unstacked one */
    def erase[F[+_], A, S <: Tuple](p: Under[F, A, S]): A ! Shift % ? + F = p.asInstanceOf[A ! Shift % ? + F]

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
    def run[R, F[+_]](prog: Under[F, R, EmptyTuple])(using m: Machine[F]): R ! F =
      if m.inner then innerDyn[R, F](erase(prog)) else bounded[R, F](rebase(prog))


    /** a fresh prompt on the empty stack, the body under it, run */
    def delimited[R, F[+_]](body: (s: Reset[R, EmptyTuple]) => Under[F, R, s.p.type *: EmptyTuple])
                           (using om: Machine[F], at: At): R ! F =
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


  /**
   * A `reset` that runs its own machine runs it INSIDE whatever forced it, and nested resets of one answer type
   * each start one: JVM depth grows with the nesting (3 000-10 000 deep, then StackOverflowError). So the runs
   * are counted per thread, and past the room the next one runs on a fresh stack, as Cont's strict `k` does
   * (StackSwitch, specs/cont-stack.md Layer 2). A level is taken as ~4 KB cold, Cont's ~1.2 KB scaled.
   */
  // IN AN OBJECT OF ITS OWN, initialised only when a keyed `reset` runs (shift-merge): as fields of `Shift`
  // they ran in the initialiser every `Shift` user reaches, and Scala.js has neither `Integer.getInteger` nor
  // `ThreadLocal.withInitial` — the merged object failed to link for every JS program using a prompt by value.
  // The room itself is a ThreadLocal, against cont-stack's rule: sprint shift-merge-guard.
  private object ResetRoom:
    val room: Int = Integer.getInteger("okay.shift.room", math.max(32L, StackSwitch.firstRoom.toLong * 1200 / 4096).toInt)
    val left: ThreadLocal[Array[Int]] = ThreadLocal.withInitial(() => Array(room))

  /** run the machine for one `reset`, one level less of room; at zero on a fresh stack */
  private[okay] def runReset[R, F[+_]](pushed: R ! Shift % ? + F): R ! F =
    val cell = ResetRoom.left.get
    val here = cell(0)
    if here > 0 then
      cell(0) = here - 1
      try Shift.run[R, F](pushed)(using Machine.outermost[F]) finally cell(0) = here
    else StackSwitch.fresh { big =>
      val c = ResetRoom.left.get
      val saved = c(0)
      c(0) = big / 2
      try Shift.run[R, F](pushed)(using Machine.outermost[F]) finally c(0) = saved
    }

  /** a program keyed STATICALLY (by an answer type) as a dynamic one, to mix with captures to prompts by
   * value in one `flatMap`: rows are invariant, so this is the written row coercion (widen-is-a-coercion) */
  def dynamic[A, K, F[+_]](p: A ! Shift % K + F): A ! Shift % ? + F = toDyn[A, K, F](p)

  /** a capture built at `Shift % ?` typed at the evidence's key: the same `Cont0` operation at the same
   * erasure (only the machine reads it), and the evidence says which delimiter it targets — the claim
   * `toDyn`/`ofDyn` below make, made for a block's evidence */
  private[okay] def asRow[A](in: Prompted[?])(q: A ! Shift % ? + in.F): A ! in.K + in.F = q.asInstanceOf[A ! in.K + in.F]

  // THE ONE CLAIM: a `Shift % R` program is a `Shift` program at the same erasure (only the machine reads
  // `Cont0`), and a capture of answer `R` reaches only the prompt of `R`'s key, where its `k` and body are
  // typed in that `reset`'s row.
  private[okay] def toDyn[A, R, F[+_]](q: A ! Shift % R + F): A ! Shift % ? + F = q.asInstanceOf[A ! Shift % ? + F]
  private[okay] def ofDyn[A, R, F[+_]](q: A ! Shift % ? + F): A ! Shift % R + F = q.asInstanceOf[A ! Shift % R + F]
  private[okay] def innerDyn[R, F[+_]](q: R ! Shift % ? + F): R ! F = q.asInstanceOf[R ! F]
  private[okay] def clauseDyn[R, A, F[+_], G[+_]](f: (A => R ! G) => R ! G): (A => R ! Shift % ? + F) => R ! Shift % ? + F =
    f.asInstanceOf[(A => R ! Shift % ? + F) => R ! Shift % ? + F]

  /** the test reads the prompt, so `Shift % Int + Shift % String` is a good row */
  given typeableK[R](using k: Key[R]): TypeableK.ByValue[Shift % R] = new:
    def test(x: Any): Boolean = x match
      case s: Cont0.Shift0[?, ?, ?, ?, ?, ?] => (s.p: AnyRef) eq k.prompt
      case d: Cont0.Dollar0[?, ?, ?, ?, ?] => (d.p: AnyRef) eq k.prompt
      case _ => false

  /** level 2: the program as a `Cont` whose answers are programs: `c / k` is `reset(q >>= k)` */
  def cont[A, R, F[+_]](q: A ! Shift % R + F)(using Key[R], Distinct[Shift % R + F], Machine[F]): Cont[A, R ! F, R ! F] =
    Cont.shift[A, R ! F, R ! F](k => okay.reset[R, F](q.flatMap(a => k(a).plus[Shift % R])))

  /** level 2: a whole `Cont` as one capture */
  def embed[A, R, F[+_]](c: Cont[A, R ! F, R ! F])(using Key[R], At): A ! Shift % R + F =
    okay.shift0[R, A, F](k => c / k)

  /**
   * The key of an answer type, made at compile time: two types, two keys; one type (through any alias, a union
   * in either order), one key, and one prompt for it. An abstract type has none: it is passed in, as a
   * `ClassTag` is.
   */
  final class Key[R] private (val id: String, private[okay] val prompt: Prompt[R]):
    override def toString: String = id

  object Key:
    private val keys = new java.util.concurrent.ConcurrentHashMap[String, Key[Any]]

    /** the key of `id`, one per id */
    def intern[R](id: String): Key[R] =
      val k = keys.get(id)
      // one key per id, made at `Any` and read back at the type the id names
      (if k != null then k else keys.computeIfAbsent(id, i => new Key[Any](i, new Prompt[Any]("reset", i)))).asInstanceOf[Key[R]]

    inline given of[R]: Key[R] = ${ keyImpl[R] }

  def keyImpl[R: Type](using q: Quotes): Expr[Key[R]] =
    import q.reflect.*
    def parts(t: TypeRepr, or: Boolean): List[TypeRepr] = t.dealias match
      case OrType(a, b) if or => parts(a, or) ++ parts(b, or)
      case AndType(a, b) if !or => parts(a, or) ++ parts(b, or)
      case other => List(other)
    // bounded by the type's own nesting, which the compiler has already walked
    def norm(t: TypeRepr): String = t.dealias.simplified match
      case o: OrType => parts(o, or = true).map(norm).distinct.sorted.mkString("(", " | ", ")")
      case a: AndType => parts(a, or = false).map(norm).distinct.sorted.mkString("(", " & ", ")")
      case AppliedType(c, args) => norm(c) + args.map(norm).mkString("[", ", ", "]")
      case c: ConstantType => c.show
      case other =>
        val s = other.typeSymbol
        if s.isClassDef || s.flags.is(Flags.Opaque) then s.fullName
        else report.errorAndAbort(
          s"the answer type ${Type.show[R]} is abstract here (${other.show}), so a reset or shift of it has no key; " +
            s"take a `Shift.Key[${other.show}]` as a parameter where the type is known")
    '{ Key.intern[R](${ Expr(norm(TypeRepr.of[R])) }) }

  /**
   * THE ONE MACHINE GUARD (specs/shift-merge.md): whether a machine already runs in the row `F` — `F` holds a
   * `Shift` of any key — read off the row at compile time. Every door that runs a machine takes it: outermost,
   * it runs its own; inside one, it pushes its delimiter on the machine already running (what `scope`,
   * `collecting` and `pausing` spell by hand), so a capture never meets a second machine's prompt stack. A row
   * that cannot be read — an abstract part and no `Shift` — is a compile error asking for this evidence as a
   * parameter, so a generic helper passes the obligation on to the caller who knows the row.
   */
  final class Machine[F[+_]] @scala.annotation.publicInBinary private[okay] (val inner: Boolean)

  object Machine:
    inline given of[F[+_]]: Machine[F] = ${ machineImpl[F] }
    /** for a door that has already read the row (a keyed `reset`'s own machine): no reading again */
    private[okay] def outermost[F[+_]]: Machine[F] = new Machine[F](false)

  def machineImpl[F[+_]: Type](using q: Quotes): Expr[Machine[F]] =
    import q.reflect.*
    val shift = TypeRepr.of[Shift[Any, Any]].typeSymbol
    val delim = TypeRepr.of[Cont0[?, ?, ?, Any]].typeSymbol
    // bounded by the row's own nesting, which the compiler has already walked
    def members(t: TypeRepr): List[TypeRepr] = t.dealias.simplified match
      case OrType(a, b) => members(a) ++ members(b)
      case other => List(other)
    // an alias of a type lambda (`State % Int`, `Instances.Of[G]`) applied: one beta step per alias, at most
    // as many as the source wrote
    @scala.annotation.tailrec
    def reduce(t: TypeRepr, fuel: Int): TypeRepr = t.dealias.simplified match
      case r @ AppliedType(tc, args) if fuel > 0 =>
        val d = tc.dealias
        if d == tc then r else reduce(d.appliedTo(args), fuel - 1)
      case other => other
    val ms = members(TypeRepr.of[F].appliedTo(TypeRepr.of[Any])).map(reduce(_, 64)).flatMap(members)
    val inner = ms.exists(m => m.typeSymbol == shift || m.typeSymbol == delim)
    val unread = ms.filterNot(m => m.typeSymbol.isClassDef)
    if !inner && unread.nonEmpty then
      report.errorAndAbort(
        s"whether a machine already runs in the row ${Type.show[F]} cannot be read here: " +
          s"${unread.map(_.show).mkString(", ")} is abstract, and it may hold a Shift.\n" +
          "Pass the obligation on to the caller, who knows the row: take `(using Shift.Machine[F])`\n" +
          "(docs/continuations/12-one-machine.md)")
    '{ new Machine[F](${ Expr(inner) }) }
}
