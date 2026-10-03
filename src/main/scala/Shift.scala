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
  if n.inner then Shift.innerDyn(pushed) else Shift.run[R, F](pushed)(using Shift.Machine.outermost[F])

/** `reset` as a value: `p.handle(Reset[R])` */
object Reset:
  def apply[R](using k: Shift.Key[R]): Handler.Full[Shift % R, R, [A] =>> R, Shift.Machine] =
    new Handler.Full[Shift % R, R, [A] =>> R, Shift.Machine]:
      def run[A, F[+_]](p: A ! Shift % R + F)(using a: A <:< R, d: Distinct[Shift % R + F], n: Shift.Machine[F]): R ! F =
        reset[R, F](a.substituteCo[[X] =>> X ! Shift % R + F](p))

/** a delimiter's name: answer type `R`, identity by allocation, labelled for diagnostics. On the machine
 * (`Delimited`) a prompt in force is two value boundaries — this marks its opening — and its closing (`close`),
 * with a `ret` between them. Open for a handler frame's delimiter (`HandleFrames.Handling`, `Catching`) */
class Prompt[R](val what: String, val where: String) extends Delimited.Mark:
  /** `what @ where`, joined only when asked */
  def label: String = s"$what @ $where"
  override def toString: String = label
  /** the mark of the value boundary where the prompt's place ends: its value has passed `ret` there */
  private[okay] lazy val close: Delimited.Mark = new Delimited.Mark {}

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
  inline given here: At = ${ okay.macros.ShiftMacros.hereImpl }

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

  /** a program the machine runs: a handler frame's (`HandleFrames`) as much as a prompt's */
  type U[F[+_], A] = A ! Shift % ? + F

  /** `reset` at `p` */
  def push[R, F[+_]](p: Prompt[R])(body: R ! Shift % ? + F): R ! Shift % ? + F =
    dollar[R, R, F](p)(okay.pure)(body)

  /** `ret $ body` at `p`: `ret` runs outside, a `shift0` to `p` takes it along */
  def dollar[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! Shift % ? + F)(body: R0 ! Shift % ? + F): R ! Shift % ? + F =
    op[R, F](Dollar[Any, R0, R, F](p, ret, body))

  /** capture to `p`; the body runs under `p`, `k` re-installs it */
  def shift[R, A, F[+_]](p: Prompt[R])
                        (f: (A => R ! Shift % ? + F) => R ! Shift % ? + F)(using at: At): A ! Shift % ? + F =
    shift0[R, A, F](p)(k => push[R, F](p)(f(k)))

  /** the body consumes `p`; `k` re-installs it */
  def shift0[R, A, F[+_]](p: Prompt[R])
                         (f: (A => R ! Shift % ? + F) => R ! Shift % ? + F)(using at: At): A ! Shift % ? + F =
    withSubCont[R, A, F](p)(f)

  /** `shift0` with the continuation as DATA (DPJS's `withSubCont`): `k(x)` resumes it with a value, and
   * `k.resumeWith(m)` runs a computation inside it, its delimiters in force */
  def withSubCont[R, A, F[+_]](p: Prompt[R])
                              (f: Resumption[A, R, F] => R ! Shift % ? + F)(using at: At): A ! Shift % ? + F =
    op[A, F](Shift0[Any, A, R, F](p, f, at.where))

  /**
   * A CAPTURED CONTINUATION, up to and including its prompt: `k(x)` resumes it with a value; `resumeWith(m)` runs
   * the computation `m` INSIDE it — a capture in `m` reaches the prompts `k` carries (DPJS's `pushSubCont`,
   * "throwing into a continuation"). Either is a program that stands on its own: a deferred run, stepped into by
   * a machine for the row already running — the piece back on its stack, the value to whoever resumed — and run by
   * a machine of its own when forced by anyone else (a `k` that outlived its run). No barrier either way.
   */
  final class Resumption[A, R, F[+_]] private[Shift] (p: Prompt[R], piece: Delimited.Piece[Freer.Lift[Shift % ? + F], A, Unit, R, Unit],
                                                       held: Int)
    extends (A => R ! Shift % ? + F):
    def apply(x: A): R ! Shift % ? + F = resumeWith(okay.pure(x))
    def resumeWith(m: A ! Shift % ? + F): R ! Shift % ? + F =
      claim[R ! Shift % ? + F](Free.delay(Nested[R, F](op[R, F](Resume[Any, A, R, F](p, piece, m, held)), nested = true)))

  /** an operation of `Shift` as a node of its row */
  private def op[A, F[+_]](o: Shift[Any, A]): A ! Shift % ? + F = Freer.Inject[Freer.Lift[Shift % ? + F], Unit, Unit, A](o)

  // ---- SHIFT ON THE MACHINE (specs/cont-atm.md): its operations are values of the row, answered by `Steps` on
  // `Delimited`. A prompt in force is two marks on the segment, its opening and its closing, `ret` between them:
  // a value passes all three; a capture to it cuts at the opening (`k` the frames above it, taken along with the
  // marks and `ret`), and its body answers after the closing — `ret` skipped, as λ$'s `S0 k.e` replaces the whole
  // `ret $ body`.

  /** `ret $ body` at `p` */
  private[okay] final case class Dollar[K, R0, R, F[+_]](p: Prompt[R], ret: R0 => R ! Shift % ? + F, body: R0 ! Shift % ? + F)
    extends Shift[K, R]
  /** capture to `p`: the body, given `k`, answers in `p`'s place */
  private[okay] final case class Shift0[K, A, R, F[+_]](p: Prompt[R], body: Resumption[A, R, F] => R ! Shift % ? + F,
                                                        at: String) extends Shift[K, A]
  /** leave `p`'s place with `value` */
  private[okay] final case class Abort[K, A, R](p: Prompt[R], value: R, at: String) extends Shift[K, A]
  /** a captured continuation resumed: its segments and boundaries, `p`'s and `ret`'s included, back on top, and
   * `body` run inside them; `held` what kinds of frame the run that captured it had installed (`Steps.held`) */
  private[okay] final case class Resume[K, A, R, F[+_]](p: Prompt[R], k: Delimited.Piece[Freer.Lift[Shift % ? + F], A, Unit, R, Unit],
                                                        body: A ! Shift % ? + F, held: Int)
    extends Shift[K, R]

  /** the prompt an operation names */
  private def promptOf(x: Any): AnyRef | Null = x match
    case d: Dollar[?, ?, ?, ?] => d.p
    case s: Shift0[?, ?, ?, ?] => s.p
    case a: Abort[?, ?, ?] => a.p
    case r: Resume[?, ?, ?, ?] => r.p
    case _ => null

  /**
   * THE CLAIMS of a `Free` row on the machine, made here and nowhere else. The machine is cast-free; what its
   * types cannot say about a row of `Free` is said once here: every node of a `Free` program is at `Unit`, so
   * every segment and stack of its run is; an operation, or a nested run, was built in the row of the program it
   * stands in; and the place a prompt's boundary stands answers that prompt's type (Dybvig, Peyton Jones & Sabry's
   * `eqPrompt`).
   */
  private def claim[X](x: Any): X = x.asInstanceOf[X]

  /** `Steps.held`'s bits */
  private final val Framed = 1
  private final val Catches = 2

  /** a nested run's value boundary: no capture crosses it (a handler's operation and a throw do) */
  private object Barrier extends Delimited.Mark

  /**
   * A RUN AS A VALUE: a `Delay`'s thunk, forced by whoever holds it, STEPPED INTO by a machine for the row already
   * running (`Steps.enter`) — so a run nested in a run, a reset in a reset or a handler in a handler, is one loop and
   * not a host frame. `Nested` is Shift's own; a handler's run (`HandleFrames.Run`) is the other.
   */
  abstract class Pending[R, F[+_]] extends (() => R ! F), Freer.Suspended:
    def program: R ! Shift % ? + F

  /** a run of a `Shift % ? + F` program: `nested` — no barrier, a capture with no delimiter here goes out */
  private final class Nested[R, F[+_]](val program: R ! Shift % ? + F, val nested: Boolean) extends Pending[R, F]:
    def apply(): R ! F =
      val s = Steps[F](nested)
      Delimited.over[Shift % ? + F, F](s, s).value(program)

  /**
   * what each operation does, for a row `Shift % ? + F`, and which leave the machine. A prompt in force is two
   * value boundaries, its opening and its closing, with `ret` between them; a HANDLER FRAME is one too
   * (`HandleFrames.Handling`), and an operation of `F` it takes is a capture to it whose body is its clause — `k`
   * putting the frame back (a deep handler). A CATCH FRAME (`HandleFrames.Catching`) answers a throw in its place.
   */
  private final class Steps[F[+_]](nested: Boolean)
    extends Step[Freer.Lift[Shift % ? + F], Freer.Lift[Shift % ? + F]], Delimited.Outer[Shift % ? + F, F]:
    private type L[S, R, A] = Freer.Lift[Shift % ? + F][S, R, A]
    private type U = Unit

    /**
     * what kinds of frame this run has installed: `Framed` (a handler's — until then an operation of `F` leaves
     * without a look) and `Catches` (a catch frame's — until then user code runs with no `try`). A captured `k`
     * carries the bits of the run it was captured in, and the run that resumes it — perhaps another, a dialogue
     * driven later — takes them on with the frames its piece puts back
     */
    private var held = 0
    private def framed: Boolean = (held & Framed) != 0
    private def hold(bits: Int, machine: Delimited[L]): Unit =
      if (bits & Catches) != 0 && (held & Catches) == 0 then machine.guarding()
      held |= bits
    /** the frame `apply` found for the operation it kept, for `step` to cut to */
    private var taker: HandleFrames.Handling[?] | Null = null

    // ---- Outer: which operations leave

    def apply[X, B, S, R, Z](op: (Shift % ? + F)[X], m: Stack[L, B, S, R, Z], machine: Delimited[L]): F[X] | Null = op match
      // an installation and a resumption are always this machine's: they put a prompt on, not look for one
      case _: Dollar[?, ?, ?, ?] | _: Resume[?, ?, ?, ?] => null
      case s: Shift[?, ?] =>
        val p = promptOf(s)
        if nested && p != null && machine.holds(m, _ eq p) == null then claim[F[X]](op) else null
      case _ =>
        if !framed then claim[F[X]](op)
        else machine.holds(m, takes(op)) match
          case h: HandleFrames.Handling[?] => taker = h; null
          case _ => claim[F[X]](op)

    private def takes(op: Any): Delimited.Mark => Boolean =
      case h: HandleFrames.Handling[?] => h.takes(op)
      case _ => false

    def diagonal[T, R]: T =:= R = claim[T =:= R](summon[T =:= T])

    def enter[T, R, A](t: () => Freer[L, T, R, A]): Freer[L, T, R, A] | Null = t match
      case p: Pending[?, ?] => claim[Freer[L, T, R, A]](p.program)
      case _ => null

    def barrier(t: () => Any): Delimited.Mark | Null = t match
      case n: Nested[?, ?] if !n.nested => Barrier
      case _ => null

    // ---- Step: what the machine does with them

    def step[A, B, S, T, R, Z](op: L[T, R, A], k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z],
                               machine: Delimited[L]): Delimited.Next[L, Z] =
      at(op, claim[Frames[L, A, B, U, U]](k), claim[Stack[L, B, U, U, Z]](m), machine)

    private def at[A, B, Z](op: (Shift % ? + F)[A], k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z],
                            machine: Delimited[L]): Delimited.Next[L, Z] = op match
      case d: Dollar[?, r0, r, ?] =>
        d.p match
          case _: HandleFrames.Handling[?] => hold(Framed, machine)
          case _ => ()
        d.p match
          case _: HandleFrames.Catching => hold(Catches, machine)
          case _ => ()
        val ret = claim[r0 => Freer[L, U, U, r]](d.ret)
        val outside = machine.delim(d.p.close, claim[Frames[L, r, B, U, U]](k), m)
        machine.next(claim[Freer[L, U, U, r0]](d.body), machine.end[r0, U], machine.delim(d.p, machine.frame(ret, machine.end[r, U]), outside))
      case s: Shift0[?, a, r, ?] =>
        val c = claim[Cut[a, r, Z]](cutOrFail(s.p, s.at, k, m, machine))
        val body = claim[Resumption[a, r, F] => Freer[L, U, U, r]](s.body)
        machine.next(body(Resumption[a, r, F](s.p, c.piece, held)), c.out, c.rest)
      case ab: Abort[?, a, r] =>
        val c = claim[Cut[a, r, Z]](cutOrFail(ab.p, ab.at, k, m, machine))
        machine.next(Freer.Return[L, U, r](ab.value), c.out, c.rest)
      case rs: Resume[?, a, r, ?] =>
        hold(rs.held, machine)
        machine.reinstall(claim[Delimited.Piece[L, a, U, r, U]](rs.k), claim[Freer[L, U, U, a]](rs.body), claim[Frames[L, r, B, U, U]](k), m)
      // an operation of `F` a frame takes (`apply` kept it): a capture to the frame, the clause its body
      case _ =>
        val h = taker.nn
        taker = null
        handled(h, op, cut(k, m, _ eq h, never, machine).nn, machine)

    /** the clause of frame `h` for `op` in the frame's place, `k` resuming the piece cut to it */
    private def handled[A, Y, Z](h: HandleFrames.Handling[Y], op: Any, c: Cut[A, Any, Z], machine: Delimited[L]): Delimited.Next[L, Z] =
      val piece = claim[Delimited.Piece[L, Any, U, Y, U]](c.piece)
      machine.next(claim[Freer[L, U, U, Any]](h.clause(op, Resumption[Any, Y, F](h, piece, held))), c.out, c.rest)

    /** a throw: the nearest catch frame answers it in its place, the frames above it dropped; one that declines
     * (null) or throws passes it, or what it threw, to the frames below; none takes it — thrown on */
    override def thrown[A, B, S, T, R, Z](t: Throwable, k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z],
                                          machine: Delimited[L]): Delimited.Next[L, Z] | Null =
      catchFrom(t, claim[Frames[L, A, B, U, U]](k), claim[Stack[L, B, U, U, Z]](m), machine)

    @scala.annotation.tailrec
    private def catchFrom[A, B, Z](t: Throwable, k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z],
                                   machine: Delimited[L]): Delimited.Next[L, Z] | Null =
      cut(k, m, catches, never, machine) match
        case null => null
        case c => answerOf(c.tag, t) match
          case null => catchFrom(t, c.out, c.rest, machine)
          case again: Delimited.Thrown => catchFrom(again.t, c.out, c.rest, machine)
          case p => machine.next(claim[Freer[L, U, U, Any]](p), c.out, c.rest)

    /** the catch frame's answer for `t`: a program, null (not its throw), or what its handler threw */
    private def answerOf(tag: Delimited.Mark, t: Throwable): Any = tag match
      case h: HandleFrames.Catching => try h.caught(t) catch case t2: Throwable => Delimited.Thrown(t2)
      case _ => null

    private val catches: Delimited.Mark => Boolean = _.isInstanceOf[HandleFrames.Catching]
    private val never: Delimited.Mark => Boolean = _ => false

    /** a capture to a prompt: the piece up to and including its closing (its opening and `ret` taken along), and
     * what lies under the closing */
    private final class Cut[A, R, Z](val tag: Delimited.Mark, val piece: Delimited.Piece[L, A, U, R, U],
                                     val out: Frames[L, R, Any, U, U], val rest: Stack[L, Any, U, U, Z])

    private def cutOrFail[A, B, Z](p: Prompt[?], from: String, k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z],
                                   machine: Delimited[L]): Cut[A, Any, Z] =
      cut(k, m, _ eq p, _ eq Barrier, machine) match
        case null => throw NoPrompt(from, p.label, installed(m))
        case c => c

    /** to the nearest prompt `is` holds for, `stop` before a barrier it holds for; null when none */
    private def cut[A, B, Z](k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z], is: Delimited.Mark => Boolean,
                             stop: Delimited.Mark => Boolean, machine: Delimited[L]): Cut[A, Any, Z] | Null =
      machine.cut(k, m, is, stop) match
        case null => null
        case open => (open.tag, open.rest) match
          case (p: Prompt[?], Stack.Delim(close, out, rest)) if close eq p.close =>
            val piece = Delimited.Piece.Snoc(open.piece, open.out, close)
            Cut(p, claim[Delimited.Piece[L, A, U, Any, U]](piece), claim[Frames[L, Any, Any, U, U]](out), claim[Stack[L, Any, U, U, Z]](rest))
          case _ => throw IllegalStateException(s"prompt ${open.tag} opened and never closed")

    /** the prompts on the machine's stack, innermost first, for `NoPrompt` */
    private def installed[B, Z](m: Stack[L, B, U, U, Z]): List[String] =
      var seen = List.empty[String]
      labels(m, p => seen = p.label :: seen)
      seen.reverse

    @scala.annotation.tailrec
    private def labels[B, S, R, Z](m: Stack[L, B, S, R, Z], see: Prompt[?] => Unit): Unit = m match
      case Stack.Delim(tag, _, rest) =>
        tag match
          case p: Prompt[?] => see(p)
          case _ => ()
        if !(tag eq Barrier) then labels(rest, see)
      case _ => ()

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
    Shift.shift[in.Res, A, Pure](in.prompt)(f.asInstanceOf[(A => in.Res ! Shift % ? + Pure) => in.Res ! Shift % ? + Pure])
      .asInstanceOf[A ! rw.R]

  /** the 0-variant */
  def shift0[R, A, F[+_]](using in: Prompted[R])
                           (f: (A => R ! Shift % ? + F) => R ! Shift % ? + F)(using At): A ! Shift % ? + F =
    shift0[R, A, F](in.prompt)(f)

  /** the 0-variant in a direct block */
  inline def shift0[A](using in: Prompted[?])[F[_]]
                          (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                          (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    Shift.shift0[in.Res, A, Pure](in.prompt)(f.asInstanceOf[(A => in.Res ! Shift % ? + Pure) => in.Res ! Shift % ? + Pure])
      .asInstanceOf[A ! rw.R]

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

  /** a capture to `in`'s delimiter typed at the row `r` chose: the same operation at the same
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

  /** `Shift`'s operations are its own values */
  given Effect[Shift % ?] = new Effect[Shift % ?]:
    def test(x: Any): Boolean = x.isInstanceOf[Shift[?, ?]]

  /** run on the machine under the barrier: a capture with no delimiter is `NoPrompt` */
  def run[R, F[+_]](prog: R ! Shift % ? + F)(using m: Machine[F]): R ! F =
    // a machine outside runs the program, its captures included
    if m.inner then innerDyn(prog) else bounded[R, F](prog)

  /** run without the barrier: a capture with no delimiter goes out, for a machine outside */
  def runNested[R, F[+_]](prog: R ! Shift % ? + F)(using Row.In[Shift % ?, F]): R ! F =
    Free.delay(Nested[R, F](prog, nested = true))

  /** a run with no barrier as a value (a handler frame's, `HandleFrames.pending`) */
  private[okay] def nestedRun[R, F[+_]](prog: R ! Shift % ? + F): Pending[R, F] = Nested[R, F](prog, nested = true)

  /** drop the continuation and answer `value` at `p` */
  def abort[R, A, F[+_]](p: Prompt[R])(value: R)(using at: At): A ! Shift % ? + F =
    op[A, F](Abort[Any, A, R](p, value, at.where))

  /** on the machine: a capture with no delimiter is `NoPrompt`. A value, run by whoever forces it */
  private[okay] def bounded[R, F[+_]](prog: R ! Shift % ? + F): R ! F =
    Free.delay(Nested[R, F](prog, nested = false))

  /**
   * A DELIMITER YOU NAME, ITS KEY ITS OWN TYPE (shift-prompt-key, specs/shift-merge.md stage 3): `reset` hands
   * its body a `Reset[R, F]` — the prompt, and the row `F` OUTSIDE the delimiter, fixed when it is installed —
   * and `Shift % d.type` in the row is a capture to it, `reset` its handler: the third key of the one effect,
   * beside the answer type (`reset`/`shift` at the top level) and `?` (prompts by value, `NoPrompt` possible).
   *
   * The prompt stack is the row, and its ORDER is in the handles: a capture's body is typed at the row of its
   * own delimiter — `shift`'s under it (`Shift % d.type + F`), `shift0`'s outside it (`F`) — and that row was
   * fixed before any delimiter inside `d` existed, so a body cannot name one (it would be captured into `k`,
   * not installed). A shift with no reset, to a foreign delimiter, or to one that escaped its reset leaves a
   * key nothing handles, and the program does not compile where it is run or embedded.
   */
  object Stacked:

    /** what a `reset` hands its body: its prompt, and the row outside it */
    final class Reset[R, F[+_]] private[Shift] (val p: Prompt[R])

    /** a delimiter's key is told apart by VALUE: the operations of `Shift % d.type` are the ones naming `d`'s
     * prompt (so `Distinct` lets two delimiters share a row, as two answer types do) */
    given key[D <: Reset[?, ?] & Singleton](using v: ValueOf[D]): TypeableK.ByValue[Shift % D] = new:
      def test(x: Any): Boolean = Stacked.names(x, v.value.p)

    /** an instance's prompt as its own key (Lexical.Stacked): the same test */
    given promptKey[P <: Prompt[?] & Singleton](using v: ValueOf[P]): TypeableK.ByValue[Shift % P] = new:
      def test(x: Any): Boolean = Stacked.names(x, v.value)

    private def names(x: Any, p: AnyRef): Boolean = promptOf(x) eq p

    /** a fresh delimiter, the body under it with its key in the row; run — or pushed on the machine running */
    def reset[R, F[+_]](body: (d: Reset[R, F]) => R ! Shift % d.type + F)(using m: Machine[F], at: At): R ! F =
      val d = new Reset[R, F](named[R]("reset"))
      Shift.run[R, F](push[R, F](d.p)(toDyn(body(d))))(using m)

    /** `ret $ body`: `ret` runs outside the delimiter, a `shift0` to it takes `ret` along */
    def dollar[R0, R, F[+_]](ret: R0 => R ! F)(body: (d: Reset[R, F]) => R0 ! Shift % d.type + F)
                            (using m: Machine[F], at: At): R ! F =
      val d = new Reset[R, F](named[R]("dollar"))
      Shift.run[R, F](Shift.dollar[R0, R, F](d.p)(retDyn(ret))(toDyn(body(d))))(using m)

    /** capture to `d`; the body runs under `d` — at its own row — and `k` re-installs it */
    def shift[R, F[+_]](d: Reset[R, F])[A](f: (A => R ! Shift % d.type + F) => R ! Shift % d.type + F)
                       (using At): A ! Shift % d.type + F =
      ofDyn(Shift.shift[R, A, F](d.p)(clauseDyn(f)))

    /** capture to `d`, the body OUTSIDE it: typed at the row outside `d` */
    def shift0[R, F[+_]](d: Reset[R, F])[A](f: (A => R ! F) => R ! F)(using At): A ! Shift % d.type + F =
      ofDyn(Shift.shift0[R, A, F](d.p)(clauseDyn(f)))

    /** drop the continuation and answer `value` at `d` */
    def abort[R, F[+_]](d: Reset[R, F])[A](value: R)(using At): A ! Shift % d.type + F =
      ofDyn(Shift.abort[R, A, F](d.p)(value))

    // ---- for a handle made BEFORE its installation (an instance's prompt, Lexical.Stacked): keyed by the
    // prompt, the row outside fixed by the handle's own type — its maker's to keep honest

    private[okay] def dollarAt[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! F)(body: R0 ! Shift % p.type + F)
                                            (using m: Machine[F]): R ! F =
      Shift.run[R, F](Shift.dollar[R0, R, F](p)(retDyn(ret))(toDyn(body)))(using m)

    private[okay] def shift0At[R, A, F[+_]](p: Prompt[R])(f: (A => R ! F) => R ! F)(using At): A ! Shift % p.type + F =
      ofDyn(Shift.shift0[R, A, F](p)(clauseDyn(f)))

  /** a program keyed STATICALLY (by an answer type) as a dynamic one, to mix with captures to prompts by
   * value in one `flatMap`: rows are invariant, so this is the written row coercion (widen-is-a-coercion) */
  def dynamic[A, K, F[+_]](p: A ! Shift % K + F): A ! Shift % ? + F = toDyn[A, K, F](p)

  /** a capture built at `Shift % ?` typed at the evidence's key: the same operation at the same
   * erasure (only the machine reads it), and the evidence says which delimiter it targets — the claim
   * `toDyn`/`ofDyn` below make, made for a block's evidence */
  private[okay] def asRow[A](in: Prompted[?])(q: A ! Shift % ? + in.F): A ! in.K + in.F = q.asInstanceOf[A ! in.K + in.F]

  // THE ONE CLAIM: a `Shift % R` program is a `Shift` program at the same erasure (only the machine reads
  // its operations), and a capture of answer `R` reaches only the prompt of `R`'s key, where its `k` and body are
  // typed in that `reset`'s row.
  private[okay] def toDyn[A, R, F[+_]](q: A ! Shift % R + F): A ! Shift % ? + F = q.asInstanceOf[A ! Shift % ? + F]
  private[okay] def ofDyn[A, R, F[+_]](q: A ! Shift % ? + F): A ! Shift % R + F = q.asInstanceOf[A ! Shift % R + F]
  private[okay] def innerDyn[R, F[+_]](q: R ! Shift % ? + F): R ! F = q.asInstanceOf[R ! F]
  private[okay] def clauseDyn[R, A, F[+_], G[+_]](f: (A => R ! G) => R ! G): (A => R ! Shift % ? + F) => R ! Shift % ? + F =
    f.asInstanceOf[(A => R ! Shift % ? + F) => R ! Shift % ? + F]
  private[okay] def retDyn[R0, R, F[+_]](f: R0 => R ! F): R0 => R ! Shift % ? + F =
    f.asInstanceOf[R0 => R ! Shift % ? + F]

  /** the test reads the prompt, so `Shift % Int + Shift % String` is a good row */
  given typeableK[R](using k: Key[R]): TypeableK.ByValue[Shift % R] = new:
    def test(x: Any): Boolean = promptOf(x) eq k.prompt

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

    inline given of[R]: Key[R] = ${ okay.macros.ShiftMacros.keyImpl[R] }

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
    inline given of[F[+_]]: Machine[F] = ${ okay.macros.ShiftMacros.machineImpl[F] }
    /** for a door that has already read the row (a keyed `reset`'s own machine): no reading again */
    private[okay] def outermost[F[+_]]: Machine[F] = new Machine[F](false)

}
