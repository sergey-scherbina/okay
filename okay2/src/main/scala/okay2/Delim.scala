package okay2


import scala.annotation.{implicitNotFound, tailrec}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import Free.{Return, Inject, Bind}

/**
 * Delimited control as an EFFECT — multi-prompt, in the shape of
 * Dybvig, Peyton Jones and Sabry's "A monadic framework for delimited
 * continuations" (2007): a prompt is a first-class tag carrying the
 * delimiter's answer type, `push` installs one, and `shift` captures
 * up to a NAMED prompt rather than to the nearest one.
 *
 * `push` (reset) is an operation, not a handler application, because
 * capturing across an intervening delimiter is the whole point of
 * multi-prompt and nested handlers cannot do it: one machine has to
 * own the whole prompt stack. Tags are what let several answer types
 * coexist in ONE effect row: with the answer type riding inside the
 * prompt, there is a single `Delim` signature and the tags keep them
 * apart. The price: the operations' payloads are programs in the same
 * row, which a single-parameter signature cannot express, so they are
 * erased here and re-typed inside the machine at exactly two lines.
 *
 * WHAT IS DIFFERENT FROM THE SCALA 3 CORE. Its `Prompted ?=>` doors —
 * `shift[A]`, `exit`, `emit`, `pause`, `onReturn` — read their row
 * and answer type off the `direct` block's context. Scala 2 has no
 * context functions and okay2 has no `direct`, so the evidence is a
 * VALUE passed first: `Delim.shift[Int, Int](in)(k => k(5))`,
 * `Delim.emit(e)(a)`, `Delim.pause(s)(q)`. The evidence carries the
 * row it was made at as a type member (`in.Rest`, the row beside
 * `Delim`), so no door needs the cast the inline Scala 3 doors make,
 * and a body is an ordinary function of its evidence.
 */

/**
 * A delimiter's identity AND its answer type; identity is the tag.
 * `label` is what it is CALLED — "collect @ Walk.scala:12" — built on
 * demand (the Scala 3 core measured 21% on a prompt-per-operation
 * lane when it was interpolated at construction).
 */
final class Prompt[R](val what: String, val where: String) {
  def label: String = what + " @ " + where
  override def toString: String = label
}

/**
 * A capture naming a prompt that is not on THIS machine's stack. The
 * message names the prompt that was wanted, the prompts that are
 * actually installed (innermost first), and the rule that explains
 * the difference.
 */
final class NoPrompt(val from: String, val wanted: String, val installed: List[String])
  extends RuntimeException(NoPrompt.say(from, wanted, installed))

object NoPrompt {
  def say(from: String, wanted: String, installed: List[String]): String = {
    val stack =
      if (installed.isEmpty) "  (none: this machine has no delimiter installed)"
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
        |running are `scope`, `collecting`, `pausing`.""".stripMargin
  }
}

/**
 * WHERE THIS WAS WRITTEN. The Scala 3 core reads the caller's
 * `file:line` with an inline macro; here `At.here` is an implicit def
 * macro that does the same. A Scala 2 def macro cannot expand in the
 * run that defines it, but none has to: every door in this core TAKES
 * its `At` as a parameter and passes it on, so the default is only
 * ever materialised at a caller's site, in another run. A caller that
 * wants a different label installs one lexically:
 * `implicit val at: At = At("Booking.scala:31")`. Lexical scope wins
 * over the companion's default.
 */
final case class At(where: String) extends AnyVal {
  override def toString: String = where
}

object At {
  /** for a call that has no position to offer */
  val unknown: At = At("<unknown>")

  /** the caller's `file:line`, read off the expansion's position */
  implicit def here: At = macro AtMacro.here
}

object AtMacro {
  def here(c: blackbox.Context): c.Tree = {
    import c.universe._
    val pos = c.enclosingPosition
    val where =
      if (pos == NoPosition) "<unknown>"
      else s"${pos.source.file.name}:${pos.line}"
    q"_root_.okay2.At($where)"
  }
}

sealed trait Delim extends Row { type Op[+A] = Delim.Op[A] }

object Delim {
  sealed trait Op[+A]
  /** install a delimiter and run the body under it (reset) */
  final case class Push[R](prompt: Prompt[R], body: Any) extends Op[R]
  /**
   * Capture the continuation up to THIS prompt. The classic family is
   * two independent bits, so it is one operation with two flags:
   *
   *   underPrompt — does f's body run with the delimiter still
   *                 installed? (shift, control: yes; the 0-variants
   *                 consume it)
   *   delimitK    — does invoking the captured continuation re-install
   *                 the delimiter? (shift, shift0: yes; the
   *                 control-variants hand back a bare segment)
   */
  final case class Capture[R, A](prompt: Prompt[R], f: Any, underPrompt: Boolean, delimitK: Boolean, at: String) extends Op[A]
  /**
   * `ret $ body` at the delimiter `prompt` (λ$, Materzok & Biernacki,
   * APLAS 2012): the body answers `R0`, the delimiter answers `R`, and
   * a `shift0` to `prompt` takes `ret` along with the delimiter. Body
   * and `ret` are programs in the machine's row, erased here as
   * `Push`'s body is and re-typed at the machine's claim lines.
   */
  final case class Dollar[R0, R](prompt: Prompt[R], ret: Any, body: Any) extends Op[R]

  implicit val effect: Effect[Delim] = Effect.of[Delim]

  /** a prompt is its own typed token: the same prompt has the same
   * answer type — the witness the machine uses to cut its stack */
  implicit val samePrompt: Same[Prompt] = Same.byIdentity[Prompt]

  /**
   * THE ROW HAS NO MACHINE YET. A machine owns one prompt stack, so
   * starting a SECOND one inside a row that already has `Delim` is the
   * mistake that reads like ordinary code and fails at run time:
   * `resumable` around `collect`. Every combinator that RUNS a machine
   * asks for this; the ones that only install a delimiter (`push`,
   * `scope`, `collecting`, `pausing`) do not. An ABSTRACT `F` reads as
   * absent, as in the Scala 3 core: a guard against the shape people
   * write, not a proof.
   */
  @implicitNotFound("this row already contains Delim, so this would start a SECOND machine, and a capture cannot cross from one machine's prompt stack to another's.\nUse the nested form, which installs a delimiter on the machine already running:\n  delimited -> scope,   collect -> collecting,   resumable -> pausing")
  final class OneMachine[F <: Row] private[Delim] ()

  object OneMachine {
    implicit def fresh[F <: Row](implicit ev: NoDelim[F]): OneMachine[F] = { val _ = ev; new OneMachine[F]() }
  }

  /** `Delim` is not a member of F (`F <:< Delim` fails: the row does
   * not require it) — the usual Scala 2 absence witness:
   * one instance always, two more when the member IS there, so the
   * search is ambiguous exactly when Delim is in the row */
  sealed trait NoDelim[F <: Row]
  object NoDelim {
    private val inst: NoDelim[Pure] = new NoDelim[Pure] {}
    implicit def yes[F <: Row]: NoDelim[F] = inst.asInstanceOf[NoDelim[F]]
    implicit def no1[F <: Row](implicit m: F <:< Delim): NoDelim[F] = { val _ = m; inst.asInstanceOf[NoDelim[F]] }
    implicit def no2[F <: Row](implicit m: F <:< Delim): NoDelim[F] = { val _ = m; inst.asInstanceOf[NoDelim[F]] }
  }

  /** a fresh delimiter tag, labelled with the line that asked for it */
  def prompt[R](implicit at: At): Prompt[R] = named[R]("prompt")(at)

  private def named[R](what: String)(at: At): Prompt[R] = new Prompt[R](what, at.where)

  /** run the body under the delimiter — reset, as an operation */
  def push[R, F <: Row](p: Prompt[R])(body: Free[Delim with F, R]): R ! (Delim + F) =
    Free.inject[Delim, R](Push(p, body)).plus[F]

  /**
   * `ret $ body` at the delimiter `p`: the body answers `R0`, the
   * delimiter answers `R`, and a `shift0` to `p` captures `ret` along
   * with the delimiter. `push(p)(body)` is `dollar(p)(pure)(body)`.
   * The under-prompt captures (`shift`, `control`) run their body under
   * a PLAIN delimiter, APLAS 2012's `S k.e = S0 k.<e>`. The
   * control-variants need the bare segment, which answers `R0` and not
   * `R`, and are refused at a `dollar` (see the machine). The Scala 3
   * core's twin (specs/shift0-dollar.md, okay2-dollar).
   */
  def dollar[R0, R, F <: Row](p: Prompt[R])(ret: R0 => R ! (Delim + F))(body: Free[Delim with F, R0]): R ! (Delim + F) =
    Free.inject[Delim, R](Dollar[R0, R](p, ret, body)).plus[F]

  /**
   * Capture the continuation up to `p` and hand it to `f`. The
   * continuation is a PROGRAM-valued function, so `f` may perform
   * effects around it, invoke it many times, or drop it entirely
   * (an early exit). `shift`: the body runs under the delimiter and
   * the continuation re-installs it.
   */
  def shift[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Delim + F)) => R ! (Delim + F))(implicit at: At): A ! (Delim + F) =
    Free.inject[Delim, A](Capture[R, A](p, f, underPrompt = true, delimitK = true, at = at.where)).plus[F]

  /** the body CONSUMES the delimiter (a further shift to `p` escapes
   * outward), the continuation still re-installs it */
  def shift0[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Delim + F)) => R ! (Delim + F))(implicit at: At): A ! (Delim + F) =
    Free.inject[Delim, A](Capture[R, A](p, f, underPrompt = false, delimitK = true, at = at.where)).plus[F]

  /** the body runs under the delimiter, the continuation does NOT
   * re-install it — a bare segment, spliced where it is invoked */
  def control[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Delim + F)) => R ! (Delim + F))(implicit at: At): A ! (Delim + F) =
    Free.inject[Delim, A](Capture[R, A](p, f, underPrompt = true, delimitK = false, at = at.where)).plus[F]

  /** neither: the delimiter is consumed and the continuation is bare */
  def control0[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Delim + F)) => R ! (Delim + F))(implicit at: At): A ! (Delim + F) =
    Free.inject[Delim, A](Capture[R, A](p, f, underPrompt = false, delimitK = false, at = at.where)).plus[F]

  /** abort to a prompt with a value: a shift that drops the
   * continuation (the 0-variant, so the delimiter goes with it) */
  def abort[R, A, F <: Row](p: Prompt[R])(value: R)(implicit at: At): A ! (Delim + F) =
    shift0[R, A, F](p)(_ => pure[Delim + F, R](value))(at)

  /** the common shape: a fresh prompt, a block under it, run */
  def reset[R, F <: Row](body: Prompt[R] => R ! (Delim + F))(implicit om: OneMachine[F], at: At): R ! F = {
    val p = named[R]("reset")(at)
    run[R, F](push[R, F](p)(body(p)))(om)
  }

  // ==================================================================
  // THE EVIDENCE DOORS
  // ==================================================================

  /**
   * THE DELIMITER IS INSTALLED — the evidence, and the typed door.
   * `NoPrompt` is thrown when a capture names a prompt that is not on
   * the stack; a capture made through THIS cannot, because the only
   * way to hold a `Prompted` is to be inside the `scope`/`delimited`
   * that installed one. The constructor is private to the object, so
   * the evidence cannot be forged.
   *
   * `Rest` is the row beside `Delim` the delimiter was installed on,
   * so a door taking the evidence answers at `Delim + in.Rest` with no
   * type argument for the row and no cast; `Aux[R, F]` spells it for
   * a body's parameter. What it does not catch: the evidence escaping
   * its own scope and being used afterwards — that stays the runtime
   * `NoPrompt`.
   */
  sealed abstract class Prompted[R] private[Delim] (val prompt: Prompt[R]) {
    /** the delimiter's answer type, as a member */
    type Res = R
    /** the row beside Delim the delimiter was installed on */
    type Rest <: okay2.Row
    /** the whole row the body runs in */
    type Row = Delim + Rest
  }

  object Prompted {
    type Aux[R, F <: okay2.Row] = Prompted[R] { type Rest = F }
  }

  private def prompted[R, F <: Row](p: Prompt[R]): Prompted.Aux[R, F] = new Prompted[R](p) { type Rest = F }

  /**
   * INSTALL A DELIMITER, AND NOTHING ELSE: a fresh prompt, the body
   * under it with the evidence in hand, and the machine left to
   * whoever is running it. The half of `delimited` that NESTS: the
   * OUTERMOST combinator runs the machine (`delimited`, `collect`,
   * `resumable`), everything under it installs only (`scope`,
   * `collecting`, `pausing`).
   */
  def scope[R, F <: Row](body: Prompted.Aux[R, F] => R ! (Delim + F))(implicit at: At): R ! (Delim + F) =
    scopeAs[R, F]("scope")(body)(at)

  /** the one place a delimiter is installed: `what` is the door's own
   * name and `at` the caller's position, joined ONCE */
  private def scopeAs[R, F <: Row](what: String)(body: Prompted.Aux[R, F] => R ! (Delim + F))(at: At): R ! (Delim + F) = {
    val p = named[R](what)(at)
    push[R, F](p)(body(prompted[R, F](p)))
  }

  /** install a fresh delimiter, run the body under it with the
   * evidence in hand, and handle the machine — the OUTERMOST form;
   * `scope` is the one that nests */
  def delimited[R, F <: Row](body: Prompted.Aux[R, F] => R ! (Delim + F))(implicit om: OneMachine[F], at: At): R ! F =
    run[R, F](scopeAs[R, F]("delimited")(body)(at))(om)

  /** capture up to the delimiter the evidence names — the same word as
   * the prompt-taking primitive; the row is the evidence's, so the
   * call site writes two type arguments, not three */
  def shift[R, A](in: Prompted[R])(f: (A => R ! (Delim + in.Rest)) => R ! (Delim + in.Rest))(implicit at: At): A ! (Delim + in.Rest) =
    shift[R, A, in.Rest](in.prompt)(f)(at)

  /** the 0-variant: the body consumes the delimiter */
  def shift0[R, A](in: Prompted[R])(f: (A => R ! (Delim + in.Rest)) => R ! (Delim + in.Rest))(implicit at: At): A ! (Delim + in.Rest) =
    shift0[R, A, in.Rest](in.prompt)(f)(at)

  /** the continuation does not re-install the delimiter */
  def control[R, A](in: Prompted[R])(f: (A => R ! (Delim + in.Rest)) => R ! (Delim + in.Rest))(implicit at: At): A ! (Delim + in.Rest) =
    control[R, A, in.Rest](in.prompt)(f)(at)

  /** neither */
  def control0[R, A](in: Prompted[R])(f: (A => R ! (Delim + in.Rest)) => R ! (Delim + in.Rest))(implicit at: At): A ! (Delim + in.Rest) =
    control0[R, A, in.Rest](in.prompt)(f)(at)

  /** abort to the delimiter the evidence names, with a value */
  def abort[R, A](in: Prompted[R])(value: R)(implicit at: At): A ! (Delim + in.Rest) =
    abort[R, A, in.Rest](in.prompt)(value)(at)

  // ==================================================================
  // THE PATTERNS — the four shapes that earn a capture in ordinary
  // code, each under a name that says what it DOES. Every one is two
  // or three lines over `shift`; the value is the name and the
  // evidence.
  // ==================================================================

  /**
   * 1 · LEAVE EARLY WITH AN ANSWER. `Delim.exit(in)(value)` stops there
   * and makes `value` the answer of the scope `in` names; the rest of
   * the program does not run — a capture that DROPS its continuation
   * is what an early return is. It replaces an exception thrown for
   * control flow, a sentinel threaded through every caller, or two
   * loops rewritten as a fold with a flag.
   */
  def exit[R](in: Prompted[R])(value: R)(implicit at: At): Unit ! (Delim + in.Rest) =
    shift[R, Unit](in)(_ => pure[Delim + in.Rest, R](value))(at)

  /**
   * 2 · A PUSH API, READ AS A PULL. The evidence for a block that is
   * collecting values: it carries the element type, the prompt's
   * answer (`Res`: a list for `collect`, a state-passing function for
   * `collectUntil`) and the row, so `emit` needs no type argument and
   * a producer written against `Emitting[A]` runs under either.
   */
  sealed abstract class Emitting[A] {
    type Elem = A
    type Res
    type Rest <: okay2.Row
    type Row = Delim + Rest
    val in: Prompted.Aux[Res, Rest]
    /** what one emit does with the rest of the producer `k` */
    def onEmit(a: A)(k: Unit => Res ! Row): Res ! Row
  }

  object Emitting {
    type Aux[A, F <: okay2.Row] = Emitting[A] { type Rest = F }
  }

  /** `collect`'s evidence: the list is built on the way BACK, by the
   * continuation — `emit` conses after the rest of the producer has
   * answered, which is why the producer never has to know */
  private final class Listing[A, F <: Row](val in: Prompted.Aux[List[A], F]) extends Emitting[A] {
    type Res = List[A]
    type Rest = F
    def onEmit(a: A)(k: Unit => List[A] ! (Delim + F)): List[A] ! (Delim + F) = k(()).map(a :: _)
  }

  /**
   * `collectUntil`'s evidence: the state is passed on the way DOWN
   * through the prompt's answer, which is a FUNCTION of it — `PState`'s
   * trick over `Cont`, here over the prompt. An emit answers `s => …`
   * at once; applying it adds the element, and either ends with
   * `fo.end` — the continuation never called, the rest of the producer
   * never run — or resumes `k`, whose own answer is the next such
   * function. Nothing is mutated, so a multi-shot capture inside the
   * producer sees its own state. No cast: the evidence's row IS the
   * row the door captures at, by the type member.
   */
  private final class Stopping[A, S, R, F <: Row](val in: Prompted.Aux[S => R ! (Delim + F), F], fo: FoldUntil[A, S, R])
    extends Emitting[A] {
    type Res = S => R ! (Delim + F)
    type Rest = F
    def onEmit(a: A)(k: Unit => Res ! (Delim + F)): Res ! (Delim + F) =
      pure[Delim + F, Res]((s: S) => {
        val s2 = fo.add(s, a)
        if (fo.done(s2)) pure[Delim + F, R](fo.end(s2))
        else k(()).flatMap(f => f(s2))
      })
  }

  /** run `body`, which emits, and answer with everything it emitted, in
   * order — the producer stays an ordinary walk, the consumer gets a
   * list */
  def collect[A, F <: Row](body: Emitting.Aux[A, F] => Unit ! (Delim + F))(implicit om: OneMachine[F], at: At): List[A] ! F =
    run[List[A], F](collectAs[A, F]("collect")(body)(at))(om)

  /** the same collection, NESTED: it installs its delimiter and leaves
   * the machine to the `delimited`/`resumable` around it, so a capture
   * from inside — a `pause`, an `exit` to an outer scope — crosses it */
  def collecting[A, F <: Row](body: Emitting.Aux[A, F] => Unit ! (Delim + F))(implicit at: At): List[A] ! (Delim + F) =
    collectAs[A, F]("collecting")(body)(at)

  private def collectAs[A, F <: Row](what: String)(body: Emitting.Aux[A, F] => Unit ! (Delim + F))(at: At): List[A] ! (Delim + F) =
    scopeAs[List[A], F](what)(in => body(new Listing[A, F](in)).map(_ => List.empty[A]))(at)

  /**
   * A COLLECT THAT STOPS: run the SAME producer `collect` runs, fold
   * what it emits with `fo`, and stop the producer where `done` first
   * holds — `take(3)` over a tree walk runs the walk to its third leaf
   * and no further. The answer is `fo.end` of the state the emits
   * built. `done(init)` runs no body at all.
   */
  def collectUntil[A, S, R, F <: Row](fo: FoldUntil[A, S, R])(body: Emitting.Aux[A, F] => Unit ! (Delim + F))
                                     (implicit om: OneMachine[F], at: At): R ! F =
    if (fo.done(fo.init)) pure[F, R](fo.end(fo.init))
    else run[R, F](collectUntilAs[A, S, R, F]("collectUntil")(fo)(body)(at))(om)

  /** the nested half of `collectUntil`, as `collecting` is of `collect` */
  def collectingUntil[A, S, R, F <: Row](fo: FoldUntil[A, S, R])(body: Emitting.Aux[A, F] => Unit ! (Delim + F))
                                        (implicit at: At): R ! (Delim + F) =
    if (fo.done(fo.init)) pure[Delim + F, R](fo.end(fo.init))
    else collectUntilAs[A, S, R, F]("collectingUntil")(fo)(body)(at)

  private def collectUntilAs[A, S, R, F <: Row](what: String)(fo: FoldUntil[A, S, R])
                                                (body: Emitting.Aux[A, F] => Unit ! (Delim + F))(at: At): R ! (Delim + F) =
    scopeAs[S => R ! (Delim + F), F](what)(in =>
      body(new Stopping[A, S, R, F](in, fo))
        // the producer ended on its own: the state's own answer
        .map(_ => (s: S) => pure[Delim + F, R](fo.end(s))))(at)
      // the first emit's function, applied to the start; each
      // application resumes the producer up to the next emit
      .flatMap(f => f(fo.init))

  /** emit one value into the `collect` (or `collectUntil`) in force */
  def emit[A](e: Emitting[A])(a: A)(implicit at: At): Unit ! (Delim + e.Rest) =
    shift[e.Res, Unit](e.in)(k => e.onEmit(a)(k))(at)

  /**
   * 3 · STOP IN THE MIDDLE, CARRY ON LATER. What a paused program is:
   * either it is asking, and the REST OF IT is right there as
   * `resume`, or it is finished — Queinnec's web dialogue as a type,
   * and the shape of an approval gate, a wizard, a REPL. `G` is the row
   * the paused program still runs in, `Delim` and all; `Dialogue`
   * spells it for a caller who only knows their own row.
   */
  sealed trait Paused[Q, A, R, G <: Row] {
    /** the answer, if it has one */
    def finished: Option[R]
    /** the question it is waiting on, if it is waiting */
    def asking: Option[Q]
    /** WHERE it is waiting — the `pause`'s own position */
    def where: Option[String]
  }

  object Paused {
    final case class Ask[Q, A, R, G <: Row](question: Q, resume: A => Paused[Q, A, R, G] ! G, at: String) extends Paused[Q, A, R, G] {
      def finished: Option[R] = None
      def asking: Option[Q] = Some(question)
      def where: Option[String] = Some(at)
    }
    final case class Done[Q, A, R, G <: Row](value: R) extends Paused[Q, A, R, G] {
      def finished: Option[R] = Some(value)
      def asking: Option[Q] = None
      def where: Option[String] = None
    }
  }

  /** a paused program whose caller's row is `F` */
  type Dialogue[Q, A, R, F <: Row] = Paused[Q, A, R, Delim + F]

  /** the evidence for a block that may pause, carrying the question,
   * answer and final types and the row as members */
  sealed abstract class Asking[Q, A, R] {
    type Qst = Q
    type Ans = A
    type Fin = R
    type Rest <: okay2.Row
    type Row = Delim + Rest
    val in: Prompted.Aux[Paused[Q, A, R, Delim + Rest], Rest]
  }

  object Asking {
    type Aux[Q, A, R, F <: okay2.Row] = Asking[Q, A, R] { type Rest = F }
  }

  private final class AskingAt[Q, A, R, F <: Row](val in: Prompted.Aux[Paused[Q, A, R, Delim + F], F]) extends Asking[Q, A, R] {
    type Rest = F
  }

  /** run `body` until it pauses or finishes, and answer with WHICH — a
   * state machine with a `step` column, replaced by straight-line code
   * whose record is the continuation */
  def resumable[Q, A, R, F <: Row](body: Asking.Aux[Q, A, R, F] => R ! (Delim + F))
                                  (implicit om: OneMachine[F], at: At): Dialogue[Q, A, R, F] ! F =
    run[Dialogue[Q, A, R, F], F](pausingAs[Q, A, R, F]("resumable")(body)(at))(om)

  /** the same, NESTED: the dialogue's delimiter goes on the machine
   * already running */
  def pausing[Q, A, R, F <: Row](body: Asking.Aux[Q, A, R, F] => R ! (Delim + F))(implicit at: At): Dialogue[Q, A, R, F] ! (Delim + F) =
    pausingAs[Q, A, R, F]("pausing")(body)(at)

  private def pausingAs[Q, A, R, F <: Row](what: String)(body: Asking.Aux[Q, A, R, F] => R ! (Delim + F))(at: At): Dialogue[Q, A, R, F] ! (Delim + F) =
    scopeAs[Dialogue[Q, A, R, F], F](what)(in =>
      body(new AskingAt[Q, A, R, F](in)).map(r => Paused.Done[Q, A, R, Delim + F](r)))(at)

  /** ask, and hand the rest of the program back to the caller */
  def pause[Q, A, R](s: Asking[Q, A, R])(q: Q)(implicit at: At): A ! (Delim + s.Rest) =
    shift[Paused[Q, A, R, Delim + s.Rest], A](s.in)(k => pure(Paused.Ask(q, k, at.where)))(at)

  /** answer every question until the dialogue is done — the driver for
   * the common case where the answers are available now */
  def drive[Q, A, R, F <: Row](p: Dialogue[Q, A, R, F])(answer: Q => A ! F)(implicit om: OneMachine[F]): R ! F =
    p match {
      case Paused.Done(r) => pure[F, R](r)
      case Paused.Ask(q, resume, _) =>
        answer(q).flatMap(a => run[Dialogue[Q, A, R, F], F](resume(a))(om).flatMap(drive[Q, A, R, F](_)(answer)(om)))
    }

  /**
   * ...AND OUTLIVE THE PROCESS. A continuation is a closure and cannot
   * be written to disk; the JOURNAL — the answers the dialogue has
   * been given, in order — can, and where it stands is RE-DERIVED by
   * running the program again over them (`replay`). Exact under the
   * discipline `Replayable` states.
   */
  type Journal[A] = List[A]

  /** answer the question a dialogue is asking, and keep the answer:
   * the pair is what you persist after every step */
  def answer[Q, A, R, F <: Row](p: Dialogue[Q, A, R, F], j: Journal[A])(a: A)(implicit om: OneMachine[F]): (Dialogue[Q, A, R, F], Journal[A]) ! F =
    p match {
      case Paused.Ask(_, resume, _) => run[Dialogue[Q, A, R, F], F](resume(a))(om).map(next => (next, j :+ a))
      case done => pure[F, (Dialogue[Q, A, R, F], Journal[A])]((done, j))
    }

  /** where the dialogue stands, from its program and its journal —
   * what replaces persisting a continuation */
  def replay[Q, A, R, F <: Row](body: Asking.Aux[Q, A, R, F] => R ! (Delim + F))(j: Journal[A])
                               (implicit om: OneMachine[F], rp: Replayable[Delim + F], at: At): Dialogue[Q, A, R, F] ! F = {
    val _ = rp
    j.foldLeft(resumable[Q, A, R, F](body)(om, at)) { (acc, a) =>
      acc.flatMap {
        case Paused.Ask(_, resume, _) => run[Dialogue[Q, A, R, F], F](resume(a))(om)
        case done => pure[F, Dialogue[Q, A, R, F]](done)   // more answers than questions
      }
    }
  }

  /**
   * 4 · DO SOMETHING ON THE WAY BACK. `Delim.onReturn(in)(f)` runs the
   * rest of the scope and then puts its answer through `f`: the rest
   * of the program is a value here, so it can be measured, logged,
   * undone, or have a compensation folded into its answer — from a
   * place in the middle, without the code around it restructured.
   */
  def onReturn[R](in: Prompted[R])(f: R => R)(implicit at: At): Unit ! (Delim + in.Rest) =
    shift[R, Unit](in)(k => k(()).map(f))(at)

  // ==================================================================
  // THE MACHINE
  // ==================================================================

  /**
   * The machine's continuation, TYPED: a chain from the current answer
   * A to the run's answer Z. `K` is one Bind's continuation (types
   * chain through it), `Mark` a delimiter carrying its prompt — and so
   * the answer type of the program under it. `Done` carries the
   * equality it means: scalac 2 refines a pattern's OWN type
   * parameters from the scrutinee but not the method's from a pattern
   * (`Refl() extends Eq[A, A]` does not make `A = B` in a match), so
   * the witness travels as a value and `reify`/`loop` apply it.
   */
  private sealed trait Segs[F <: Row, A, Z]
  private object Segs {
    final case class Done[F <: Row, A, Z](ev: A =:= Z) extends Segs[F, A, Z]
    final case class K[F <: Row, X, Y, Z](f: X => Y ! (Delim + F), rest: Segs[F, Y, Z]) extends Segs[F, X, Z]
    final case class Mark[F <: Row, X, Z](p: Prompt[X], rest: Segs[F, X, Z]) extends Segs[F, X, Z]
    /** a `dollar` delimiter: the body's X0 leaves through `ret` into
     * the prompt's X */
    final case class Ret[F <: Row, X0, X, Z](p: Prompt[X], ret: X0 => X ! (Delim + F), rest: Segs[F, X, Z]) extends Segs[F, X0, Z]
  }

  /** the stack cut at a prompt. TWO SHAPES, as in the Scala 3 core: a
   * `dollar` changes the answer type and a plain mark does not */
  private sealed trait Cut[F <: Row, A, P, Z]

  /** a plain mark: the chain up to it, answering the prompt's P */
  private final case class Plain[F <: Row, A, P, Z](captured: Segs[F, A, P], outer: Segs[F, P, Z]) extends Cut[F, A, P, Z]

  /** a `dollar`: the chain answers the body's `P0`, and `close` (the
   * `Ret` with its function) leads to `P`. A delimited continuation is
   * `captured` then `close`, so `ret` goes with it (`$/S0`). `P0` is an
   * existential spelled as a type MEMBER, so `captured` and `close` stay
   * linked through one stable value, with no cast. */
  private sealed abstract class AtRet[F <: Row, A, P, Z] extends Cut[F, A, P, Z] {
    type P0
    val captured: Segs[F, A, P0]
    val close: Segs[F, P0, P]
    val outer: Segs[F, P, Z]
  }
  private def atRet[F <: Row, A, Q, P, Z](c: Segs[F, A, Q], cl: Segs[F, Q, P], o: Segs[F, P, Z]): AtRet[F, A, P, Z] =
    new AtRet[F, A, P, Z] {
      type P0 = Q
      val captured: Segs[F, A, Q] = c
      val close: Segs[F, Q, P] = cl
      val outer: Segs[F, P, Z] = o
    }

  /**
   * The segments `cut` walked past, as a type-aligned stack
   * (specs/stack-safety.md): from a chain leading out of Y it builds one
   * leading out of A, for ANY end Q. Scala 2 refines no method type
   * parameter by a match, so each step is a METHOD on the node: `Top`
   * is finished, `On` hands back the frame below it with one more
   * segment wrapped. No cast.
   */
  private sealed trait Wrap[F <: Row, A, Y] { def step[Q](c: Segs[F, Y, Q]): Unwound[F, A, Q] }
  private final case class Top[F <: Row, A]() extends Wrap[F, A, A] {
    def step[Q](c: Segs[F, A, Q]): Unwound[F, A, Q] = Unwound.Finished(c)
  }
  private final case class On[F <: Row, A, X, Y](under: Wrap[F, A, X], frame: Frame[F, X, Y]) extends Wrap[F, A, Y] {
    def step[Q](c: Segs[F, Y, Q]): Unwound[F, A, Q] = Unwound.More(under, frame(c))
  }
  /** one segment put back around a chain, whatever that chain's end */
  private trait Frame[F <: Row, X, Y] { def apply[Q](c: Segs[F, Y, Q]): Segs[F, X, Q] }

  private sealed trait Unwound[F <: Row, A, Q]
  private object Unwound {
    final case class Finished[F <: Row, A, Q](c: Segs[F, A, Q]) extends Unwound[F, A, Q]
    final case class More[F <: Row, A, X, Q](w: Wrap[F, A, X], c: Segs[F, X, Q]) extends Unwound[F, A, Q]
  }

  @tailrec private def unwind[F <: Row, A, Q](u: Unwound[F, A, Q]): Segs[F, A, Q] = u match {
    case Unwound.Finished(c) => c
    case m: Unwound.More[F, A, x, Q] => unwind(m.w.step(m.c))
  }

  /** where the walk is: the chain still to search, and what it passed */
  private final case class Walk[F <: Row, A, X, Z](kont: Segs[F, X, Z], w: Wrap[F, A, X])

  /** the machine's state between steps: a program and the stack it
   * continues into, the head type an abstract member so a
   * monomorphic `@tailrec` loop can carry it (a polymorphic local loop
   * cannot be tail-recursive at changing type arguments in scalac 2) */
  private sealed abstract class Next[F <: Row, Z] {
    type A
    val prog: A ! (Delim + F)
    val kont: Segs[F, A, Z]
  }

  private def next[F <: Row, X, Z](p: Free[Delim with F, X], k: Segs[F, X, Z]): Next[F, Z] = new Next[F, Z] {
    type A = X
    val prog: X ! (Delim + F) = p
    val kont: Segs[F, X, Z] = k
  }

  /**
   * The machine. The freer tree's Bind nodes already reify
   * continuations as plain functions, so the tree IS the control stack
   * — the machine only keeps the segment chain and the prompt markers
   * in it. A captured segment is turned back INTO A PROGRAM (`reify`),
   * which is why the continuation is an ordinary value: multi-shot
   * comes for free, and nothing is a closure over interpreter state.
   *
   * Two claims and no other cast: a Push's body and a Capture's f are
   * programs in this machine's row, erased at the operation because
   * the row's other half F is not the operation's to name — re-typed
   * here, at their two lines, where F is known.
   */
  def run[R, F <: Row](prog: Free[Delim with F, R])(implicit om: OneMachine[F]): R ! F = {
    val _ = om
    machine[R, F](prog, None)
  }

  /**
   * THE MACHINE THAT FORWARDS INSTEAD OF THROWING — for a row that
   * still has a `Delim` in it, i.e. one running inside another
   * machine. A capture naming a prompt this machine does not hold is
   * re-emitted into the residual program, and this machine resumes
   * with the same stack when the answer arrives — the foreign-
   * operation path, verbatim; the outer machine, which does hold the
   * prompt, then captures across this machine's frames. The evidence
   * is what makes it well-typed and cast-free: `F <:< Delim` (the
   * residual row still requires Delim), so the operation lands in it
   * by the evidence's own substitution.
   */
  def runNested[R, F <: Row](prog: Free[Delim with F, R])(implicit in: F <:< Delim): R ! F =
    machine[R, F](prog, Some(in))

  private def machine[R, F <: Row](prog: Free[Delim with F, R], forward: Option[F <:< Delim]): R ! F = {
    type Rw = Delim + F
    type Prog[X] = X ! Rw

    /** frames back into a program: binds become flatMaps, markers
     * become pushes — the continuation re-installs its delimiter */
    @tailrec def reify[A, P](segs: Segs[F, A, P], start: Prog[A]): Prog[P] = segs match {
      case Segs.Done(ev) => ev.substituteCo[Prog](start)
      case Segs.K(f, rest) => reify(rest, start.flatMap(f))
      case Segs.Mark(p, rest) => reify(rest, Free.inject[Delim, A](Push(p, start)).plus[F])
      case r: Segs.Ret[F, A, x, P] => reify(r.rest, Free.inject[Delim, x](Dollar[A, x](r.p, r.ret, start)).plus[F])
    }

    /** the delimiters this machine has installed, innermost first —
     * what `NoPrompt` prints; only ever runs on the failing path */
    def installed(kont: Segs[F, _, R]): List[String] = {
      @tailrec def go(k: Segs[F, _, R], acc: List[String]): List[String] = k match {
        case Segs.Done(_) => acc.reverse
        case Segs.Mark(q, rest) => go(rest, q.label :: acc)
        case Segs.Ret(q, _, rest) => go(rest, q.label :: acc)
        case Segs.K(_, rest) => go(rest, acc)
      }
      go(kont, Nil)
    }

    /** cut the chain at the mark of p: the mark's prompt IS p by
     * identity, and Same's witness makes the mark's type P's */
    def cut[A, P, Z](kont: Segs[F, A, Z], p: Prompt[P]): Option[Cut[F, A, P, Z]] =
      walk[A, P, Z](Walk(kont, Top[F, A]()), p)

    // A LOOP (specs/stack-safety.md): each segment passed on the way
    // down is pushed onto `Wrap`, and the captured part is wrapped in
    // them once the mark is found. The recursive version rebuilt the
    // prefix on the way back up, one frame per segment, and a shift
    // under 20 000 other delimiters overflowed.
    @tailrec def walk[A, P, Z](at: Walk[F, A, _, Z], p: Prompt[P]): Option[Cut[F, A, P, Z]] = at match {
      case here: Walk[F, A, x, Z] => here.kont match {
        case Segs.Done(_) => None
        case m: Segs.Mark[F, x, Z] => samePrompt.same(m.p, p) match {
          case Some(ev) =>
            Some(Plain(unwind(here.w.step(Segs.Done[F, x, P](ev))), ev.substituteCo[({ type L[t] = Segs[F, t, Z] })#L](m.rest)))
          case None =>
            walk[A, P, Z](Walk(m.rest, On(here.w, new Frame[F, x, x] {
              def apply[Q](c: Segs[F, x, Q]): Segs[F, x, Q] = Segs.Mark(m.p, c)
            })), p)
        }
        case r: Segs.Ret[F, x, x1, Z] => samePrompt.same(r.p, p) match {
          case Some(ev) =>
            // the body's chain ends at x; the Ret leads from x to the prompt's P
            Some(atRet[F, A, x, P, Z](unwind(here.w.step(Segs.Done[F, x, x](implicitly[x =:= x]))),
              Segs.Ret[F, x, x1, P](r.p, r.ret, Segs.Done[F, x1, P](ev)),
              ev.substituteCo[({ type L[t] = Segs[F, t, Z] })#L](r.rest)))
          case None =>
            walk[A, P, Z](Walk(r.rest, On(here.w, new Frame[F, x, x1] {
              def apply[Q](c: Segs[F, x1, Q]): Segs[F, x, Q] = Segs.Ret(r.p, r.ret, c)
            })), p)
        }
        case k: Segs.K[F, x, y, Z] =>
          walk[A, P, Z](Walk(k.rest, On(here.w, new Frame[F, x, y] {
            def apply[Q](c: Segs[F, y, Q]): Segs[F, x, Q] = Segs.K(k.f, c)
          })), p)
      }
    }

    // the split as a pattern (okay2-split-at-rest): a Delim operation is
    // an arm of the loop and its continuation the loop's own tail call,
    // where `split` answered each step through two closures and an Either
    val Mine = Split.at[Delim]

    def _loop(state: Next[F, R]): R ! F = loop(state)

    // ONE tail-recursive loop: only a FOREIGN operation (or a forwarded
    // capture) suspends, under a flatMap closure, and the Delim ops
    // themselves are flat
    @tailrec def loop(state: Next[F, R]): R ! F = Free.resume(state.prog) match {
      case Return(x) => state.kont match {
        case Segs.Done(ev) => pure[F, R](ev(x))
        case Segs.K(f, rest) => loop(next(f(x), rest))
        // the delimited block finished normally: drop its marker
        case Segs.Mark(_, rest) => loop(next(pure[Rw, state.A](x), rest))
        // a `dollar` finished normally: leave it, through its return
        case Segs.Ret(_, ret, rest) => loop(next(ret(x), rest))
      }
      case Inject(e) => loop(next(Bind(Inject[Rw, state.A](e), (y: state.A) => Return[Rw, state.A](y)), state.kont))
      case Bind(Inject(Mine(op)), k) =>
        val kont: Segs[F, Any, R] = Segs.K(k, state.kont)
        op match {
          case pu: Push[r] =>
            // claim 1: the pushed body answers the prompt's r in this
            // row; r is an answer of the op, which K carries up
            val body = pu.body.asInstanceOf[Prog[r]]
            loop(next(body, Segs.Mark(pu.prompt, Segs.K((a: r) => pure[Rw, Any](a), kont))))

          case cap: Capture[p, a] =>
            // claim 2: f takes a continuation into the prompt's answer
            // and gives back a program at it, in this row. shift/control
            // put the body back under the delimiter; the 0-variants have
            // consumed it
            def resume(k: a => Prog[p], outer: Segs[F, p, R]): Next[F, R] = {
              val body = cap.f.asInstanceOf[(a => Prog[p]) => Prog[p]](k)
              if (cap.underPrompt) next(Free.inject[Delim, p](Push(cap.prompt, body)).plus[F], outer)
              else next(body, outer)
            }
            // shift/shift0 re-install the delimiter (a dollar's with its
            // return function, `$/S0`); control/control0 hand back the
            // bare segment, which answers the prompt's type only at a
            // plain mark
            cut(kont, cap.prompt) match {
              // AtRet BEFORE Plain: in the other order scalac 2.13 calls
              // this arm "unreachable code" (a false reachability verdict on
              // the type pattern); the two cases are disjoint either way
              case Some(r: AtRet[F, Any, p, R]) =>
                if (cap.delimitK) loop(resume((v: a) => reify(r.close, reify(r.captured, pure[Rw, Any](v))), r.outer))
                else throw new UnsupportedOperationException(
                  s"${cap.at}: a control-capture to ${cap.prompt.label}, which is a `dollar`: its bare continuation answers the body's type, not the prompt's (specs/shift0-dollar.md)")
              case Some(Plain(captured, outer)) =>
                loop(resume((v: a) => {
                  val seg = reify(captured, pure[Rw, Any](v))
                  if (cap.delimitK) Free.inject[Delim, p](Push(cap.prompt, seg)).plus[F] else seg
                }, outer))
              case None => forward match {
                case Some(in) =>
                  // re-emit, and resume this machine with the same
                  // stack; `kont` is immutable, so a multi-shot outer
                  // capture may re-enter it as often as it likes
                  in.substituteContra[({ type L[-x] = Free[x with Row, Any] })#L](Free.inject[Delim, Any](cap))
                    .flatMap(x => _loop(next(pure[Rw, Any](x), kont)))
                case None => throw new NoPrompt(cap.at, cap.prompt.label, installed(kont))
              }
            }

          case d: Dollar[r0, r] =>
            // claims 1b and 1c, the same as Push's: the body answers r0
            // and ret leads from r0 to the prompt's r, in this row
            val body = d.body.asInstanceOf[Prog[r0]]
            val ret = d.ret.asInstanceOf[r0 => Prog[r]]
            loop(next(body, Segs.Ret[F, r0, r, R](d.prompt, ret, Segs.K((a: r) => pure[Rw, Any](a), kont))))
        }
      // a foreign operation suspends the machine: the residual program
      // performs it and resumes with the same stack
      case Bind(Inject(g), k) =>
        val kont: Segs[F, Any, R] = Segs.K(k, state.kont)
        Inject[F, Any](g).flatMap(x => _loop(next(pure[Rw, Any](x), kont)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(next(prog, Segs.Done[F, R, R](implicitly[R =:= R])))
  }

  // ==================================================================
  // THE PROMPT STACK IN THE TYPE
  //
  // `NoPrompt` — a capture naming a prompt that is not installed — is
  // a run-time exception on every door above. Here it is a compile
  // error: the stack of installed prompts is carried as a TYPE, an
  // HList of prompt identities (the Scala 3 core spells it as a tuple
  // `p.type *: S`; Scala 2 has no `*:`, so `Cons[P, S]`/`Empty`), and a
  // capture asks for evidence (`Has`) that its prompt's singleton type
  // is on the stack in force. The three shapes that throw on the doors
  // above are refused by the compiler: a shift with no reset (there is
  // no stack to call `shift` on), a shift to a foreign prompt of the
  // same answer type (`Has` fails on its identity), and a prompt that
  // ESCAPED its reset and is shifted to afterwards (after the reset
  // returns the stack in force is the OUTER one, which has no
  // `inner.p.type` in it).
  //
  // WHAT IS DIFFERENT FROM THE SCALA 3 CORE, and why. There the stack
  // is a lexical given and the doors are functions; a body is a
  // DEPENDENT function `(s: In[R, S]) => Under[F, R, s.p.type *: S]`,
  // and the indexed `Prog` facade types every program by the stack it
  // was built under. Scala 2 has no dependent function types, so the
  // stack is a VALUE the body receives (`in.stack`), the doors are its
  // METHODS (which is also what lets `Has` resolve with the stack's `S`
  // fixed by the receiver rather than inferred), and a program is an
  // ordinary `A ! (Delim + F)`. The one hole that leaves, said here: a
  // program BUILT under an inner stack, leaked out of its reset and
  // run afterwards, is still a run-time `NoPrompt` — the `Prog` index
  // that closes it needs the dependent body type.
  //
  //     Delim.Stacked.delimited[Int, Pure] { s =>
  //       s.stack.shift[Int, Int, Pure](s.p)(k => k(5).map(_ * 2))   // 10
  //     }
  //
  // ADDITIVE: every door above keeps its spelling.
  // ==================================================================
  object Stacked {

    /** the stack of installed prompts, as a type: an HList of prompt
     * identities, innermost first */
    sealed trait Stk
    sealed trait Empty extends Stk
    sealed trait Cons[P, S <: Stk] extends Stk

    /** "P is on the stack S" — the evidence that replaces the throw */
    @implicitNotFound("prompt ${P} is not on the prompt stack ${S}: a shift names the prompt of a reset it is INSIDE (in.stack.shift(in.p)(…) under Delim.Stacked.delimited/reset) — not one that has returned, and not one another reset made")
    sealed trait Has[S <: Stk, P]
    object Has {
      private val inst: Has[Empty, Nothing] = new Has[Empty, Nothing] {}
      implicit def here[P, S <: Stk]: Has[Cons[P, S], P] = inst.asInstanceOf[Has[Cons[P, S], P]]
      implicit def there[P, Q, S <: Stk](implicit h: Has[S, P]): Has[Cons[Q, S], P] = { val _ = h; inst.asInstanceOf[Has[Cons[Q, S], P]] }
    }

    /**
     * The stack in force, as a value whose type is the stack: the doors
     * are its methods, so a capture is `in.stack.shift(in.p)(f)` and the
     * evidence `Has[S, p.type]` is resolved with S fixed by the receiver.
     * A `Stack` comes only from `delimited` (empty) and `reset` (one
     * more prompt), which is what makes "a shift with no reset" a
     * compile error: there is nothing to call it on.
     */
    final class Stack[S <: Stk] private[Stacked] () {

      /** capture up to `p` — REQUIRES `p` on the stack in force. The
       * evidence is the whole point and is otherwise unused. */
      def shift[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Delim + F)) => R ! (Delim + F))(implicit ev: Has[S, p.type], at: At): A ! (Delim + F) = {
        val _ = ev
        Delim.shift[R, A, F](p)(f)(at)
      }

      /** `Delim.control`, stacked: the continuation is a bare segment */
      def control[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Delim + F)) => R ! (Delim + F))(implicit ev: Has[S, p.type], at: At): A ! (Delim + F) = {
        val _ = ev
        Delim.control[R, A, F](p)(f)(at)
      }

      /** drop the continuation and answer `value` at `p` */
      def abort[R, A, F <: Row](p: Prompt[R])(value: R)(implicit ev: Has[S, p.type], at: At): A ! (Delim + F) = {
        val _ = ev
        Delim.abort[R, A, F](p)(value)(at)
      }

      /**
       * A fresh prompt pushed on this stack, for the body only: the
       * nested delimiter. The body receives the new `In`, whose `stack`
       * has one more prompt; after it returns the stack in force is
       * this one — which is what refuses a shift to its prompt from
       * outside.
       */
      def reset[R, F <: Row](body: In[R, S] => R ! (Delim + F))(implicit at: At): R ! (Delim + F) = {
        val in = new In[R, S](named[R]("reset")(at))
        push[R, F](in.p)(body(in))
      }

      // `shift0`/`control0` are NOT here: their body runs with the
      // delimiter CONSUMED, so its stack is the part of `S` below `p` —
      // a type-level function this stage does not price. The unstacked
      // doors remain.
    }

    /** what `reset`/`delimited` hand their body: the prompt, and the
     * stack that installing it made */
    final class In[R, S <: Stk] private[Stacked] (val p: Prompt[R]) {
      val stack: Stack[Cons[p.type, S]] = new Stack[Cons[p.type, S]]()
    }

    /** the root: a fresh prompt on an EMPTY stack, the body under it,
     * the machine run — `Delim.delimited`'s job, with the stack in the
     * type. Every stacked program starts here. */
    def delimited[R, F <: Row](body: In[R, Empty] => R ! (Delim + F))(implicit om: OneMachine[F], at: At): R ! F = {
      val in = new In[R, Empty](named[R]("delimited")(at))
      run[R, F](push[R, F](in.p)(body(in)))(om)
    }
  }
}
