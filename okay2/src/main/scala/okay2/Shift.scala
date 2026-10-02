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
 * prompt, there is a single `Shift[Any]` signature and the tags keep them
 * apart. The price: the operations' payloads are programs in the same
 * row, which a single-parameter signature cannot express, so they are
 * erased here and re-typed inside the machine at exactly two lines.
 *
 * WHAT IS DIFFERENT FROM THE SCALA 3 CORE. Its `Prompted ?=>` doors —
 * `shift[A]`, `exit`, `emit`, `pause`, `onReturn` — read their row
 * and answer type off the `direct` block's context. Scala 2 has no
 * context functions and okay2 has no `direct`, so the evidence is a
 * VALUE passed first: `Shift.shift[Int, Int](in)(k => k(5))`,
 * `Shift.emit(e)(a)`, `Shift.pause(s)(q)`. The evidence carries the
 * row it was made at as a type member (`in.Rest`, the row beside
 * `Shift[Any]`), so no door needs the cast the inline Scala 3 doors make,
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
        |ONE `Shift.run` PER PROGRAM. A machine owns one prompt stack,
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

/**
 * Delimited control, ONE effect (specs/shift-merge.md's twin, okay2-shift-merge): `Shift[K]`, keyed.
 * `Shift[R]` in the row is a capture to the nearest `reset` of answer `R`, and `reset` is its handler (the
 * Scala 3 core's `Shift % R`, specs/shift-effect.md); `Shift[Any]` is the dynamic form, a capture to a
 * `Prompt[R]` VALUE made at run time (the core's `Shift % ?`, the operator's "Shift % Any"), until okay2 its
 * own effect `Shift[Any]`. Both run on one machine (`Shift.run`); no value of this type is made. Scala 2 has no context functions, so there is no
 * short `shift[A]` inside a `reset { }`: a capture names its answer, value and row, as `Shift.shift` does.
 */
sealed trait Shift[K] extends Row with Shift.AnyKey { type Op[+A] = Shift.Op[A] }

/** the level-1 doors, mixed into the package object: `shift`, `shift0`, `reset` */
trait Shifts {

  /** Danvy-Filinski's capture: the body runs under its `reset`, so it may capture to it again; `k` re-installs it */
  def shift[R, A, F <: Row](f: (A => R ! (Shift[R] + F)) => R ! (Shift[R] + F))(implicit k: Shift.Key[R], at: At): A ! (Shift[R] + F) =
    Shift.out[A, R, F](Shift.shift[R, A, F](k.prompt)(Shift.clause[R, A, Shift[R] + F, F](f))(at))

  /** the body runs outside its `reset`; `k` re-installs it */
  def shift0[R, A, F <: Row](f: (A => R ! F) => R ! F)(implicit k: Shift.Key[R], at: At): A ! (Shift[R] + F) =
    Shift.out[A, R, F](Shift.shift0[R, A, F](k.prompt)(Shift.clause[R, A, F, F](f))(at))

  /** delimit, and answer every capture of answer `R` */
  def reset[R, F <: Row](body: R ! (Shift[R] + F))(implicit k: Shift.Key[R], n: Shift.Nesting[F]): R ! F = {
    val pushed = Shift.push[R, F](k.prompt)(Shift.in[R, R, F](body))
    // a row that still holds a capture's effect is run by the machine outside
    if (n.inner) Shift.inner[R, F](pushed) else Shift.runReset[R, F](pushed)
  }
}

object Shift {
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
  final case class Dollar[R0, R](prompt: Prompt[R], ret: Any, body: Any, shots: Shots) extends Op[R]

  /**
   * How many times the machine has entered a watched `dollar` through
   * ONE capture (the Scala 3 core's `Shift.Shots`, lexical-tail-guard-
   * abort): a capture that takes the delimiter gets a fresh count, so
   * `n > 1` means "this same captured context has been RUN a second
   * time". `null` on a plain dollar. Counted when the reified program is
   * stepped, not when `k` builds it.
   */
  final class Shots(val resumed: Int => Unit) { var n: Int = 0 }

  implicit val effect: Effect[Shift[Any]] = Effect.of[Shift[Any]]

  /** a prompt is its own typed token: the same prompt has the same
   * answer type — the witness the machine uses to cut its stack */
  implicit val samePrompt: Same[Prompt] = Same.byIdentity[Prompt]

  /**
   * THE ROW HAS NO MACHINE YET. A machine owns one prompt stack, so
   * starting a SECOND one inside a row that already has a `Shift` (of any key) is the
   * mistake that reads like ordinary code and fails at run time:
   * `resumable` around `collect`. Every combinator that RUNS a machine
   * asks for this; the ones that only install a delimiter (`push`,
   * `scope`, `collecting`, `pausing`) do not. An ABSTRACT `F` reads as
   * absent, as in the Scala 3 core: a guard against the shape people
   * write, not a proof.
   */
  @implicitNotFound("this row already contains a Shift, so this would start a SECOND machine, and a capture cannot cross from one machine's prompt stack to another's.\nUse the nested form, which installs a delimiter on the machine already running:\n  delimited -> scope,   collect -> collecting,   resumable -> pausing")
  final class OneMachine[F <: Row] private[Shift] ()

  object OneMachine {
    implicit def fresh[F <: Row](implicit ev: NoMachine[F]): OneMachine[F] = { val _ = ev; new OneMachine[F]() }
    /** for a door that decided the row by its own evidence (`Shift.Nesting`): no search, no promise checked */
    private[okay2] def unchecked[F <: Row]: OneMachine[F] = new OneMachine[F]()
  }

  /** no `Shift` of ANY key is a member of F (`F <:< Shift.AnyKey` fails: the
   * row does not require one) — ONE machine guard for the static and the
   * dynamic form, as the core's `OneMachine` reads `Shift[?, Any]`
   * (okay2-shift-merge; it was `Delim` alone). The usual Scala 2 absence
   * witness: one instance always, two more when the member IS there, so
   * the search is ambiguous exactly when a Shift is in the row */
  sealed trait NoMachine[F <: Row]
  object NoMachine {
    private val inst: NoMachine[Pure] = new NoMachine[Pure] {}
    implicit def yes[F <: Row]: NoMachine[F] = inst.asInstanceOf[NoMachine[F]]
    implicit def no1[F <: Row](implicit m: F <:< Shift.AnyKey): NoMachine[F] = { val _ = m; inst.asInstanceOf[NoMachine[F]] }
    implicit def no2[F <: Row](implicit m: F <:< Shift.AnyKey): NoMachine[F] = { val _ = m; inst.asInstanceOf[NoMachine[F]] }
  }

  /** a fresh delimiter tag, labelled with the line that asked for it */
  def prompt[R](implicit at: At): Prompt[R] = named[R]("prompt")(at)

  private def named[R](what: String)(at: At): Prompt[R] = new Prompt[R](what, at.where)

  /** run the body under the delimiter — reset, as an operation */
  def push[R, F <: Row](p: Prompt[R])(body: Free[Shift[Any] with F, R]): R ! (Shift[Any] + F) =
    Free.inject[Shift[Any], R](Push(p, body)).plus[F]

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
  def dollar[R0, R, F <: Row](p: Prompt[R])(ret: R0 => R ! (Shift[Any] + F))(body: Free[Shift[Any] with F, R0]): R ! (Shift[Any] + F) =
    Free.inject[Shift[Any], R](Dollar[R0, R](p, ret, body, null)).plus[F]

  /** `dollar`, told each time the machine enters it: `resumed(1)` at the
   * call itself and at the first run of each capture that took the
   * delimiter, `resumed(2)` at that capture's second run, and so on —
   * what a `ret` cannot see, since a resumption that leaves by `abort`
   * never returns through it (okay2-lexical, the Scala 3 core's twin) */
  def dollarResumed[R0, R, F <: Row](p: Prompt[R])(ret: R0 => R ! (Shift[Any] + F), resumed: Int => Unit)(body: Free[Shift[Any] with F, R0]): R ! (Shift[Any] + F) =
    Free.inject[Shift[Any], R](Dollar[R0, R](p, ret, body, new Shots(resumed))).plus[F]

  /**
   * Capture the continuation up to `p` and hand it to `f`. The
   * continuation is a PROGRAM-valued function, so `f` may perform
   * effects around it, invoke it many times, or drop it entirely
   * (an early exit). `shift`: the body runs under the delimiter and
   * the continuation re-installs it.
   */
  def shift[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F))(implicit at: At): A ! (Shift[Any] + F) =
    Free.inject[Shift[Any], A](Capture[R, A](p, f, underPrompt = true, delimitK = true, at = at.where)).plus[F]

  /** the body CONSUMES the delimiter (a further shift to `p` escapes
   * outward), the continuation still re-installs it */
  def shift0[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F))(implicit at: At): A ! (Shift[Any] + F) =
    Free.inject[Shift[Any], A](Capture[R, A](p, f, underPrompt = false, delimitK = true, at = at.where)).plus[F]

  /** the body runs under the delimiter, the continuation does NOT
   * re-install it — a bare segment, spliced where it is invoked */
  def control[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F))(implicit at: At): A ! (Shift[Any] + F) =
    Free.inject[Shift[Any], A](Capture[R, A](p, f, underPrompt = true, delimitK = false, at = at.where)).plus[F]

  /** neither: the delimiter is consumed and the continuation is bare */
  def control0[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F))(implicit at: At): A ! (Shift[Any] + F) =
    Free.inject[Shift[Any], A](Capture[R, A](p, f, underPrompt = false, delimitK = false, at = at.where)).plus[F]

  /** abort to a prompt with a value: a shift that drops the
   * continuation (the 0-variant, so the delimiter goes with it) */
  def abort[R, A, F <: Row](p: Prompt[R])(value: R)(implicit at: At): A ! (Shift[Any] + F) =
    shift0[R, A, F](p)(_ => pure[Shift[Any] + F, R](value))(at)

  /** the common shape: a fresh prompt, a block under it, run */
  def reset[R, F <: Row](body: Prompt[R] => R ! (Shift[Any] + F))(implicit om: OneMachine[F], at: At): R ! F = {
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
   * `Rest` is the row beside `Shift[Any]` the delimiter was installed on,
   * so a door taking the evidence answers at `Shift[Any] + in.Rest` with no
   * type argument for the row and no cast; `Aux[R, F]` spells it for
   * a body's parameter. What it does not catch: the evidence escaping
   * its own scope and being used afterwards — that stays the runtime
   * `NoPrompt`.
   */
  sealed abstract class Prompted[R] private[Shift] (val prompt: Prompt[R]) {
    /** the delimiter's answer type, as a member */
    type Res = R
    /** the row beside Shift[Any] the delimiter was installed on */
    type Rest <: okay2.Row
    /** the whole row the body runs in */
    type Row = Shift[Any] + Rest
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
  def scope[R, F <: Row](body: Prompted.Aux[R, F] => R ! (Shift[Any] + F))(implicit at: At): R ! (Shift[Any] + F) =
    scopeAs[R, F]("scope")(body)(at)

  /** the one place a delimiter is installed: `what` is the door's own
   * name and `at` the caller's position, joined ONCE */
  private def scopeAs[R, F <: Row](what: String)(body: Prompted.Aux[R, F] => R ! (Shift[Any] + F))(at: At): R ! (Shift[Any] + F) = {
    val p = named[R](what)(at)
    push[R, F](p)(body(prompted[R, F](p)))
  }

  /** install a fresh delimiter, run the body under it with the
   * evidence in hand, and handle the machine — the OUTERMOST form;
   * `scope` is the one that nests */
  def delimited[R, F <: Row](body: Prompted.Aux[R, F] => R ! (Shift[Any] + F))(implicit om: OneMachine[F], at: At): R ! F =
    run[R, F](scopeAs[R, F]("delimited")(body)(at))(om)

  /** capture up to the delimiter the evidence names — the same word as
   * the prompt-taking primitive; the row is the evidence's, so the
   * call site writes two type arguments, not three */
  def shift[R, A](in: Prompted[R])(f: (A => R ! (Shift[Any] + in.Rest)) => R ! (Shift[Any] + in.Rest))(implicit at: At): A ! (Shift[Any] + in.Rest) =
    shift[R, A, in.Rest](in.prompt)(f)(at)

  /** the 0-variant: the body consumes the delimiter */
  def shift0[R, A](in: Prompted[R])(f: (A => R ! (Shift[Any] + in.Rest)) => R ! (Shift[Any] + in.Rest))(implicit at: At): A ! (Shift[Any] + in.Rest) =
    shift0[R, A, in.Rest](in.prompt)(f)(at)

  /** the continuation does not re-install the delimiter */
  def control[R, A](in: Prompted[R])(f: (A => R ! (Shift[Any] + in.Rest)) => R ! (Shift[Any] + in.Rest))(implicit at: At): A ! (Shift[Any] + in.Rest) =
    control[R, A, in.Rest](in.prompt)(f)(at)

  /** neither */
  def control0[R, A](in: Prompted[R])(f: (A => R ! (Shift[Any] + in.Rest)) => R ! (Shift[Any] + in.Rest))(implicit at: At): A ! (Shift[Any] + in.Rest) =
    control0[R, A, in.Rest](in.prompt)(f)(at)

  /** abort to the delimiter the evidence names, with a value */
  def abort[R, A](in: Prompted[R])(value: R)(implicit at: At): A ! (Shift[Any] + in.Rest) =
    abort[R, A, in.Rest](in.prompt)(value)(at)

  // ==================================================================
  // THE PATTERNS — the four shapes that earn a capture in ordinary
  // code, each under a name that says what it DOES. Every one is two
  // or three lines over `shift`; the value is the name and the
  // evidence.
  // ==================================================================

  /**
   * 1 · LEAVE EARLY WITH AN ANSWER. `Shift.exit(in)(value)` stops there
   * and makes `value` the answer of the scope `in` names; the rest of
   * the program does not run — a capture that DROPS its continuation
   * is what an early return is. It replaces an exception thrown for
   * control flow, a sentinel threaded through every caller, or two
   * loops rewritten as a fold with a flag.
   */
  def exit[R](in: Prompted[R])(value: R)(implicit at: At): Unit ! (Shift[Any] + in.Rest) =
    shift[R, Unit](in)(_ => pure[Shift[Any] + in.Rest, R](value))(at)

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
    type Row = Shift[Any] + Rest
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
    def onEmit(a: A)(k: Unit => List[A] ! (Shift[Any] + F)): List[A] ! (Shift[Any] + F) = k(()).map(a :: _)
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
  private final class Stopping[A, S, R, F <: Row](val in: Prompted.Aux[S => R ! (Shift[Any] + F), F], fo: FoldUntil[A, S, R])
    extends Emitting[A] {
    type Res = S => R ! (Shift[Any] + F)
    type Rest = F
    def onEmit(a: A)(k: Unit => Res ! (Shift[Any] + F)): Res ! (Shift[Any] + F) =
      pure[Shift[Any] + F, Res]((s: S) => {
        val s2 = fo.add(s, a)
        if (fo.done(s2)) pure[Shift[Any] + F, R](fo.end(s2))
        else k(()).flatMap(f => f(s2))
      })
  }

  /** run `body`, which emits, and answer with everything it emitted, in
   * order — the producer stays an ordinary walk, the consumer gets a
   * list */
  def collect[A, F <: Row](body: Emitting.Aux[A, F] => Unit ! (Shift[Any] + F))(implicit om: OneMachine[F], at: At): List[A] ! F =
    run[List[A], F](collectAs[A, F]("collect")(body)(at))(om)

  /** the same collection, NESTED: it installs its delimiter and leaves
   * the machine to the `delimited`/`resumable` around it, so a capture
   * from inside — a `pause`, an `exit` to an outer scope — crosses it */
  def collecting[A, F <: Row](body: Emitting.Aux[A, F] => Unit ! (Shift[Any] + F))(implicit at: At): List[A] ! (Shift[Any] + F) =
    collectAs[A, F]("collecting")(body)(at)

  private def collectAs[A, F <: Row](what: String)(body: Emitting.Aux[A, F] => Unit ! (Shift[Any] + F))(at: At): List[A] ! (Shift[Any] + F) =
    scopeAs[List[A], F](what)(in => body(new Listing[A, F](in)).map(_ => List.empty[A]))(at)

  /**
   * A COLLECT THAT STOPS: run the SAME producer `collect` runs, fold
   * what it emits with `fo`, and stop the producer where `done` first
   * holds — `take(3)` over a tree walk runs the walk to its third leaf
   * and no further. The answer is `fo.end` of the state the emits
   * built. `done(init)` runs no body at all.
   */
  def collectUntil[A, S, R, F <: Row](fo: FoldUntil[A, S, R])(body: Emitting.Aux[A, F] => Unit ! (Shift[Any] + F))
                                     (implicit om: OneMachine[F], at: At): R ! F =
    if (fo.done(fo.init)) pure[F, R](fo.end(fo.init))
    else run[R, F](collectUntilAs[A, S, R, F]("collectUntil")(fo)(body)(at))(om)

  /** the nested half of `collectUntil`, as `collecting` is of `collect` */
  def collectingUntil[A, S, R, F <: Row](fo: FoldUntil[A, S, R])(body: Emitting.Aux[A, F] => Unit ! (Shift[Any] + F))
                                        (implicit at: At): R ! (Shift[Any] + F) =
    if (fo.done(fo.init)) pure[Shift[Any] + F, R](fo.end(fo.init))
    else collectUntilAs[A, S, R, F]("collectingUntil")(fo)(body)(at)

  private def collectUntilAs[A, S, R, F <: Row](what: String)(fo: FoldUntil[A, S, R])
                                                (body: Emitting.Aux[A, F] => Unit ! (Shift[Any] + F))(at: At): R ! (Shift[Any] + F) =
    scopeAs[S => R ! (Shift[Any] + F), F](what)(in =>
      body(new Stopping[A, S, R, F](in, fo))
        // the producer ended on its own: the state's own answer
        .map(_ => (s: S) => pure[Shift[Any] + F, R](fo.end(s))))(at)
      // the first emit's function, applied to the start; each
      // application resumes the producer up to the next emit
      .flatMap(f => f(fo.init))

  /** emit one value into the `collect` (or `collectUntil`) in force */
  def emit[A](e: Emitting[A])(a: A)(implicit at: At): Unit ! (Shift[Any] + e.Rest) =
    shift[e.Res, Unit](e.in)(k => e.onEmit(a)(k))(at)

  /**
   * 3 · STOP IN THE MIDDLE, CARRY ON LATER. What a paused program is:
   * either it is asking, and the REST OF IT is right there as
   * `resume`, or it is finished — Queinnec's web dialogue as a type,
   * and the shape of an approval gate, a wizard, a REPL. `G` is the row
   * the paused program still runs in, `Shift[Any]` and all; `Dialogue`
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
  type Dialogue[Q, A, R, F <: Row] = Paused[Q, A, R, Shift[Any] + F]

  /** the evidence for a block that may pause, carrying the question,
   * answer and final types and the row as members */
  sealed abstract class Asking[Q, A, R] {
    type Qst = Q
    type Ans = A
    type Fin = R
    type Rest <: okay2.Row
    type Row = Shift[Any] + Rest
    val in: Prompted.Aux[Paused[Q, A, R, Shift[Any] + Rest], Rest]
  }

  object Asking {
    type Aux[Q, A, R, F <: okay2.Row] = Asking[Q, A, R] { type Rest = F }
  }

  private final class AskingAt[Q, A, R, F <: Row](val in: Prompted.Aux[Paused[Q, A, R, Shift[Any] + F], F]) extends Asking[Q, A, R] {
    type Rest = F
  }

  /** run `body` until it pauses or finishes, and answer with WHICH — a
   * state machine with a `step` column, replaced by straight-line code
   * whose record is the continuation */
  def resumable[Q, A, R, F <: Row](body: Asking.Aux[Q, A, R, F] => R ! (Shift[Any] + F))
                                  (implicit om: OneMachine[F], at: At): Dialogue[Q, A, R, F] ! F =
    run[Dialogue[Q, A, R, F], F](pausingAs[Q, A, R, F]("resumable")(body)(at))(om)

  /** the same, NESTED: the dialogue's delimiter goes on the machine
   * already running */
  def pausing[Q, A, R, F <: Row](body: Asking.Aux[Q, A, R, F] => R ! (Shift[Any] + F))(implicit at: At): Dialogue[Q, A, R, F] ! (Shift[Any] + F) =
    pausingAs[Q, A, R, F]("pausing")(body)(at)

  private def pausingAs[Q, A, R, F <: Row](what: String)(body: Asking.Aux[Q, A, R, F] => R ! (Shift[Any] + F))(at: At): Dialogue[Q, A, R, F] ! (Shift[Any] + F) =
    scopeAs[Dialogue[Q, A, R, F], F](what)(in =>
      body(new AskingAt[Q, A, R, F](in)).map(r => Paused.Done[Q, A, R, Shift[Any] + F](r)))(at)

  /** ask, and hand the rest of the program back to the caller */
  def pause[Q, A, R](s: Asking[Q, A, R])(q: Q)(implicit at: At): A ! (Shift[Any] + s.Rest) =
    shift[Paused[Q, A, R, Shift[Any] + s.Rest], A](s.in)(k => pure(Paused.Ask(q, k, at.where)))(at)

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
  def replay[Q, A, R, F <: Row](body: Asking.Aux[Q, A, R, F] => R ! (Shift[Any] + F))(j: Journal[A])
                               (implicit om: OneMachine[F], rp: Replayable[Shift[Any] + F], at: At): Dialogue[Q, A, R, F] ! F = {
    val _ = rp
    j.foldLeft(resumable[Q, A, R, F](body)(om, at)) { (acc, a) =>
      acc.flatMap {
        case Paused.Ask(_, resume, _) => run[Dialogue[Q, A, R, F], F](resume(a))(om)
        case done => pure[F, Dialogue[Q, A, R, F]](done)   // more answers than questions
      }
    }
  }

  /**
   * 4 · DO SOMETHING ON THE WAY BACK. `Shift.onReturn(in)(f)` runs the
   * rest of the scope and then puts its answer through `f`: the rest
   * of the program is a value here, so it can be measured, logged,
   * undone, or have a compensation folded into its answer — from a
   * place in the middle, without the code around it restructured.
   */
  def onReturn[R](in: Prompted[R])(f: R => R)(implicit at: At): Unit ! (Shift[Any] + in.Rest) =
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
    final case class K[F <: Row, X, Y, Z](f: X => Y ! (Shift[Any] + F), rest: Segs[F, Y, Z]) extends Segs[F, X, Z]
    final case class Mark[F <: Row, X, Z](p: Prompt[X], rest: Segs[F, X, Z]) extends Segs[F, X, Z]
    /** a `dollar` delimiter: the body's X0 leaves through `ret` into
     * the prompt's X */
    final case class Ret[F <: Row, X0, X, Z](p: Prompt[X], ret: X0 => X ! (Shift[Any] + F), shots: Shots, rest: Segs[F, X, Z]) extends Segs[F, X0, Z]
  }

  /** the copy of a `Ret` a capture takes with it: a watched dollar gets a
   * FRESH count, one per capture (`Shots`) */
  private def retake[F <: Row, X0, X, Q](r: Segs.Ret[F, X0, X, _], c: Segs[F, X, Q]): Segs.Ret[F, X0, X, Q] =
    Segs.Ret(r.p, r.ret, if (r.shots == null) null else new Shots(r.shots.resumed), c)

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
    val prog: A ! (Shift[Any] + F)
    val kont: Segs[F, A, Z]
  }

  private def next[F <: Row, X, Z](p: Free[Shift[Any] with F, X], k: Segs[F, X, Z]): Next[F, Z] = new Next[F, Z] {
    type A = X
    val prog: X ! (Shift[Any] + F) = p
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
  def run[R, F <: Row](prog: Free[Shift[Any] with F, R])(implicit om: OneMachine[F]): R ! F = {
    val _ = om
    machine[R, F](prog, None)
  }

  /**
   * THE MACHINE THAT FORWARDS INSTEAD OF THROWING — for a row that
   * still has a `Shift[Any]` in it, i.e. one running inside another
   * machine. A capture naming a prompt this machine does not hold is
   * re-emitted into the residual program, and this machine resumes
   * with the same stack when the answer arrives — the foreign-
   * operation path, verbatim; the outer machine, which does hold the
   * prompt, then captures across this machine's frames. The evidence
   * is what makes it well-typed and cast-free: `F <:< Shift[Any]` (the
   * residual row still requires Shift[Any]), so the operation lands in it
   * by the evidence's own substitution.
   */
  def runNested[R, F <: Row](prog: Free[Shift[Any] with F, R])(implicit in: F <:< Shift[Any]): R ! F =
    machine[R, F](prog, Some(in))

  private def machine[R, F <: Row](prog: Free[Shift[Any] with F, R], forward: Option[F <:< Shift[Any]]): R ! F = {
    type Rw = Shift[Any] + F
    type Prog[X] = X ! Rw

    /** frames back into a program: binds become flatMaps, markers
     * become pushes — the continuation re-installs its delimiter */
    @tailrec def reify[A, P](segs: Segs[F, A, P], start: Prog[A]): Prog[P] = segs match {
      case Segs.Done(ev) => ev.substituteCo[Prog](start)
      case Segs.K(f, rest) => reify(rest, start.flatMap(f))
      case Segs.Mark(p, rest) => reify(rest, Free.inject[Shift[Any], A](Push(p, start)).plus[F])
      case r: Segs.Ret[F, A, x, P] => reify(r.rest, Free.inject[Shift[Any], x](Dollar[A, x](r.p, r.ret, start, r.shots)).plus[F])
    }

    /** the delimiters this machine has installed, innermost first —
     * what `NoPrompt` prints; only ever runs on the failing path */
    def installed(kont: Segs[F, _, R]): List[String] = {
      @tailrec def go(k: Segs[F, _, R], acc: List[String]): List[String] = k match {
        case Segs.Done(_) => acc.reverse
        case Segs.Mark(q, rest) => go(rest, q.label :: acc)
        case Segs.Ret(q, _, _, rest) => go(rest, q.label :: acc)
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
              retake[F, x, x1, P](r, Segs.Done[F, x1, P](ev)),
              ev.substituteCo[({ type L[t] = Segs[F, t, Z] })#L](r.rest)))
          case None =>
            walk[A, P, Z](Walk(r.rest, On(here.w, new Frame[F, x, x1] {
              def apply[Q](c: Segs[F, x1, Q]): Segs[F, x, Q] = retake(r, c)
            })), p)
        }
        case k: Segs.K[F, x, y, Z] =>
          walk[A, P, Z](Walk(k.rest, On(here.w, new Frame[F, x, y] {
            def apply[Q](c: Segs[F, y, Q]): Segs[F, x, Q] = Segs.K(k.f, c)
          })), p)
      }
    }

    // the split as a pattern (okay2-split-at-rest): a Shift[Any] operation is
    // an arm of the loop and its continuation the loop's own tail call,
    // where `split` answered each step through two closures and an Either
    val Mine = Split.at[Shift[Any]]

    def _loop(state: Next[F, R]): R ! F = loop(state)

    // ONE tail-recursive loop: only a FOREIGN operation (or a forwarded
    // capture) suspends, under a flatMap closure, and the Shift[Any] ops
    // themselves are flat
    @tailrec def loop(state: Next[F, R]): R ! F = Free.resume(state.prog) match {
      case Return(x) => state.kont match {
        case Segs.Done(ev) => pure[F, R](ev(x))
        case Segs.K(f, rest) => loop(next(f(x), rest))
        // the delimited block finished normally: drop its marker
        case Segs.Mark(_, rest) => loop(next(pure[Rw, state.A](x), rest))
        // a `dollar` finished normally: leave it, through its return
        case Segs.Ret(_, ret, _, rest) => loop(next(ret(x), rest))
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
              if (cap.underPrompt) next(Free.inject[Shift[Any], p](Push(cap.prompt, body)).plus[F], outer)
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
                  if (cap.delimitK) Free.inject[Shift[Any], p](Push(cap.prompt, seg)).plus[F] else seg
                }, outer))
              case None => forward match {
                case Some(in) =>
                  // re-emit, and resume this machine with the same
                  // stack; `kont` is immutable, so a multi-shot outer
                  // capture may re-enter it as often as it likes
                  in.substituteContra[({ type L[-x] = Free[x with Row, Any] })#L](Free.inject[Shift[Any], Any](cap))
                    .flatMap(x => _loop(next(pure[Rw, Any](x), kont)))
                case None => throw new NoPrompt(cap.at, cap.prompt.label, installed(kont))
              }
            }

          case d: Dollar[r0, r] =>
            // claims 1b and 1c, the same as Push's: the body answers r0
            // and ret leads from r0 to the prompt's r, in this row
            val body = d.body.asInstanceOf[Prog[r0]]
            val ret = d.ret.asInstanceOf[r0 => Prog[r]]
            // a watched dollar is told it is being entered — here, when the
            // program is RUN, so a continuation built and dropped does not count
            val shots = d.shots
            if (shots != null) { shots.n += 1; shots.resumed(shots.n) }
            loop(next(body, Segs.Ret[F, r0, r, R](d.prompt, ret, shots, Segs.K((a: r) => pure[Rw, Any](a), kont))))
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
  // ordinary `A ! (Shift[Any] + F)`. The one hole that leaves, said here: a
  // program BUILT under an inner stack, leaked out of its reset and
  // run afterwards, is still a run-time `NoPrompt` — the `Prog` index
  // that closes it needs the dependent body type.
  //
  //     Shift.Stacked.delimited[Int, Pure] { s =>
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
    @implicitNotFound("prompt ${P} is not on the prompt stack ${S}: a shift names the prompt of a reset it is INSIDE (in.stack.shift(in.p)(…) under Shift.Stacked.delimited/reset) — not one that has returned, and not one another reset made")
    sealed trait Has[S <: Stk, P] {
      /** the stack BELOW p: what a capture to p leaves in force, since it
       * takes p and every delimiter installed inside it. Found by the
       * same induction that finds p (okay2-dollar; the Scala 3 core's
       * `Has.Below`) */
      type Below <: Stk
    }
    object Has {
      type Aux[S <: Stk, P, B <: Stk] = Has[S, P] { type Below = B }
      implicit def here[P, S <: Stk]: Aux[Cons[P, S], P, S] = new Has[Cons[P, S], P] { type Below = S }
      implicit def there[P, Q, S <: Stk, B <: Stk](implicit h: Aux[S, P, B]): Aux[Cons[Q, S], P, B] = {
        val _ = h
        new Has[Cons[Q, S], P] { type Below = B }
      }
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
      def shift[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F))(implicit ev: Has[S, p.type], at: At): A ! (Shift[Any] + F) = {
        val _ = ev
        Shift.shift[R, A, F](p)(f)(at)
      }

      /** `Shift.control`, stacked: the continuation is a bare segment */
      def control[R, A, F <: Row](p: Prompt[R])(f: (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F))(implicit ev: Has[S, p.type], at: At): A ! (Shift[Any] + F) = {
        val _ = ev
        Shift.control[R, A, F](p)(f)(at)
      }

      /** drop the continuation and answer `value` at `p` */
      def abort[R, A, F <: Row](p: Prompt[R])(value: R)(implicit ev: Has[S, p.type], at: At): A ! (Shift[Any] + F) = {
        val _ = ev
        Shift.abort[R, A, F](p)(value)(at)
      }

      /**
       * A fresh prompt pushed on this stack, for the body only: the
       * nested delimiter. The body receives the new `In`, whose `stack`
       * has one more prompt; after it returns the stack in force is
       * this one — which is what refuses a shift to its prompt from
       * outside.
       */
      def reset[R, F <: Row](body: In[R, S] => R ! (Shift[Any] + F))(implicit at: At): R ! (Shift[Any] + F) = {
        val in = new In[R, S](named[R]("reset")(at))
        push[R, F](in.p)(body(in))
      }

      /**
       * `Shift.shift0`, stacked: the body runs with `p` CONSUMED, under
       * the stack BELOW it (ICFP 2011's rule for S0), which it receives
       * as a `Stack[B]`: a shift to `p` from the body is refused at
       * compile time, and a shift to a prompt below `p` resolves. `k`
       * re-installs `p`, so it answers under the same `B`.
       *
       * TWO STEPS, `stack.shift0(p).apply { below => k => … }`: the
       * evidence must be found before the body is typed (the body's
       * stack is `ev.Below`), and a Scala 2 implicit list is the LAST
       * one, so it would take a block written right after `(p)` as the
       * implicit argument (memory implicit-paren-list-eats-next-call).
       */
      def shift0[R, A, F <: Row](p: Prompt[R])(implicit ev: Has[S, p.type], at: At): Shift0[R, A, F, ev.Below] =
        new Shift0[R, A, F, ev.Below](p, at)

      /**
       * `Shift.dollar`, stacked: a fresh prompt on this stack for the
       * body, and `ret` run OUTSIDE it, under this stack. A `shift0` to
       * its prompt takes `ret` along.
       */
      def dollar[R0, R, F <: Row](ret: R0 => R ! (Shift[Any] + F))(body: In[R, S] => R0 ! (Shift[Any] + F))(implicit at: At): R ! (Shift[Any] + F) = {
        val in = new In[R, S](named[R]("dollar")(at))
        Shift.dollar[R0, R, F](in.p)(ret)(body(in))
      }

      /** `Shift.dollarResumed`, stacked: `dollar` told each time the
       * machine enters it (the stacked tail instance's guard,
       * okay2-lexical-walk-stacked) */
      def dollarResumed[R0, R, F <: Row](ret: R0 => R ! (Shift[Any] + F), resumed: Int => Unit)(body: In[R, S] => R0 ! (Shift[Any] + F))(implicit at: At): R ! (Shift[Any] + F) = {
        val in = new In[R, S](named[R]("dollar")(at))
        Shift.dollarResumed[R0, R, F](in.p)(ret, resumed)(body(in))
      }

      // `control0` is NOT here, as in the Scala 3 core: its continuation
      // is a bare segment run where `p` is gone, but the code inside it
      // was typed with `p` on its stack (specs/shift0-dollar.md,
      // Decisions). The unstacked door remains.
    }

    /** the second step of a stacked `shift0`: the body, under `B` */
    final class Shift0[R, A, F <: Row, B <: Stk] private[Stacked] (p: Prompt[R], at: At) {
      def apply(f: Stack[B] => (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F)): A ! (Shift[Any] + F) =
        Shift.shift0[R, A, F](p)(k => f(new Stack[B]())(k))(at)
    }

    /** what `reset`/`delimited` hand their body: the prompt, and the
     * stack that installing it made */
    final class In[R, S <: Stk] private[Stacked] (val p: Prompt[R]) {
      val stack: Stack[Cons[p.type, S]] = new Stack[Cons[p.type, S]]()
    }

    /** the root: a fresh prompt on an EMPTY stack, the body under it,
     * the machine run — `Shift.delimited`'s job, with the stack in the
     * type. Every stacked program starts here. */
    def delimited[R, F <: Row](body: In[R, Empty] => R ! (Shift[Any] + F))(implicit om: OneMachine[F], at: At): R ! F = {
      val in = new In[R, Empty](named[R]("delimited")(at))
      run[R, F](push[R, F](in.p)(body(in)))(om)
    }
  }

  // THE STATIC FORM (okay2-level1-api): keyed by the answer type

  /** a static program as a dynamic one, so it mixes with captures to prompts by value in one `flatMap`: rows
   * are a written coercion here as in the core (`Shift.dynamic`, specs/shift-merge.md) */
  def dynamic[A, K, F <: Row](p: A ! (Shift[K] + F)): A ! (Shift[Any] + F) = toDyn[A, K, F](p)

  // a static program IS a dynamic one at the same erasure: one machine reads both, the key only picks the
  // delimiter its captures target, which their prompt already names
  private def toDyn[A, K, F <: Row](p: A ! (Shift[K] + F)): A ! (Shift[Any] + F) = p.asInstanceOf[A ! (Shift[Any] + F)]

  /** every `Shift[K]`, whatever the key: what `Nesting` and the one machine guard look for in a row */
  sealed trait AnyKey extends Row

  // THE NAMED PATTERNS (shift-patterns): the captures most programs want, so an early exit and a generator
  // need no `Shift[Any]` in sight. In this object, as in the Scala 3 core: a top-level `collect` would clash.

  /** leave the nearest `reset` of answer `R` now, with its answer `v`: what follows is dropped */
  def exit[R, A, F <: Row](v: R)(implicit k: Key[R], at: At): A ! (Shift[R] + F) =
    okay2.shift0[R, A, F](_ => pure[F, R](v))

  /** a generator: run `body`, and answer everything it `emit`ted, in order. The core's static `collect`; here
   * its own name, because an overload beside the dynamic `collect(body: Emitting => …)` costs that one's lambda
   * its parameter type in Scala 2 (measured: "missing parameter type") */
  def gather[W, F <: Row](body: Unit ! (Shift[List[W]] + F))(implicit k: Key[List[W]], n: Nesting[F]): List[W] ! F =
    okay2.reset[List[W], F](body.map(_ => Nil))

  /** inside `gather`: hand `w` out, and go on */
  def emit[W, F <: Row](w: W)(implicit k: Key[List[W]], at: At): Unit ! (Shift[List[W]] + F) =
    okay2.shift0[List[W], Unit, F](k => k(()).map(w :: _))

  /** `reset` as a value, for the handler-value doors */
  def handle[R, F <: Row](p: R ! (Shift[R] + F))(implicit k: Key[R], n: Nesting[F]): R ! F = okay2.reset[R, F](p)

  /** level 2: the program as a `Cont` whose answers are programs: `c / k` is `reset(q >>= k)` */
  def cont[A, R, F <: Row](q: A ! (Shift[R] + F))(implicit k: Key[R], n: Nesting[F]): Cont[A, R ! F, R ! F] =
    Cont.shift[A, R ! F, R ! F](kk => okay2.reset[R, F](q.flatMap[Shift[R] + F, R](a => kk(a))))

  /** level 2: a whole `Cont` as one capture */
  def embed[A, R, F <: Row](c: Cont[A, R ! F, R ! F])(implicit k: Key[R], at: At): A ! (Shift[R] + F) =
    okay2.shift0[R, A, F](kk => c / kk)

  /**
   * A `reset` that runs its own machine runs it INSIDE whatever forced it, and nested resets of one answer type
   * each start one: JVM depth grows with the nesting. So the runs are counted per thread, and past the room the
   * next one runs on a fresh stack, as Cont's strict `k` does (StackSwitch, specs/cont-stack.md Layer 2). A
   * level is taken as ~4 KB cold, Cont's ~1.2 KB scaled. The Scala 3 core's twin.
   */
  private val room: Int = {
    // `System.getProperty`, not `Integer.getInteger`: the shared source links on Scala.js and Native too
    val p = System.getProperty("okay.shift.room")
    if (p != null) p.toInt else math.max(32L, StackSwitch.firstRoom.toLong * 1200 / 4096).toInt
  }
  private val left: ThreadLocal[Array[Int]] = new ThreadLocal[Array[Int]] { override def initialValue(): Array[Int] = Array(room) }

  /** run the machine for one `reset`, one level less of room; at zero on a fresh stack */
  private[okay2] def runReset[R, F <: Row](pushed: R ! (Shift[Any] + F)): R ! F = {
    val cell = left.get
    val here = cell(0)
    if (here > 0) {
      cell(0) = here - 1
      try machineFor[R, F](pushed) finally cell(0) = here
    } else StackSwitch.fresh { big =>
      val c = left.get
      val saved = c(0)
      c(0) = big / 2
      try machineFor[R, F](pushed) finally c(0) = saved
    }
  }

  // `run`'s `OneMachine` asks the row to be free of any Shift, which `Nesting` already decided at the door
  private def machineFor[R, F <: Row](pushed: R ! (Shift[Any] + F)): R ! F = run[R, F](pushed)(OneMachine.unchecked[F])

  // THE ONE CLAIM: a `Shift[R]` program is a `Shift[Any]` program at the same erasure (only the machine reads its
  // operations), and a capture of answer `R` reaches only the prompt of `R`'s key, where its `k` and body are
  // typed in that `reset`'s row.
  private[okay2] def in[A, R, F <: Row](q: A ! (Shift[R] + F)): A ! (Shift[Any] + F) = q.asInstanceOf[A ! (Shift[Any] + F)]
  private[okay2] def out[A, R, F <: Row](q: A ! (Shift[Any] + F)): A ! (Shift[R] + F) = q.asInstanceOf[A ! (Shift[R] + F)]
  private[okay2] def inner[R, F <: Row](q: R ! (Shift[Any] + F)): R ! F = q.asInstanceOf[R ! F]
  private[okay2] def clause[R, A, G <: Row, F <: Row](f: (A => R ! G) => R ! G): (A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F) =
    f.asInstanceOf[(A => R ! (Shift[Any] + F)) => R ! (Shift[Any] + F)]

  /** the test reads the prompt, so `Shift[Int] + Shift[String]` is a good row */
  implicit def typeableK[R](implicit k: Key[R]): TypeableK.ByValue[Shift[R]] = new TypeableK.ByValue[Shift[R]] {
    def test(x: scala.Any): Boolean = x match {
      case c: Shift.Capture[_, _] => c.prompt eq k.prompt
      case p: Shift.Push[_] => p.prompt eq k.prompt
      case d: Shift.Dollar[_, _] => d.prompt eq k.prompt
      case _ => false
    }
  }

  /**
   * The key of an answer type, made at compile time: two types, two keys; one type (through any alias, an
   * intersection in either order), one key, and one prompt for it. An abstract type has none: it is passed in,
   * as a `ClassTag` is.
   */
  final class Key[R] private (val id: String, private[okay2] val prompt: Prompt[R]) {
    override def toString: String = id
  }

  object Key {
    private val keys = new java.util.concurrent.ConcurrentHashMap[String, Key[scala.Any]]

    /** the key of `id`, one per id */
    def intern[R](id: String): Key[R] = {
      val k = keys.get(id)
      // one key per id, made at `Any` and read back at the type the id names
      (if (k != null) k else keys.computeIfAbsent(id, i => new Key[scala.Any](i, new Prompt[scala.Any]("reset", i)))).asInstanceOf[Key[R]]
    }

    implicit def of[R]: Key[R] = macro ShiftMacro.key[R]
  }

  /**
   * Whether a row still holds a capture's effect (a `Shift` or `Shift[Any]`): a `reset` over such a row pushes its
   * prompt on the machine an outer one runs. An abstract row reads as none, so code generic in the row and
   * nested in another `reset` passes its `Nesting` on.
   */
  final class Nesting[F <: Row] private[okay2] (val inner: Boolean)

  object Nesting extends PlainNesting {
    implicit def shifting[F <: Row](implicit ev: F <:< Shift.AnyKey): Nesting[F] = { val _ = ev; new Nesting[F](true) }
  }
  trait PlainNesting {
    implicit def plain[F <: Row]: Nesting[F] = new Nesting[F](false)
  }
}

object ShiftMacro {
  def key[R: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val r = weakTypeOf[R]
    if (r =:= typeOf[Any])
      c.abort(c.enclosingPosition, "`Any` is the dynamic key: `Shift[Any]` captures to a `Prompt` value, made by `Shift.prompt`, and has no key")
    // the members of an intersection, flattened by a worklist
    def parts(t: Type): List[Type] = {
      val out = List.newBuilder[Type]
      var todo = List(t)
      while (todo.nonEmpty) {
        val h = todo.head
        todo = todo.tail
        h.dealias match {
          case RefinedType(ps, _) => todo = ps ++ todo
          case other => out += other
        }
      }
      out.result()
    }
    // bounded by the type's own nesting, which the compiler has already walked
    def norm(t: Type): String = t.dealias match {
      case rt: RefinedType => parts(rt).map(norm).distinct.sorted.mkString("(", " with ", ")")
      case ConstantType(Constant(v)) => v.toString
      case TypeRef(_, sym, args) if sym.isClass =>
        sym.fullName + (if (args.isEmpty) "" else args.map(norm).mkString("[", ", ", "]"))
      case SingleType(_, sym) => sym.fullName + ".type"
      case other => c.abort(c.enclosingPosition,
        s"the answer type $r is abstract here ($other), so a reset or shift of it has no key; " +
          s"take a `Shift.Key[$other]` as a parameter where the type is known")
    }
    q"_root_.okay2.Shift.Key.intern[$r](${norm(r)})"
  }
}
