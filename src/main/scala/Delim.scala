package okay

import okay.!.*
import scala.annotation.{implicitNotFound, tailrec}
import scala.util.NotGiven

/**
 * Delimited control as an EFFECT — multi-prompt, in the shape of
 * Dybvig, Peyton Jones and Sabry's "A monadic framework for delimited
 * continuations" (2007): a prompt is a first-class tag carrying the
 * delimiter's answer type, `push` installs one, and `shift` captures
 * up to a NAMED prompt rather than to the nearest one.
 *
 * Two design points worth stating, because both were arrived at the
 * hard way.
 *
 * FIRST: `push` (reset) is an operation, not a handler application.
 * The reason is not symmetry with shift — it is that capturing across
 * an intervening delimiter is the whole point of multi-prompt, and
 * nested handlers cannot do it: an inner handler forwarding a shift
 * it does not own would forward it OPAQUELY, leaving its own frames
 * out of the captured continuation. One machine has to own the whole
 * prompt stack, so both push and shift must reach it as operations.
 *
 * SECOND: tags are what make several answer types coexist in ONE
 * effect row. A signature parameterised by its answer (`Control % R`)
 * would give two prompts of different answer types the same runtime
 * class, and union splitting here is by class — the two would be
 * indistinguishable. With the answer type riding inside the prompt,
 * there is a single `Delim` signature and the tags keep them apart.
 *
 * The price, stated plainly: the operations' payloads are programs in
 * the same row, which a single-parameter signature cannot express in
 * types, so they are erased here and re-typed inside the machine. The
 * smart constructors below are the only way to build these
 * operations, which makes the casts sealed module invariants — the
 * same discipline as Writer's phantom equation, and documented in the
 * same spirit rather than hidden.
 */

/**
 * A delimiter's identity AND its answer type; identity is the tag.
 *
 * `label` is what it is CALLED — "collect @ Walk.scala:12" — and it
 * exists so that the machine can say something a JVM stack trace
 * cannot (delim-diagnostics, 2026-09-17): which delimiters are
 * installed, and where each of them was written. One interned string
 * per prompt, nothing computed at run time.
 */
final class Prompt[R](val what: String, val where: String):
  /**
   * BUILT ON DEMAND, and the reason is a measured 21%: `Prompt` is
   * constructed in hot loops (DelimBenchmark's delimPushOnly makes a
   * thousand per operation), and interpolating the label at the
   * constructor cost 23.2 -> 28.2 us/op on that lane. The two pieces
   * are stored as they arrive and joined only when something asks —
   * which, apart from a test, means only the failure path.
   */
  def label: String = s"$what @ $where"
  override def toString: String = label

enum Delim[+A] derives Effect:
  /** install a delimiter and run the body under it (reset) */
  case Push[R](prompt: Prompt[R], body: Any) extends Delim[R]

  /**
   * Capture the continuation up to THIS prompt. The whole classic
   * family is two independent bits, so it is one operation with two
   * flags rather than four cases:
   *
   *   underPrompt — does f's body run with the delimiter still
   *                 installed? (shift, control: yes; the 0-variants
   *                 consume it)
   *   delimitK    — does invoking the captured continuation
   *                 re-install the delimiter? (shift, shift0: yes;
   *                 the control-variants hand back a bare segment)
   *
   *   reset(E[shift    f]) = reset (f (x => reset E[x]))
   *   reset(E[control  f]) = reset (f (x =>       E[x]))
   *   reset(E[shift0   f]) =        f (x => reset E[x])
   *   reset(E[control0 f]) =        f (x =>       E[x])
   */
  case Capture[R, A](prompt: Prompt[R], f: Any,
                     underPrompt: Boolean, delimitK: Boolean,
                     at: String) extends Delim[A]

  /**
   * λ$'s delimiter (Materzok & Biernacki, APLAS 2012): run the body
   * under the delimiter and, when it returns `x`, LEAVE the delimiter
   * and continue with `ret(x)`. The difference from `push` followed by
   * a `flatMap` is what a 0-capture takes: the delimiter TOGETHER WITH
   * `ret` (the `$/S0` rule), so a capture that drops its continuation
   * never runs `ret`, and one that resumes twice runs it twice. `push`
   * is this with `ret` the identity (specs/shift0-dollar.md).
   */
  case Dollar[R0, R](prompt: Prompt[R], ret: Any, body: Any) extends Delim[R]

  /** a `dollarResumed`: a `Dollar` that is told when the machine enters
   * it. Its own node so the plain one carries no count field
   * (delim-dollar-shots-bytes: +16 B per plain dollar when both shared
   * one shape with a null) */
  case Watched[R0, R](prompt: Prompt[R], ret: Any, body: Any, shots: Delim.Shots) extends Delim[R]

/**
 * A capture naming a prompt that is not on THIS machine's stack.
 *
 * The message is the documentation for the one hazard the type system
 * does not close, because this is the error people meet first and at
 * the worst time. It names the prompt that was wanted, the prompts
 * that are actually installed (innermost first, each with the line it
 * was installed at), and the rule that explains the difference.
 */
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

  /**
   * THE ROW HAS NO MACHINE YET (delim-safety stage 0, 2026-09-17).
   *
   * A machine owns one prompt stack, so starting a SECOND one inside
   * a row that already has `Delim` is the mistake that reads like
   * ordinary code and fails at run time: `resumable` around `collect`
   * is "a producer that pauses for an answer", and it used to compile
   * and throw `NoPrompt`. A row is a union, so the second `Delim` is
   * the SAME `Delim` by class, and the inner machine claims the outer
   * machine's operation.
   *
   * Every combinator that RUNS a machine asks for this; the ones that
   * only install a delimiter (`push`, `scope`, `collecting`,
   * `pausing`) do not, because installing into a Delim row is exactly
   * what they are for.
   *
   * WHAT IT DOES NOT CATCH, said here rather than discovered: an
   * ABSTRACT `F`. `NotGiven` reads "unknown" as "absent" (the caveat
   * `Failing` already records for its `In[Async, F]` probe), so a
   * generic helper taking `F[+_]` compiles and still throws when
   * instantiated at a Delim row. A guard against the shape people
   * write, not a proof: specs/delim-safety.md has the two stages that
   * would make it one.
   */
  @implicitNotFound("this row already contains Delim, so this would start a SECOND machine, and a capture cannot cross from one machine's prompt stack to another's.\nUse the nested form, which installs a delimiter on the machine already running:\n  delimited -> scope,   collect -> collecting,   resumable -> pausing\n(docs/continuations-in-practice.md, \"The second rule: one machine\")")
  final class OneMachine[F[+_]] private[Delim] ()
  object OneMachine:
    /**
     * Membership by APPLICATION, not by `Row.In` — measured, and
     * the reason is a compiler crash rather than taste. `NotGiven[
     * In[Delim, F]]` asks implicit search to prove membership in an
     * abstract row, `In.deeper` unfolds it into `G + H`, and dotty
     * 3.9 dies in `orDominator` with "Failure to join alternatives F
     * and G" — at Delim's OWN internal call sites, so the core did
     * not compile. A union on the RIGHT of a `<:<` needs no join
     * (subtyping INTO a union is the easy direction), and `<:<` is
     * covariant in its second parameter, so `refl` conforms.
     */
    given fresh[F[+_]](using NotGiven[Delim[Any] <:< F[Any]]): OneMachine[F] =
      new OneMachine[F]()

  /** a fresh delimiter tag, labelled with the line that asked for it */
  def prompt[R](using at: At): Prompt[R] = named[R]("prompt")

  /** the label every door builds: what made it, and where — as two
   * references, joined only if anybody asks (see `Prompt.label`) */
  private def named[R](what: String)(using at: At): Prompt[R] =
    new Prompt[R](what, at.where)

  /** run the body under the delimiter — reset, as an operation */
  def push[R, F[+_]](p: Prompt[R])(body: R ! Delim + F): R ! Delim + F =
    effect(Push(p, body))

  /**
   * `ret $ body` at the delimiter `p`: the body answers `R0`, the
   * delimiter answers `R`, and a `shift0` to `p` captures `ret` along
   * with the delimiter. `push(p)(body)` is `dollar(p)(pure)(body)`.
   * The under-prompt captures (`shift`, `control`) run their body
   * under a PLAIN delimiter, which is APLAS 2012's `S k.e = S0 k.⟨e⟩`
   * (TestDollarProbe). The control-variants need the bare segment,
   * which answers `R0` and not `R`, and are refused at a `dollar`
   * whose `R0` differs (see the machine).
   */
  def dollar[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! Delim + F)(body: R0 ! Delim + F): R ! Delim + F =
    effect(Dollar(p, ret, body))

  /**
   * How many times the machine has entered a watched `dollar` through
   * ONE capture: the count a `dollarResumed` hook is called with. A
   * capture that takes the delimiter gets a fresh count, so `n > 1`
   * means "this same captured context has been RUN a second time",
   * which is what a handler keeping its state in a cell must refuse
   * (`Lexical.tail`'s guard, lexical-tail-guard-abort). Counted when
   * the reified program is stepped, not when `k` builds it, so a
   * continuation built and dropped is not a resumption.
   */
  final class Shots(val resumed: Int => Unit):
    var n: Int = 0

  /**
   * `dollar`, told each time the machine enters it: `resumed(1)` at
   * the call itself and at the first run of each capture that took the
   * delimiter, `resumed(2)` at that capture's second run, and so on.
   * The one thing a `ret` cannot see: a resumption that leaves the body
   * by `abort` or a `shift0` outward never returns through `ret`, and
   * this hook fires before the body runs.
   */
  def dollarResumed[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! Delim + F, resumed: Int => Unit)
                                 (body: R0 ! Delim + F): R ! Delim + F =
    effect(Watched(p, ret, body, Shots(resumed)))

  /**
   * Capture the continuation up to `p` and hand it to `f`. The
   * continuation is a PROGRAM-valued function, so `f` may perform
   * effects around it, invoke it many times, or drop it entirely
   * (which is an early exit).
   *
   * `shift`: the body runs under the delimiter and the continuation
   * re-installs it — the variant most people mean, and the one that
   * lets a captured continuation shift again.
   */
  def shift[R, A, F[+_]](p: Prompt[R])
                        (f: (A => R ! Delim + F) => R ! Delim + F)(using at: At): A ! Delim + F =
    effect(Capture(p, f, underPrompt = true, delimitK = true, at = at.where))

  /** the body CONSUMES the delimiter (a further shift to `p` escapes
   * outward), the continuation still re-installs it */
  def shift0[R, A, F[+_]](p: Prompt[R])
                         (f: (A => R ! Delim + F) => R ! Delim + F)(using at: At): A ! Delim + F =
    effect(Capture(p, f, underPrompt = false, delimitK = true, at = at.where))

  /** the body runs under the delimiter, the continuation does NOT
   * re-install it — a bare segment, spliced where it is invoked */
  def control[R, A, F[+_]](p: Prompt[R])
                          (f: (A => R ! Delim + F) => R ! Delim + F)(using at: At): A ! Delim + F =
    effect(Capture(p, f, underPrompt = true, delimitK = false, at = at.where))

  /** neither: the delimiter is consumed and the continuation is bare */
  def control0[R, A, F[+_]](p: Prompt[R])
                           (f: (A => R ! Delim + F) => R ! Delim + F)(using at: At): A ! Delim + F =
    effect(Capture(p, f, underPrompt = false, delimitK = false, at = at.where))

  /** the common shape: a fresh prompt, a block under it, run */
  def reset[R, F[+_]](body: Prompt[R] => R ! Delim + F)
                     (using om: OneMachine[F], at: At): R ! F =
    val p = named[R]("reset")(using at)
    run(push(p)(body(p)))

  /**
   * THE DELIMITER IS INSTALLED — the evidence, and the typed door
   * (delim-prompted, 2026-09-16). `NoPrompt` is thrown when a capture
   * names a prompt that is not on the stack; a capture made through
   * THIS cannot, because the only way to hold a `Prompted[R]` is to be
   * inside the `delimited` that installed one. The constructor is
   * private to the package, so the evidence cannot be forged.
   *
   * It is the given, not the prompt, that has to be asked for: a
   * `Prompt[R]` is one line to make and proves nothing.
   *
   *     def banner: Prompted[Int] ?=> Int ! Delim + W = direct:
   *       "hello".tell
   *       1 + !Delim.shift[Int, Int, W](k => k(5))
   *
   * That function compiles, is a value, travels — and can only be
   * CALLED where a `delimited` put the evidence in scope.
   *
   * WHY NOT A ROW MEMBER. An obligation in the row (`A ! Delim +
   * Prompted[p.type] + F`, discharged by `push`) was written first
   * and does not compose: rows here are unions and `Free` is invariant
   * in them, so a body that does NOT capture to the prompt being
   * installed — `push(inner) { shift(outer)(…) }`, or any body with no
   * capture at all — cannot be widened into the row the handler wants.
   * Both shapes are ordinary and both are in `TestDelim`.
   *
   * WHAT IT DOES NOT CATCH: the evidence escaping its own `delimited`
   * and being used afterwards. That stays the runtime `NoPrompt`, and
   * closing it needs the region trick `runST` uses.
   */
  final class Prompted[R] private[Delim] (val prompt: Prompt[R]):
    /** the delimiter's answer type, as a MEMBER: a call site that
     * summons `Prompted[?]` can then name it without a parameter
     * (delim-one-type) */
    type Res = R

  /**
   * INSTALL A DELIMITER, AND NOTHING ELSE (delim-nesting, 2026-09-17):
   * a fresh prompt, the body under it with the evidence in scope, and
   * the machine left to whoever is running it.
   *
   * This is the half of `delimited` that NESTS. `delimited` finishes
   * by calling `run`, and one `run` is one machine that owns one
   * prompt stack — so a delimiter installed by an inner `run` cannot
   * be crossed by a capture from the outer one, and a capture from
   * inside an inner machine to an outer prompt is a runtime
   * `NoPrompt`. Every delimiter under ONE `run` is on one stack,
   * which is what makes crossing a delimiter — the whole point of
   * multi-prompt — work at all.
   *
   *     Delim.delimited[R, F]:          // the outermost one: runs
   *       direct:
   *         !Delim.scope[R2, F]:        // nested: installs only
   *           ...
   *
   * The rule, in one line: the OUTERMOST combinator runs the machine
   * (`delimited`, `collect`, `resumable`), everything under it
   * installs only (`scope`, `collecting`, `pausing`).
   */
  def scope[R, F[+_]](body: Prompted[R] ?=> R ! Delim + F)(using At): R ! Delim + F =
    scopeAs("scope")(body)

  /** the one place a delimiter is installed: `what` is the door's own
   * name and `at` the caller's line, and the two are joined ONCE.
   * (An earlier cut passed the name inside the `At` and produced
   * "scope @ delimited @ File:41" — caught by the label test.) */
  private def scopeAs[R, F[+_]](what: String)(body: Prompted[R] ?=> R ! Delim + F)
                               (using At): R ! Delim + F =
    val p = named[R](what)
    push(p)(body(using new Prompted[R](p)))

  /** install a fresh delimiter, run the body under it with the
   * evidence in scope, and handle the machine — the OUTERMOST form;
   * `scope` is the one that nests */
  def delimited[R, F[+_]](body: Prompted[R] ?=> R ! Delim + F)
                         (using om: OneMachine[F], at: At): R ! F =
    run(scopeAs("delimited")(body))(using om)

  /**
   * `scope` with a way out: `ret $ body` at a fresh delimiter, the
   * evidence in scope for the body (dollar-doors). The same word as
   * the prompt-taking primitive, told apart by the first clause: a
   * prompt there is the primitive, a return function here is this one
   * (the `shift` rule, delim-one-name). Installs only, like `scope`;
   * the machine is whoever runs the row.
   */
  def dollar[R0, R, F[+_]](ret: R0 => R ! Delim + F)(body: Prompted[R] ?=> R0 ! Delim + F)
                          (using at: At): R ! Delim + F =
    val p = named[R]("dollar")
    dollar[R0, R, F](p)(ret)(body(using new Prompted[R](p)))

  /** capture up to the delimiter in force — the same word as the
   * prompt-taking primitive, and the compiler picks by what you
   * write: a prompt in the first clause is the primitive, a handler
   * there is this one (delim-one-name) */
  def shift[R, A, F[+_]](using in: Prompted[R])
                          (f: (A => R ! Delim + F) => R ! Delim + F)(using At): A ! Delim + F =
    shift[R, A, F](in.prompt)(f)

  /**
   * The same capture, inside a `direct` block, with ONE type argument
   * (delim-one-type): `!Delim.shift[Int](k => k(5))` — the third
   * overload of the one name, told apart by how many type arguments
   * the call site writes.
   *
   * `A` is the only thing the call site knows that inference cannot —
   * a mark gives its argument no expected type, so without it `A`
   * falls to `Any` and the next operator refuses it. The other two are
   * already written down somewhere: the answer type is the evidence's
   * `Res`, and the ROW is the block's own, read off the `DirectCtx`
   * exactly as `Reader.ask` reads its environment (reader-env).
   *
   * `inline` for the same reason `Reader.ask` is: the `DirectCtx` that
   * pins the row is a value parameter of a lambda the macro strips, so
   * a reference to it must not survive into the output.
   */
  inline def shift[A](using in: Prompted[?])[F[_]]
                         (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                         (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    okay.effect[rw.R, A](Capture(in.prompt, f, underPrompt = true, delimitK = true,
      at = at.where).asInstanceOf[rw.R[A]])

  /** the 0-variant: the body consumes the delimiter */
  def shift0[R, A, F[+_]](using in: Prompted[R])
                           (f: (A => R ! Delim + F) => R ! Delim + F)(using At): A ! Delim + F =
    shift0[R, A, F](in.prompt)(f)

  /** the 0-variant inside a `direct` block, ONE type argument: the
   * mirror of the inline `shift` above, with the same reasons for
   * `inline` and for its cast (dollar-doors) */
  inline def shift0[A](using in: Prompted[?])[F[_]]
                          (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                          (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    okay.effect[rw.R, A](Capture(in.prompt, f, underPrompt = false, delimitK = true,
      at = at.where).asInstanceOf[rw.R[A]])

  /** the continuation does not re-install the delimiter */
  def control[R, A, F[+_]](using in: Prompted[R])
                            (f: (A => R ! Delim + F) => R ! Delim + F)(using At): A ! Delim + F =
    control[R, A, F](in.prompt)(f)

  /** neither */
  def control0[R, A, F[+_]](using in: Prompted[R])
                             (f: (A => R ! Delim + F) => R ! Delim + F)(using At): A ! Delim + F =
    control0[R, A, F](in.prompt)(f)

  /** abort to the delimiter in force with a value */
  def abort[R, A, F[+_]](using in: Prompted[R])(value: R)(using At): A ! Delim + F =
    abort[R, A, F](in.prompt)(value)

  // ==================================================================
  // THE PATTERNS (delim-patterns, 2026-09-17)
  //
  // A raw `shift` is a sharp tool and reads like one. These are the
  // four shapes that actually earn a capture in ordinary code, each
  // under a name that says what it DOES, so the reader needs no
  // theory to follow the call site. Every one is two or three lines
  // over `shift` — the value is the name and the evidence, not the
  // code.
  //
  // Each takes NO type arguments at the call site: the types are read
  // off the evidence (`in.Res`, `e.Elem`, `s.Qst`) and off the
  // block's `DirectCtx`, the trick delim-one-type introduced for
  // `shift[A]` and reader-env for `Reader.ask`.
  // ==================================================================

  /**
   * 1 · LEAVE EARLY WITH AN ANSWER.
   *
   * `!Delim.exit(value)` in a direct block stops there and makes
   * `value` the answer of the `delimited` around it. The rest of the
   * block does not run — a capture that DROPS its continuation is
   * what an early return is.
   *
   * What it replaces: an exception thrown for control flow (untyped,
   * and it walks past everything that was counting on an orderly
   * exit), a sentinel value threaded through every caller, or
   * rewriting two nested loops as a fold with a flag. This one leaves
   * from anywhere, including from inside a lambda, and the type says
   * what the answer is.
   *
   * The `for`-style spelling of the same thing is `Delim.abort`.
   */
  inline def exit(using in: Prompted[?])[F[_]]
                 (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                 (value: in.Res): Unit ! rw.R =
    shift[Unit](using in)(_ => okay.pure(value))

  /**
   * 2 · A PUSH API, READ AS A PULL.
   *
   * The evidence for a block that is collecting values. It carries
   * the element type as a MEMBER so `emit` can take no type argument
   * and still be precise about what it accepts.
   */
  sealed abstract class Emitting[A]:
    type Elem = A
    /** the prompt's answer: a list for `collect`, a state-passing
     * function for `collectUntil` (below) */
    type Res
    // NOT private: `emit` is inline and reaches this, and a private
    // member behind an inline body makes the compiler synthesize an
    // accessor with an unstable name (E192 — the same finding as
    // Cont.Shift's, measured here 2026-09-17)
    val in: Prompted[Res]
    /** what one emit does with the rest of the producer `k`, at the
     * row `X` the block runs in — the evidence decides, so a producer
     * written against `Emitting[A]` runs under either collect */
    def onEmit[X[+_]](a: A)(k: Unit => Res ! X): Res ! X

  /** `collect`'s evidence: the list is built on the way BACK, by the
   * continuation — `emit` conses after the rest of the producer has
   * answered, which is why the producer never has to know */
  private final class Listing[A](prompted: Prompted[List[A]]) extends Emitting[A]:
    type Res = List[A]
    val in: Prompted[List[A]] = prompted
    def onEmit[X[+_]](a: A)(k: Unit => List[A] ! X): List[A] ! X = k(()).map(a :: _)

  /**
   * `collectUntil`'s evidence (collect-early-stop): the state is
   * passed on the way DOWN through the prompt's answer, which is a
   * FUNCTION of it — `PState`'s trick over `Cont` (State.scala),
   * here over the prompt. An emit answers `s => …` at once; applying
   * it adds the element, and either ends with `fo.end` — the
   * continuation never called, the rest of the producer never run —
   * or resumes `k`, whose own answer is the next such function.
   * Nothing is mutated, so a multi-shot capture inside the producer
   * sees its own state, as `collect` sees its own list.
   *
   * `G` is the row the evidence was made at: `Delim + F` for the
   * `collectUntil[A, S, R, F]` that made it.
   */
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
    /**
     * THE ONE CAST this door needs, and why it is right: `emit` is
     * inline and captures at the row of the block it is written in
     * (`rw.R`, read off the block's `DirectCtx`), while the evidence
     * was made by `collectUntil[A, S, R, F]` at `Delim + F` — and the
     * body that emits IS that block, typed `Emitting[A] ?=> Unit !
     * (Delim + F)`. The two names denote one row; the type system sees
     * an existential behind `Emitting[?]` on one side and a member of
     * the block's evidence on the other, and cannot join them. Erasure
     * makes the coercion free; nothing about the program changes.
     */
    def atRow[R, X[+_], Y[+_]](p: R ! X): R ! Y = p.asInstanceOf[R ! Y]

  /**
   * Run `body`, which emits, and answer with everything it emitted,
   * in order.
   *
   * What it replaces: a `var buf = ListBuffer()` threaded through the
   * producer, or a callback parameter that turns the producer inside
   * out. The producer stays an ordinary recursive walk or loop; the
   * consumer gets a list. `emit` builds the list out of the rest of
   * the producer, which is why the producer never has to know.
   */
  def collect[A, F[+_]](body: Emitting[A] ?=> Unit ! Delim + F)
                       (using om: OneMachine[F], at: At): List[A] ! F =
    run(collectAs("collect")(body))(using om)

  /** the same collection, NESTED: it installs its delimiter and
   * leaves the machine to the `delimited`/`resumable` around it, so a
   * capture from inside — a `pause`, an `exit` to an outer scope —
   * crosses it instead of dying on a prompt another machine holds */
  def collecting[A, F[+_]](body: Emitting[A] ?=> Unit ! Delim + F)
                          (using At): List[A] ! Delim + F =
    collectAs("collecting")(body)

  private def collectAs[A, F[+_]](what: String)(body: Emitting[A] ?=> Unit ! Delim + F)
                                 (using At): List[A] ! Delim + F =
    scopeAs[List[A], F](what)(
      body(using new Listing[A](summon[Prompted[List[A]]]))
        .map(_ => List.empty[A]))

  /**
   * A COLLECT THAT STOPS (collect-early-stop; specs/fold-until.md is
   * the fold): run the SAME producer `collect` runs, fold what it
   * emits with `fo`, and stop the producer where `done` first holds —
   * `take(3)` over a tree walk runs the walk to its third leaf and no
   * further. The answer is `fo.end` of the state the emits built: the
   * prefix lives in the state, not in the continuation frames a stop
   * would drop, which is why `exit` inside a `collect` could never
   * answer with it. `done(init)` runs no body at all.
   */
  def collectUntil[A, S, R, F[+_]](using fo: FoldUntil[A, S, R])
                                   (body: Emitting[A] ?=> Unit ! Delim + F)
                                   (using om: OneMachine[F], at: At): R ! F =
    if fo.done(fo.init) then okay.pure(fo.end(fo.init))
    else run(collectUntilAs("collectUntil")(fo)(body))(using om)

  /** the nested half of `collectUntil`, as `collecting` is of `collect` */
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
        // the producer ended on its own: the state's own answer
        .map(_ => (s: S) => okay.pure[G, R](fo.end(s))))
      // the first emit's function, applied to the start; each
      // application resumes the producer up to the next emit
      .flatMap(f => f(fo.init))

  /** emit one value into the `collect` (or `collectUntil`) in force */
  inline def emit(using e: Emitting[?])[F[_]]
                 (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                 (a: e.Elem): Unit ! rw.R =
    shift[Unit](using e.in)(k => e.onEmit(a)(k))

  /**
   * 3 · STOP IN THE MIDDLE, CARRY ON LATER.
   *
   * What a paused program is: either it is asking, and the REST OF
   * IT is right there as `resume`, or it is finished. Queinnec's web
   * dialogue, as a type — and the same shape as an approval gate, a
   * wizard, a REPL, or any protocol that has to survive the gap
   * between one request and the next.
   *
   * `G` is the row the paused program still runs in, `Delim` and
   * all; `Dialogue` is the alias that spells it for a caller who only
   * knows their own row.
   */
  enum Paused[Q, A, R, G[+_]]:
    /** `at` is where the `pause` that made this was written
     * (delim-diagnostics): a dialogue that has not moved for a day is
     * a line of somebody's program, and saying which line is the
     * difference between an incident and a puzzle */
    case Ask[Q, A, R, G[+_]](question: Q, resume: A => Paused[Q, A, R, G] ! G,
                             at: String)
      extends Paused[Q, A, R, G]
    case Done[Q, A, R, G[+_]](value: R) extends Paused[Q, A, R, G]

  /** a paused program whose caller's row is `F` */
  type Dialogue[Q, A, R, F[+_]] = Paused[Q, A, R, Delim + F]

  object Paused:
    extension [Q, A, R, G[+_]](p: Paused[Q, A, R, G])
      /** the answer, if it has one */
      def finished: Option[R] = p match
        case Done(r) => Some(r)
        case _ => None
      /** the question it is waiting on, if it is waiting */
      def asking: Option[Q] = p match
        case Ask(q, _, _) => Some(q)
        case _ => None
      /** WHERE it is waiting — the `pause`'s own source position */
      def where: Option[String] = p match
        case Ask(_, _, at) => Some(at)
        case _ => None

  /** the evidence for a block that may pause, carrying the question
   * and answer types as members so `pause` needs no type argument */
  final class Asking[Q, A, R, G[+_]] private[Delim] (prompted: Prompted[Paused[Q, A, R, G]]):
    type Qst = Q
    type Ans = A
    type Fin = R
    type Row[+X] = G[X]
    /** public for the same reason as Emitting.in: `pause` is inline */
    val in: Prompted[Paused[Qst, Ans, Fin, Row]] = prompted

  /**
   * Run `body` until it pauses or finishes, and answer with WHICH.
   *
   * What it replaces: a state machine with a `step` column and a
   * hand-rolled record of everything the process knew so far. Here
   * the process is written as straight-line code and the record is
   * the continuation.
   *
   * A `Paused` lives in memory: it outlives a request, a retry, a
   * fork of the dialogue. To outlive a RESTART, keep the journal and
   * re-derive it — `replay`, below.
   */
  def resumable[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                               (using om: OneMachine[F], at: At): Dialogue[Q, A, R, F] ! F =
    run(pausingAs("resumable")(body))(using om)

  /** the same, NESTED: the dialogue's delimiter goes on the machine
   * already running, so a `resumable` may sit inside a `delimited`
   * and a capture may cross it */
  def pausing[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                             (using At): Dialogue[Q, A, R, F] ! Delim + F =
    pausingAs("pausing")(body)

  private def pausingAs[Q, A, R, F[+_]](what: String)
                                       (body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                                       (using At): Dialogue[Q, A, R, F] ! Delim + F =
    scopeAs[Dialogue[Q, A, R, F], F](what)(
      body(using new Asking[Q, A, R, Delim + F](summon[Prompted[Dialogue[Q, A, R, F]]]))
        .map(Paused.Done[Q, A, R, Delim + F](_)))

  /** ask, and hand the rest of the program back to the caller */
  inline def pause(using s: Asking[?, ?, ?, ?])[F[_]]
                  (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                  (q: s.Qst): s.Ans ! rw.R =
    shift[s.Ans](using s.in)(k => okay.pure(Paused.Ask(q,
      // THE ONE CAST here, and what makes it right: `s` can only be
      // held inside the `resumable` that installed this prompt, so
      // the block's row — read off its DirectCtx as `rw.R` — IS the
      // row `resumable` ran the body in. The type system cannot join
      // those two spellings of one row (the same reason the inline
      // `shift` above casts its own result), and nothing else can
      // produce an `Asking`.
      k.asInstanceOf[s.Ans => Paused[s.Qst, s.Ans, s.Fin, s.Row] ! s.Row],
      at.where)))

  /**
   * `pause`, OUTSIDE a direct block (delim-patterns-in-modules,
   * 2026-09-17): the `for`-comprehension spelling, and the only one a
   * natural transformation can use — which is where the need came
   * from, `okay-agent`'s stepper turning every `Tool.Call` into a
   * pause inside a `translate`.
   *
   * It takes the three types the inline door reads off its
   * `DirectCtx`, and it needs NO cast in exchange: with the row
   * written down, `k` already has the type `Ask.resume` wants. The
   * inline `pause` casts only because a mark gives its argument no
   * expected type.
   */
  def ask[Q, A, R, F[+_]](q: Q)(using s: Asking[Q, A, R, Delim + F], at: At)
                         : A ! Delim + F =
    shift[Paused[Q, A, R, Delim + F], A, F](using s.in)(k =>
      okay.pure(Paused.Ask(q, k, at.where)))

  /** answer every question until the dialogue is done — the driver
   * for the common case where the answers are available now */
  def drive[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F])(answer: Q => A ! F)
                           (using OneMachine[F]): R ! F =
    p match
      case Paused.Done(r) => okay.pure(r)
      case Paused.Ask(q, resume, _) =>
        answer(q).flatMap(a =>
          run(resume(a)).flatMap(drive[Q, A, R, F](_)(answer)))

  /**
   * ...AND OUTLIVE THE PROCESS (paused-persist, 2026-09-17).
   *
   * A continuation is a closure, and a closure cannot be written to
   * disk. So the thing that is kept is not the `Paused` — it is the
   * JOURNAL, the answers the dialogue has been given, in order. Where
   * it stands is then RE-DERIVED: run the program again from the top
   * and feed the recorded answers back without asking anyone.
   *
   * This is what durable workflow engines do (Temporal, Cadence,
   * Durable Functions), and it is exact under one discipline:
   *
   *     EVERYTHING THE OUTSIDE WORLD TELLS THE PROGRAM
   *     ENTERS THROUGH `pause`.
   *
   * Then the program is a pure function of its journal, and replay
   * cannot diverge from the original run. Break the discipline — read
   * a clock, call a service, roll a die anywhere but a `pause` — and
   * replay re-runs it. That is not a caveat this comment is asking you
   * to take on trust: `TestDelimPersist` has both, the program whose
   * Writer log DOUBLES on replay and the same program written to the
   * discipline, replaying exactly.
   *
   * What must be storable is `A` (and `Q`, if you show the questions
   * again) — ordinary data, not code.
   */
  type Journal[A] = List[A]

  /** answer the question a dialogue is asking, and keep the answer:
   * the pair is what you persist after every step */
  def answer[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F], j: Journal[A])(a: A)(using OneMachine[F])
                            : (Dialogue[Q, A, R, F], Journal[A]) ! F =
    p match
      case Paused.Ask(_, resume, _) => run(resume(a)).map(next => (next, j :+ a))
      case done => okay.pure((done, j))     // nobody asked; nothing to record

  /**
   * Where the dialogue stands, from its program and its journal —
   * what replaces persisting a continuation. A fresh process, a
   * different machine, a redeploy: same answers in, same place out.
   */
  def replay[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! Delim + F)
                            (using OneMachine[F], Replayable[Delim + F], At)
                            (j: Journal[A]): Dialogue[Q, A, R, F] ! F =
    j.foldLeft(resumable[Q, A, R, F](body)): (acc, a) =>
      acc.flatMap:
        case Paused.Ask(_, resume, _) => run(resume(a))
        case done => okay.pure(done)        // more answers than questions

  /**
   * 4 · DO SOMETHING ON THE WAY BACK.
   *
   * `!Delim.onReturn(f)` runs the rest of the block and then puts its
   * answer through `f`. The rest of the block is a value here, which
   * is the whole point: you can measure it, log what it produced,
   * undo it, or fold a compensation into its answer — from a place in
   * the middle, without the code around it being restructured.
   *
   * What it replaces: wrapping the remainder in a function and
   * passing it down, or a `finally` that cannot see the answer.
   */
  inline def onReturn(using in: Prompted[?])[F[_]]
                     (using inline ctx: DirectCtx[F])(using rw: Reader.RowOf[F], at: At)
                     (f: in.Res => in.Res): Unit ! rw.R =
    shift[Unit](using in)(k => k(()).map(f))

  /** a prompt is its own typed token: the same prompt has the same
   * answer type — the witness the machine uses to split its stack */
  given Same[Prompt] = Same.byIdentity

  /**
   * The machine's continuation, TYPED: a chain from the current
   * answer A to the run's answer Z. `K` is one Bind's continuation
   * (types chain through it), `Mark` a delimiter carrying its
   * prompt — and so the answer type of the program under it.
   */
  private sealed trait Segs[F[+_], A, Z]

  /**
   * A frame with a successor, and so the place a COPY's successor
   * goes. `rest` is a `var` for one writer only: `split`'s copy loop,
   * which builds a captured prefix front to back and sets each copy's
   * `rest` exactly once, while the copy is still unpublished
   * (delim-split-wrap-free). Every other frame is built whole and never
   * written again, so the chain is as immutable as it was: a captured
   * continuation may be re-run as often as it likes.
   */
  private sealed abstract class Hole[F[+_], Y, Z](var rest: Segs[F, Y, Z])

  private object Segs:
    final case class Done[F[+_], Z]() extends Segs[F, Z, Z]
    final class K[F[+_], X, Y, Z](val f: X => Y ! Delim + F, rest0: Segs[F, Y, Z])
      extends Hole[F, Y, Z](rest0), Segs[F, X, Z]
    final class Mark[F[+_], X, Z](val p: Prompt[X], rest0: Segs[F, X, Z])
      extends Hole[F, X, Z](rest0), Segs[F, X, Z]
    /** a `dollar` delimiter: the body's X0 leaves through `ret` into
     * the prompt's X */
    final class Ret[F[+_], X0, X, Z](val p: Prompt[X], val ret: X0 => X ! Delim + F, rest0: Segs[F, X, Z])
      extends Hole[F, X, Z](rest0), Segs[F, X0, Z]
    /** a watched `dollar` (`dollarResumed`): a `Ret` with its count.
     * Matched LAST wherever the chain is walked, so the plain frames
     * pay no type test for it */
    final class Watch[F[+_], X0, X, Z](val p: Prompt[X], val ret: X0 => X ! Delim + F, val shots: Shots, rest0: Segs[F, X, Z])
      extends Hole[F, X, Z](rest0), Segs[F, X0, Z]
    /** the first hole of a copy: where its head goes, one per capture */
    final class Head[F[+_], A, E] extends Hole[F, A, E](null)

  /** the copy of a `Watch` a capture takes with it: a FRESH count, one
   * per capture, so the count is "runs of this captured context"
   * (`Shots`). A plain `Ret` is copied as it is: nothing in it is
   * per-capture */
  private def retake[F[+_], X0, X, Q](r: Segs.Watch[F, X0, X, ?], c: Segs[F, X, Q]): Segs.Watch[F, X0, X, Q] =
    Segs.Watch(r.p, r.ret, Shots(r.shots.resumed), c)

  /**
   * The stack cut at a prompt: what was captured and what lies outside
   * it. TWO SHAPES, because a `dollar` delimiter changes the answer
   * type and a plain one does not, and the plain one is the hot path.
   * An earlier cut had one shape for both, which carried the delimiter
   * as a frame and its plainness as evidence. It cost 104 B and 7-9%
   * per capture on delimGenerator, which never meets a dollar
   * (specs/shift0-dollar.md, Results).
   */
  private sealed trait Cut[F[+_], A, P, Z]

  /** no mark of the prompt on this stack: allocated only on the path
   * that forwards or throws `NoPrompt`, so a found cut costs no
   * `Option` per frame */
  private final case class NotFound[F[+_], A, P, Z]() extends Cut[F, A, P, Z]


  /** a plain mark: the chain up to it, answering the prompt's P */
  private final case class Plain[F[+_], A, P, Z](captured: Segs[F, A, P], outer: Segs[F, P, Z])
    extends Cut[F, A, P, Z]

  /** a `dollar`: the chain answers the body's `P0`, and `close` (the
   * `Ret` with its function) leads to `P`. A delimited continuation is
   * `captured` then `close`, so `ret` goes with it (`$/S0`). `P0` is a
   * type MEMBER so that `captured` and `close` stay linked through one
   * stable value, with no cast. */
  private sealed abstract class AtRet[F[+_], A, P, Z] extends Cut[F, A, P, Z]:
    type P0
    val captured: Segs[F, A, P0]
    val close: Segs[F, P0, P]
    val outer: Segs[F, P, Z]

  private object AtRet:
    def apply[F[+_], A, Q, P, Z](c: Segs[F, A, Q], cl: Segs[F, Q, P], o: Segs[F, P, Z]): AtRet[F, A, P, Z] =
      new AtRet[F, A, P, Z]:
        type P0 = Q
        val captured = c
        val close = cl
        val outer = o

  /** the machine's state between steps: a program and the stack it
   * continues into */
  private final case class Next[F[+_], A, Z](prog: A ! Delim + F, kont: Segs[F, A, Z])

  /**
   * The machine. Our Bind nodes already reify continuations as plain
   * functions, so the freer tree IS the control stack — the machine
   * only has to keep the segment chain and the prompt markers in it.
   * A captured segment is turned back INTO A PROGRAM (reify below),
   * which is why the continuation is an ordinary value: multi-shot
   * comes for free, and nothing is a closure over interpreter state.
   *
   * Two claims and no other cast (cast-free-delim): a Push's body
   * and a Capture's f are programs in this machine's row, erased at
   * the operation because the row's other half F is not the
   * operation's to name — re-typed here, at their two lines, where F
   * is known. Everything else the chain's types carry.
   */
  def run[R, F[+_]](prog: R ! Delim + F)(using OneMachine[F]): R ! F =
    machine(prog, forward = false)

  /**
   * THE MACHINE THAT FORWARDS INSTEAD OF THROWING (delim-forward,
   * 2026-09-17) — for a row that still has a `Delim` in it, i.e. one
   * running inside another machine.
   *
   * When a capture names a prompt this machine does not hold, `run`
   * throws `NoPrompt`. It has another option, and the machinery for
   * it is already here: re-emit the operation into the residual
   * program and resume THIS machine with the same stack when the
   * answer arrives — which is exactly what the foreign-operation path
   * does for any effect it does not own. The outer machine, which
   * does hold the prompt, then captures across this machine's frames.
   *
   * The evidence is what makes it well-typed: forwarding puts a
   * `Delim` operation into `F`, so `F` must have one. That is the
   * case this exists for, and asking for it keeps `run`'s meaning
   * unchanged — a caller who wants forwarding says so.
   */
  def runNested[R, F[+_]](prog: R ! Delim + F)(using Row.In[Delim, F]): R ! F =
    machine(prog, forward = true)

  private def machine[R, F[+_]](prog: R ! Delim + F, forward: Boolean): R ! F = {
    type Row = Delim + F
    type Prog[A] = A ! Row

    /** frames back into a program: binds become flatMaps, markers
     * become pushes — the continuation re-installs its delimiter */
    @tailrec def reify[A, P](segs: Segs[F, A, P], start: Prog[A]): Prog[P] = segs match
      case Segs.Done() => start
      case k: Segs.K[F, A, y, P] => reify(k.rest, start.flatMap(k.f))
      case m: Segs.Mark[F, A, P] => reify(m.rest, effect[Row, A](Push(m.p, start)))
      case r: Segs.Ret[F, A, x, P] => reify(r.rest, effect[Row, x](Dollar[A, x](r.p, r.ret, start)))
      case r: Segs.Watch[F, A, x, P] => reify(r.rest, effect[Row, x](Watched[A, x](r.p, r.ret, start, r.shots)))

    /** the delimiters this machine has installed, innermost first —
     * what `NoPrompt` prints instead of saying nothing
     * (delim-diagnostics). It walks the same chain `split` does, and
     * only ever runs on the failing path. */
    def installed[A, Z](kont: Segs[F, A, Z]): List[String] =
      @tailrec def go[X](k: Segs[F, X, Z], acc: List[String]): List[String] = k match
        case Segs.Done() => acc.reverse
        case m: Segs.Mark[F, X, Z] => go(m.rest, m.p.label :: acc)
        case r: Segs.Ret[F, X, x, Z] => go(r.rest, r.p.label :: acc)
        case c: Segs.K[F, X, y, Z] => go(c.rest, acc)
        case r: Segs.Watch[F, X, x, Z] => go(r.rest, r.p.label :: acc)
      go(kont, Nil)

    /** a delimiter's frame IS itself: identity makes a chain's input
     * type the found frame's, the axiom prompts use (`Same`) */
    val sameSeg: Same[[X] =>> Segs[F, X, R]] = Same.byIdentity[[X] =>> Segs[F, X, R]]

    /** cut the chain at the delimiter of p: the delimiter's prompt IS
     * p by identity, and Same's witness makes its type P's.
     *
     * LOOPS, not recursion (specs/stack-safety.md): a shift under
     * 20 000 other delimiters overflowed the recursive version, which
     * rebuilt the prefix on the way back up. TWO of them
     * (delim-split-wrap-free): `find` walks to the delimiting frame and
     * allocates nothing; `copyTo` copies the prefix FRONT TO BACK, each
     * copy's `rest` set once into the hole the previous copy left. The
     * first loop version pushed a frame closure and a node per segment
     * onto a type-aligned stack and unwound it, +32 B per capture on
     * delimGenerator. */
    def split[A, P](kont: Segs[F, A, R], p: Prompt[P]): Cut[F, A, P, R] =
      find(kont, p) match
        case m: Segs.Mark[F, x, R] =>
          (m.p === p) match
            case Some(ev) =>
              Plain(ev.liftCo[[t] =>> Segs[F, A, t]](copyTo(kont, m)), ev.liftCo[[t] =>> Segs[F, t, R]](m.rest))
            case None => NotFound()
        case r: Segs.Ret[F, x0, x, R] =>
          (r.p === p) match
            case Some(ev) =>
              AtRet[F, A, x0, P, R](copyTo(kont, r),
                ev.liftCo[[t] =>> Segs[F, x0, t]](Segs.Ret(r.p, r.ret, Segs.Done[F, x]())),
                ev.liftCo[[t] =>> Segs[F, t, R]](r.rest))
            case None => NotFound()
        case r: Segs.Watch[F, x0, x, R] =>
          (r.p === p) match
            case Some(ev) =>
              AtRet[F, A, x0, P, R](copyTo(kont, r),
                ev.liftCo[[t] =>> Segs[F, x0, t]](retake(r, Segs.Done[F, x]())),
                ev.liftCo[[t] =>> Segs[F, t, R]](r.rest))
            case None => NotFound()
        case _ => NotFound()

    /** the frame that delimits p, or null: prompts compared by
     * identity, so the walk allocates nothing */
    @tailrec def find[X](kont: Segs[F, X, R], p: Prompt[?]): Segs[F, ?, R] | Null = kont match
      case k: Segs.K[F, X, y, R] => find(k.rest, p)
      case m: Segs.Mark[F, X, R] => if m.p eq p then m else find(m.rest, p)
      case r: Segs.Ret[F, X, x, R] => if r.p eq p then r else find(r.rest, p)
      case Segs.Done() => null
      case r: Segs.Watch[F, X, x, R] => if r.p eq p then r else find(r.rest, p)

    /** the chain from `kont` up to `stop` (a frame on it), copied */
    def copyTo[A, E](kont: Segs[F, A, R], stop: Segs[F, E, R]): Segs[F, A, E] =
      val head = Segs.Head[F, A, E]()
      fill(kont, head, stop)
      head.rest

    @tailrec def fill[X, E](cur: Segs[F, X, R], hole: Hole[F, X, E], stop: Segs[F, E, R]): Unit =
      if cur eq stop then
        sameSeg.same(cur, stop) match
          case Some(ev) => hole.rest = ev.flip.liftCo[[t] =>> Segs[F, t, E]](Segs.Done[F, E]())
          case None => ()
      else cur match
        case k: Segs.K[F, X, y, R] =>
          val c = Segs.K[F, X, y, E](k.f, null)
          hole.rest = c
          fill(k.rest, c, stop)
        case m: Segs.Mark[F, X, R] =>
          val c = Segs.Mark[F, X, E](m.p, null)
          hole.rest = c
          fill(m.rest, c, stop)
        case r: Segs.Ret[F, X, x, R] =>
          val c = Segs.Ret[F, X, x, E](r.p, r.ret, null)
          hole.rest = c
          fill(r.rest, c, stop)
        // not reached: `stop` is on the chain
        case Segs.Done() => ()
        case r: Segs.Watch[F, X, x, R] =>
          val c = retake[F, X, x, E](r, null)
          hole.rest = c
          fill(r.rest, c, stop)

    // ONE tail-recursive loop: an earlier version split it into
    // loop/onOp, and mutual recursion is not tail-optimised, so every
    // operation cost frames and a thousand nested captures blew the
    // stack. Merged, only a FOREIGN operation suspends (under a
    // flatMap closure, as State.handle does) and the Delim ops
    // themselves are flat.
    @tailrec def loop(state: Next[F, ?, R]): R ! F = state match
      case n: Next[F, a, R] => (n.prog.resume: @unchecked) match
        case Return(x) => n.kont match
          case Segs.Done() => okay.pure(x)
          case k: Segs.K[F, a, ?, R] => loop(Next(k.f(x), k.rest))
          // the delimited block finished normally: drop its marker
          case m: Segs.Mark[F, a, R] => loop(Next(okay.pure(x), m.rest))
          // a `dollar` finished normally: leave it, through its return
          case r: Segs.Ret[F, a, ?, R] => loop(Next(r.ret(x), r.rest))
          case r: Segs.Watch[F, a, ?, R] => loop(Next(r.ret(x), r.rest))

        case Inject(e) => step(e, n.kont) match
          case Left(answer) => answer
          case Right(next) => loop(next)

        case Bind(Inject(e), k) =>
          step(e, Segs.K(k, n.kont)) match
            case Left(answer) => answer
            case Right(next) => loop(next)

    /** one operation: either the machine is done (Left) or it
     * continues with a new program and stack (Right) */
    def step[X](e: Row[X], kont: Segs[F, X, R]): Either[R ! F, Next[F, ?, R]] =
      // `okay.split`, not this object's own `split` (the segment stack)
      okay.split[Delim, F](e) { c => c match
          case pu: Push[r] =>
            // claim 1: the pushed body answers the prompt's r in this
            // row; r is an X (the op's answer), which K carries up
            val body = pu.body.asInstanceOf[Prog[r]]
            Right(Next(body, Segs.Mark(pu.prompt, Segs.K((a: r) => okay.pure[Row, X](a), kont))))

          case cap: Capture[p, a] =>
            // `p` in a type pattern would BIND a new variable; the alias
            // makes a pattern name the capture's own prompt type
            type Pr = p
            // claim 2: f takes a continuation into the prompt's answer and
            // gives back a program at it, in this row; shift/control put the
            // body back under the delimiter, the 0-variants have consumed it
            def resume(k: a => Prog[p], outer: Segs[F, p, R]): Either[R ! F, Next[F, ?, R]] =
              val body = cap.f.asInstanceOf[(a => Prog[p]) => Prog[p]](k)
              if cap.underPrompt then Right(Next(effect[Row, p](Push(cap.prompt, body)), outer))
              else Right(Next(body, outer))
            // shift/shift0 re-install the delimiter (a dollar's with its
            // return function, `$/S0`); control/control0 hand back the bare
            // segment, which answers the prompt's type only at a plain mark
            split(kont, cap.prompt) match
              case Plain(captured, outer) =>
                resume((v: a) => {
                  val seg = reify(captured, okay.pure[Row, X](v))
                  if cap.delimitK then effect[Row, p](Push(cap.prompt, seg)) else seg
                }, outer)
              case r: AtRet[F, X, Pr, R] =>
                if cap.delimitK then resume((v: a) => reify(r.close, reify(r.captured, okay.pure[Row, X](v))), r.outer)
                else throw new UnsupportedOperationException(
                  s"${cap.at}: a control-capture to ${cap.prompt.label}, which is a `dollar`: its bare continuation answers the body's type, not the prompt's (specs/shift0-dollar.md)")
              case NotFound() =>
                if forward then
                  // THE ONE CAST forwarding needs, and what makes it
                  // right: `runNested` asked for `In[Delim, F]`, so an
                  // operation of this machine's own signature IS an
                  // operation of the residual row. The shape below is
                  // the foreign-operation path, verbatim: re-emit, and
                  // resume this machine with the same stack. `kont` is
                  // immutable, so a multi-shot outer capture may
                  // re-enter it as often as it likes.
                  Left(Inject(c.asInstanceOf[F[X]])
                    .flatMap(x => loop(Next(okay.pure[Row, X](x), kont))))
                else throw NoPrompt(cap.at, cap.prompt.label, installed(kont))

          // after Capture, so a capture pays no extra type test
          case d: Dollar[r0, r] =>
            // claims 1b and 1c, the same as Push's: the body answers r0
            // and ret leads from r0 to the prompt's r, in this row
            val body = d.body.asInstanceOf[Prog[r0]]
            val ret = d.ret.asInstanceOf[r0 => Prog[r]]
            Right(Next(body, Segs.Ret(d.prompt, ret, Segs.K((a: r) => okay.pure[Row, X](a), kont))))

          // last: only `dollarResumed` makes one
          case d: Watched[r0, r] =>
            // the same two claims as Dollar's, for the same reason
            val body = d.body.asInstanceOf[Prog[r0]]
            val ret = d.ret.asInstanceOf[r0 => Prog[r]]
            // told it is being entered — here, when the program is RUN,
            // so a continuation built and dropped does not count
            // (lexical-tail-guard-abort)
            val shots = d.shots
            shots.n += 1
            shots.resumed(shots.n)
            Right(Next(body, Segs.Watch(d.prompt, ret, shots, Segs.K((a: r) => okay.pure[Row, X](a), kont))))
        }
        // a foreign operation suspends the machine: the residual
        // program performs it and resumes with the same stack
        (g => Left(Inject(g).flatMap(x => loop(Next(okay.pure(x), kont)))))

    loop(Next(prog, Segs.Done()))
  }

  /** abort to a prompt with a value: a shift that drops the
   * continuation (the 0-variant, so the delimiter goes with it) */
  def abort[R, A, F[+_]](p: Prompt[R])(value: R)(using At): A ! Delim + F =
    shift0[R, A, F](p)(_ => okay.pure(value))

  // ==================================================================
  // THE PROMPT STACK IN THE TYPE (freer-base stage 2, 2026-09-23)
  //
  // `NoPrompt` — a capture naming a prompt that is not installed — is
  // a run-time exception on every door above. Here it is a compile
  // error: the stack of installed prompts is carried as a lexical
  // GIVEN (`Stack`), `reset` pushes the prompt it makes onto it for
  // its body only, and `shift` asks for evidence (`Has`) that its
  // prompt is on the stack in force. The three shapes that throw
  // today are refused by the compiler: a shift with no reset, a shift
  // to a foreign prompt of the same answer type, and a prompt that
  // ESCAPED its reset and is shifted to afterwards — the last because
  // after the reset returns the stack in force is the OUTER one,
  // which has no `p.type` in it. No region system, no tag on the
  // program's type: the probe (scripts/stage2-prompt-identity-probe.
  // scala, 4af08745) found this shape and paid for the four that do
  // not work — the stack must be a given (a for-comprehension head
  // has no expected type), not a curried dependent context function
  // (refused), not carried through a non-curried one (crashes
  // dotty), and the using clause goes BEFORE the continuation.
  //
  //     Delim.Stacked.delimited[Int, okay.Pure] { s =>
  //       import s.given
  //       shift[Int, Int, okay.Pure](s.p)(k => k(5).map(_ * 2))   // 10
  //     }
  //
  // One `import s.given` per reset is the whole cost at a call site;
  // the type arguments on `shift` are what TestDelim writes today.
  // ADDITIVE: `reset`/`shift` above and every caller keep their
  // spelling (specs/freer-base.md, Decisions).
  object Stacked:
    import scala.annotation.unused

    /** the stack in force, as a lexical given. A type MEMBER, so no
     * call site spells it and no method has a stack type parameter
     * that inference could pin to `Tuple` too early. */
    final class Stack[S0 <: Tuple]:
      type S = S0

    /** what `reset` hands its body: the prompt, and the stack that
     * installing it made — `import s.given` puts it in force. OPEN so
     * that an installation elsewhere can hand its body a richer value
     * that IS the delimiter on the stack (`Lexical.Stacked.SInst`),
     * with no second singleton to relate to this one. */
    class In[R, S <: Tuple](val p: Prompt[R]):
      given stack: Stack[p.type *: S] = new Stack[p.type *: S]

    /** "p is a PLAIN delimiter" — what `control` asks for, since a
     * control-capture's bare segment answers the body's type, which at
     * a `dollar` is not the prompt's (specs/shift0-dollar.md). Only a
     * `Reset` puts it in scope: a `dollar`, a layer or a Lexical
     * instance hands a bare `In`, and a `control` to it is refused
     * here instead of by the machine (dollar-doors). */
    @implicitNotFound("prompt ${P} is a `dollar` (or a layer, or a handler instance), not a plain reset: a control-capture's bare continuation answers the body's type, not the prompt's — use shift/shift0/abort, or a Delim.Stacked.reset (specs/shift0-dollar.md)")
    final class Plain[P]

    /** what `reset` and `delimited` hand their body: a plain delimiter,
     * which every capture may name */
    final class Reset[R, S <: Tuple](p0: Prompt[R]) extends In[R, S](p0):
      given plain: Plain[p.type] = new Plain[p.type]

    /** "p is on the stack" — the using clause that replaces the throw */
    @implicitNotFound("prompt ${P} is not on the prompt stack ${S}: a shift names the prompt of a reset it is INSIDE (Delim.Stacked.reset { s => import s.given; … shift(s.p) … }) — not one that has returned, and not one another reset made")
    sealed trait Has[S <: Tuple, P]:
      /** the stack BELOW p: what a capture to p leaves in force, since
       * it takes p and every delimiter installed inside it. Found by
       * the same induction that finds p, because a match type cannot
       * compute it: two prompts' singleton types are not provably
       * disjoint, so `Below[q.type *: S, p.type]` would never reduce. */
      type Below <: Tuple
    object Has:
      type Aux[S <: Tuple, P, B <: Tuple] = Has[S, P] { type Below = B }
      given here[P, S <: Tuple]: Aux[P *: S, P, S] = new Has[P *: S, P] { type Below = S }
      given there[P, Q, S <: Tuple, B <: Tuple](using Aux[S, P, B]): Aux[Q *: S, P, B] =
        new Has[Q *: S, P] { type Below = B }

    /** a program under the stack `S`, leaving it as it found it —
     * every capture here is balanced */
    type Under[F[+_], A, S <: Tuple] = Prog[Delim + F, A, S, S]

    /** any ordinary program, under the stack in force: the diagonal,
     * with the index read from the given rather than from an expected
     * type (which a for-comprehension head does not have) */
    inline def under[F[+_], A](p: A ! Delim + F)(using st: Stack[?]): Under[F, A, st.S] =
      Prog.diag[st.S, Delim + F, A](p)

    /**
     * The root: a fresh prompt on an EMPTY stack, the body under it,
     * the machine run — `Delim.delimited`'s job, with the stack in the
     * type. Every stacked program starts here; `reset` below installs
     * only, and needs a stack to install on.
     */
    def delimited[R, F[+_]](body: (s: Reset[R, EmptyTuple]) => Under[F, R, s.p.type *: EmptyTuple])
                           (using om: OneMachine[F], at: At): R ! F =
      val s = new Reset[R, EmptyTuple](named[R]("delimited")(using at))
      run[R, F](push[R, F](s.p)(body(s).free))(using om)

    /**
     * A fresh prompt pushed on the stack in force, for the body only:
     * the nested delimiter. After it returns the stack in force is the
     * one it was called under — which is what refuses a shift to its
     * prompt from outside.
     */
    def reset[R, F[+_]](using st: Stack[?])
                       (body: (s: Reset[R, st.S]) => Under[F, R, s.p.type *: st.S])
                       (using at: At): Under[F, R, st.S] =
      val s = new Reset[R, st.S](named[R]("reset")(using at))
      Prog.diag[st.S, Delim + F, R](push[R, F](s.p)(body(s).free))

    /**
     * Capture up to `p` — REQUIRES `p` on the stack in force. The body
     * runs under `p` and NOTHING installed inside it: the capture took
     * those along with the continuation. So the body gets its own stack,
     * `p *: B` with `B` the stack below `p`, as a given, and so does
     * `k`. The first cut of this door typed the body under the whole
     * stack, which let a shift from the body to a CAPTURED inner prompt
     * compile and throw `NoPrompt` (stacked-shift0, ProbeStackedHole).
     */
    def shift[R, A, F[+_]](p: Prompt[R])(using st: Stack[?])[B <: Tuple](using @unused ev: Has.Aux[st.S, p.type, B])
                          (f: Stack[p.type *: B] ?=> (A => Under[F, R, p.type *: B]) => Under[F, R, p.type *: B])
                          (using at: At): Under[F, A, st.S] =
      given Stack[p.type *: B] = new Stack[p.type *: B]
      Prog.diag[st.S, Delim + F, A](
        Delim.shift[R, A, F](p)(k => f(a => Prog.diag[p.type *: B, Delim + F, R](k(a))).free)(using at))

    /** `Delim.control`, stacked: the continuation is a bare segment,
     * spliced where `f` invokes it — inside `f`, under `p` and what is
     * below it, the same stack as `shift`'s body. Only to a PLAIN
     * reset (`Plain`): at a `dollar` the bare segment answers the
     * body's type, which the machine used to refuse at run time. */
    def control[R, A, F[+_]](p: Prompt[R])(using st: Stack[?])[B <: Tuple](using @unused ev: Has.Aux[st.S, p.type, B], @unused pl: Plain[p.type])
                            (f: Stack[p.type *: B] ?=> (A => Under[F, R, p.type *: B]) => Under[F, R, p.type *: B])
                            (using at: At): Under[F, A, st.S] =
      given Stack[p.type *: B] = new Stack[p.type *: B]
      Prog.diag[st.S, Delim + F, A](
        Delim.control[R, A, F](p)(k => f(a => Prog.diag[p.type *: B, Delim + F, R](k(a))).free)(using at))

    /**
     * `Delim.shift0`, stacked: the body runs with `p` CONSUMED, under the
     * stack below it. That is ICFP 2011's rule for S0 (Materzok &
     * Biernacki: the body typed under the context stack with the top
     * removed), with "the top" meaning everything down to and including
     * the named prompt. A shift to `p` from the body is refused, and a
     * shift to a prompt below `p` resolves. `k` re-installs `p` and the
     * captured delimiters, so it runs under the same `B`.
     */
    def shift0[R, A, F[+_]](p: Prompt[R])(using st: Stack[?])[B <: Tuple](using @unused ev: Has.Aux[st.S, p.type, B])
                           (f: Stack[B] ?=> (A => Under[F, R, B]) => Under[F, R, B])
                           (using at: At): Under[F, A, st.S] =
      given Stack[B] = new Stack[B]
      Prog.diag[st.S, Delim + F, A](
        Delim.shift0[R, A, F](p)(k => f(a => Prog.diag[B, Delim + F, R](k(a))).free)(using at))

    /** drop the continuation and answer `value` at `p` */
    def abort[R, A, F[+_]](p: Prompt[R])(using st: Stack[?], @unused ev: Has[st.S, p.type])
                          (value: R)(using at: At): Under[F, A, st.S] =
      Prog.diag[st.S, Delim + F, A](Delim.abort[R, A, F](p)(value)(using at))

    /**
     * `Delim.dollar`, stacked: a fresh prompt on the stack in force for
     * the body, and `ret` run OUTSIDE it, under the stack the dollar was
     * called under. A `shift0` to its prompt takes `ret` along.
     */
    def dollar[R0, R, F[+_]](using st: Stack[?])(ret: R0 => Under[F, R, st.S])
                            (body: (s: In[R, st.S]) => Under[F, R0, s.p.type *: st.S])
                            (using at: At): Under[F, R, st.S] =
      val s = new In[R, st.S](named[R]("dollar")(using at))
      Prog.diag[st.S, Delim + F, R](Delim.dollar[R0, R, F](s.p)(r0 => ret(r0).free)(body(s).free))

    // `control0` is NOT here, deliberately: its continuation is a bare
    // segment run where `p` is gone, but the code inside that segment was
    // typed with `p` on its stack, so a capture to `p` in it would pass
    // the index and throw. The index cannot say "this k needs p"; the
    // unstacked door remains (specs/shift0-dollar.md, Decisions). Were it
    // added, it would ask for `Plain[p.type]` as `control` does.
}

/** The class IS the whole identity: Delim has no parameter but its
 * (erased) answer type, so splitting a row on it is a TOTAL test —
 * said once here, rather than as a "cannot be checked at runtime"
 * warning at every use site of a test that is in fact complete. */

/**
 * WHERE THIS WAS WRITTEN, as a compile-time constant
 * (delim-diagnostics, 2026-09-17) — Delim's own diagnostic, moved
 * here because Delim is the one real consumer: every `(using At)`
 * parameter above threads a caller's own position into a captured
 * continuation that has no useful JVM stack trace of its own (it is
 * resumed on another thread, another process, a week later).
 *
 * A captured continuation has no useful JVM stack trace — it is
 * resumed on another thread, in another process, a week later, and
 * the interpreter's own frames are what a debugger shows. What the
 * machine CAN say is where in the program's own structure it is, and
 * that needs one fact the compiler has and the runtime does not: the
 * source position of a call site.
 *
 * It is a GIVEN rather than an `inline def` called by hand, because
 * the position wanted is the CALLER's: a method that takes
 * `(using At)` gets the line of whoever called it, since implicit
 * search runs there. A library then needs no inline wrapper per door.
 *
 *     def delimited[R, F[+_]](body: …)(using At): R ! F
 *     Delim.delimited[Int, Pure](…)      // At("Booking.scala:31")
 *
 * Cost: one reference to an interned string literal per call that
 * builds a prompt. Nothing is computed at run time.
 *
 * THE ONE RULE FOR USING IT INSIDE THIS MODULE: a macro cannot be
 * expanded in the compilation run that defines it, so `okay`'s own
 * main sources must never SUMMON an `At` — they thread the one their
 * caller supplied. `Delim` does exactly that: every internal call
 * passes its own `using` parameter along, and nothing in
 * `src/main/scala` writes `summon[At]` or calls an `At`-taking method
 * without one in scope.
 */
final case class At(where: String) extends AnyVal:
  override def toString: String = where

object At:

  /** for a call that has no position to offer — a prompt built by
   * machinery rather than by a line of somebody's program */
  val unknown: At = At("<unknown>")

  /** the caller's `file:line` */
  inline given here: At = ${ hereImpl }

  // NOT private: an inline body reaching a private member makes the
  // compiler synthesize an accessor with an unstable name (E192) —
  // the same finding as `Delim.Emitting.in`'s and `Cont.Shift`'s,
  // measured here 2026-09-17
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
