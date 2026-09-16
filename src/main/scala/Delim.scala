package okay

import okay.!.*
import scala.annotation.tailrec

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

/** a delimiter's identity AND its answer type; identity is the tag */
final class Prompt[R]

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
                     underPrompt: Boolean, delimitK: Boolean) extends Delim[A]

/** a shift naming a prompt that is not installed */
final class NoPrompt extends RuntimeException(
  "shift to a prompt that is not on the stack")

object Delim {

  /** a fresh delimiter tag */
  def prompt[R]: Prompt[R] = new Prompt[R]

  /** run the body under the delimiter — reset, as an operation */
  def push[R, F[+_]](p: Prompt[R])(body: R ! (Delim + F)): R ! (Delim + F) =
    effect(Push(p, body))

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
                        (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = true, delimitK = true))

  /** the body CONSUMES the delimiter (a further shift to `p` escapes
   * outward), the continuation still re-installs it */
  def shift0[R, A, F[+_]](p: Prompt[R])
                         (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = false, delimitK = true))

  /** the body runs under the delimiter, the continuation does NOT
   * re-install it — a bare segment, spliced where it is invoked */
  def control[R, A, F[+_]](p: Prompt[R])
                          (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = true, delimitK = false))

  /** neither: the delimiter is consumed and the continuation is bare */
  def control0[R, A, F[+_]](p: Prompt[R])
                           (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = false, delimitK = false))

  /** the common shape: a fresh prompt, a block under it, run */
  def reset[R, F[+_]](body: Prompt[R] => R ! (Delim + F)): R ! F =
    val p = prompt[R]
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
   *     def banner: Prompted[Int] ?=> Int ! (Delim + W) = direct:
   *       "hello".tell
   *       1 + !Delim.shift[Int, Int, W](k => k(5))
   *
   * That function compiles, is a value, travels — and can only be
   * CALLED where a `delimited` put the evidence in scope.
   *
   * WHY NOT A ROW MEMBER. An obligation in the row (`A ! (Delim +
   * Prompted[p.type] + F)`, discharged by `push`) was written first
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

  /** install a fresh delimiter, run the body under it with the
   * evidence in scope, and handle the machine */
  def delimited[R, F[+_]](body: Prompted[R] ?=> R ! (Delim + F)): R ! F =
    val p = prompt[R]
    run(push(p)(body(using new Prompted[R](p))))

  /** capture up to the delimiter in force — the same word as the
   * prompt-taking primitive, and the compiler picks by what you
   * write: a prompt in the first clause is the primitive, a handler
   * there is this one (delim-one-name) */
  def shift[R, A, F[+_]](using in: Prompted[R])
                          (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
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
                         (using inline ctx: Direct.DirectCtx[F])(using rw: Reader.RowOf[F])
                         (f: (A => in.Res ! rw.R) => in.Res ! rw.R): A ! rw.R =
    okay.effect[rw.R, A](Capture(in.prompt, f, underPrompt = true, delimitK = true)
      .asInstanceOf[rw.R[A]])

  /** the 0-variant: the body consumes the delimiter */
  def shift0[R, A, F[+_]](using in: Prompted[R])
                           (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    shift0[R, A, F](in.prompt)(f)

  /** the continuation does not re-install the delimiter */
  def control[R, A, F[+_]](using in: Prompted[R])
                            (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    control[R, A, F](in.prompt)(f)

  /** neither */
  def control0[R, A, F[+_]](using in: Prompted[R])
                             (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    control0[R, A, F](in.prompt)(f)

  /** abort to the delimiter in force with a value */
  def abort[R, A, F[+_]](using in: Prompted[R])(value: R): A ! (Delim + F) =
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
                 (using inline ctx: Direct.DirectCtx[F])(using rw: Reader.RowOf[F])
                 (value: in.Res): Unit ! rw.R =
    shift[Unit](using in)(_ => okay.pure(value))

  /**
   * 2 · A PUSH API, READ AS A PULL.
   *
   * The evidence for a block that is collecting values. It carries
   * the element type as a MEMBER so `emit` can take no type argument
   * and still be precise about what it accepts.
   */
  final class Emitting[A] private[Delim] (prompted: Prompted[List[A]]):
    type Elem = A
    // NOT private: `emit` is inline and reaches this, and a private
    // member behind an inline body makes the compiler synthesize an
    // accessor with an unstable name (E192 — the same finding as
    // Cont.Shift's, measured here 2026-09-17)
    val in: Prompted[List[Elem]] = prompted

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
  def collect[A, F[+_]](body: Emitting[A] ?=> Unit ! (Delim + F)): List[A] ! F =
    delimited[List[A], F](
      body(using new Emitting[A](summon[Prompted[List[A]]]))
        .map(_ => List.empty[A]))

  /** emit one value into the `collect` in force */
  inline def emit(using e: Emitting[?])[F[_]]
                 (using inline ctx: Direct.DirectCtx[F])(using rw: Reader.RowOf[F])
                 (a: e.Elem): Unit ! rw.R =
    shift[Unit](using e.in)(k => k(()).map(a :: _))

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
    case Ask[Q, A, R, G[+_]](question: Q, resume: A => Paused[Q, A, R, G] ! G)
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
        case Ask(q, _) => Some(q)
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
  def resumable[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! (Delim + F))
                               : Dialogue[Q, A, R, F] ! F =
    delimited[Dialogue[Q, A, R, F], F](
      body(using new Asking[Q, A, R, Delim + F](summon[Prompted[Dialogue[Q, A, R, F]]]))
        .map(Paused.Done[Q, A, R, Delim + F](_)))

  /** ask, and hand the rest of the program back to the caller */
  inline def pause(using s: Asking[?, ?, ?, ?])[F[_]]
                  (using inline ctx: Direct.DirectCtx[F])(using rw: Reader.RowOf[F])
                  (q: s.Qst): s.Ans ! rw.R =
    shift[s.Ans](using s.in)(k => okay.pure(Paused.Ask(q,
      // THE ONE CAST here, and what makes it right: `s` can only be
      // held inside the `resumable` that installed this prompt, so
      // the block's row — read off its DirectCtx as `rw.R` — IS the
      // row `resumable` ran the body in. The type system cannot join
      // those two spellings of one row (the same reason the inline
      // `shift` above casts its own result), and nothing else can
      // produce an `Asking`.
      k.asInstanceOf[s.Ans => Paused[s.Qst, s.Ans, s.Fin, s.Row] ! s.Row])))

  /** answer every question until the dialogue is done — the driver
   * for the common case where the answers are available now */
  def drive[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F])(answer: Q => A ! F): R ! F =
    p match
      case Paused.Done(r) => okay.pure(r)
      case Paused.Ask(q, resume) =>
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
  def answer[Q, A, R, F[+_]](p: Dialogue[Q, A, R, F], j: Journal[A])(a: A)
                            : (Dialogue[Q, A, R, F], Journal[A]) ! F =
    p match
      case Paused.Ask(_, resume) => run(resume(a)).map(next => (next, j :+ a))
      case done => okay.pure((done, j))     // nobody asked; nothing to record

  /**
   * Where the dialogue stands, from its program and its journal —
   * what replaces persisting a continuation. A fresh process, a
   * different machine, a redeploy: same answers in, same place out.
   */
  def replay[Q, A, R, F[+_]](body: Asking[Q, A, R, Delim + F] ?=> R ! (Delim + F))
                            (j: Journal[A]): Dialogue[Q, A, R, F] ! F =
    j.foldLeft(resumable[Q, A, R, F](body)): (acc, a) =>
      acc.flatMap:
        case Paused.Ask(_, resume) => run(resume(a))
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
                     (using inline ctx: Direct.DirectCtx[F])(using rw: Reader.RowOf[F])
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
  private enum Segs[F[+_], A, Z]:
    case Done[F[+_], Z]() extends Segs[F, Z, Z]
    case K[F[+_], X, Y, Z](f: X => Y ! (Delim + F), rest: Segs[F, Y, Z]) extends Segs[F, X, Z]
    case Mark[F[+_], X, Z](p: Prompt[X], rest: Segs[F, X, Z]) extends Segs[F, X, Z]

  /** the stack cut at a prompt: what was captured (a chain ending
   * where the prompt's mark was) and what lies outside it */
  private final case class Cut[F[+_], A, P, Z](captured: Segs[F, A, P], outer: Segs[F, P, Z])

  /** the machine's state between steps: a program and the stack it
   * continues into */
  private final case class Next[F[+_], A, Z](prog: A ! (Delim + F), kont: Segs[F, A, Z])

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
  def run[R, F[+_]](prog: R ! (Delim + F)): R ! F = {
    type Row = Delim + F
    type Prog[A] = A ! Row

    /** frames back into a program: binds become flatMaps, markers
     * become pushes — the continuation re-installs its delimiter */
    def reify[A, P](segs: Segs[F, A, P], start: Prog[A]): Prog[P] = segs match
      case Segs.Done() => start
      case Segs.K(f, rest) => reify(rest, start.flatMap(f))
      case Segs.Mark(p, rest) => reify(rest, effect[Row, A](Push(p, start)))

    /** cut the chain at the mark of p: the mark's prompt IS p by
     * identity, and Same's witness makes the mark's type P's */
    def split[A, P, Z](kont: Segs[F, A, Z], p: Prompt[P]): Option[Cut[F, A, P, Z]] = kont match
      case Segs.Done() => None
      case Segs.Mark(q, rest) =>
        (q === p) match
          case Some(ev) =>
            Some(Cut(ev.liftCo[[t] =>> Segs[F, A, t]](Segs.Done()), ev.liftCo[[t] =>> Segs[F, t, Z]](rest)))
          case None => split(rest, p).map(c => Cut(Segs.Mark(q, c.captured), c.outer))
      case Segs.K(f, rest) => split(rest, p).map(c => Cut(Segs.K(f, c.captured), c.outer))

    // ONE tail-recursive loop: an earlier version split it into
    // loop/onOp, and mutual recursion is not tail-optimised, so every
    // operation cost frames and a thousand nested captures blew the
    // stack. Merged, only a FOREIGN operation suspends (under a
    // flatMap closure, as State.handle does) and the Delim ops
    // themselves are flat.
    @tailrec def loop(state: Next[F, ?, R]): R ! F = state match
      case n: Next[F, a, R] => (n.prog.resume: @unchecked) match
        case Pure(x) => n.kont match
          case Segs.Done() => okay.pure(x)
          case Segs.K(f, rest) => loop(Next(f(x), rest))
          // the delimited block finished normally: drop its marker
          case Segs.Mark(_, rest) => loop(Next(okay.pure(x), rest))

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
            split(kont, cap.prompt) match
              case Some(cut) =>
                val k = (v: a) => {
                  val seg = reify(cut.captured, okay.pure[Row, X](v))
                  if cap.delimitK then effect[Row, p](Push(cap.prompt, seg)) else seg
                }
                // claim 2: f takes a continuation into the prompt's
                // answer and gives back a program at it, in this row
                val body = cap.f.asInstanceOf[(a => Prog[p]) => Prog[p]](k)
                // shift/control put the body back under the delimiter;
                // the 0-variants have consumed it
                if cap.underPrompt then Right(Next(effect[Row, p](Push(cap.prompt, body)), cut.outer))
                else Right(Next(body, cut.outer))
              case None => throw NoPrompt()
        }
        // a foreign operation suspends the machine: the residual
        // program performs it and resumes with the same stack
        (g => Left(Inject(g).flatMap(x => loop(Next(okay.pure(x), kont)))))

    loop(Next(prog, Segs.Done()))
  }

  /** abort to a prompt with a value: a shift that drops the
   * continuation (the 0-variant, so the delimiter goes with it) */
  def abort[R, A, F[+_]](p: Prompt[R])(value: R): A ! (Delim + F) =
    shift0[R, A, F](p)(_ => okay.pure(value))
}

/** The class IS the whole identity: Delim has no parameter but its
 * (erased) answer type, so splitting a row on it is a TOTAL test —
 * said once here, rather than as a "cannot be checked at runtime"
 * warning at every use site of a test that is in fact complete. */
