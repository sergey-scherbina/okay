package okay

import okay.freer.{Free, Freer}

/**
 * HANDLERS AS FRAMES OF THE ONE MACHINE (handle-frames, specs/handle-frames.md; on `Delimited` since cont-atm,
 * specs/cont-atm.md §5).
 *
 * A handler's run is a VALUE (`Run`, a `Shift.Pending` in a `Delay`) with two faces: its fast fold, and its
 * FRAME. Forced by anything but a machine, it is the fold; a fold that meets a run nested in it forces that one
 * as a fold too, until `Limit` folds deep, and there as its frame on a machine — inside which every handler is a
 * frame, so nothing nests past it. A running machine steps into a run's frame in its own loop. The host stack is
 * bounded by `Limit`, whatever the program's handler nesting; a composition of handlers stays on the folds.
 *
 * A frame is Shift's `ret $ body` with a `Handling` for its prompt: on the machine a value boundary like any
 * prompt's, and an operation the frame takes is a capture to it whose body is the clause (Shift's `Steps`). A
 * handler with a state is parameter passing: the frame answers `S => program`, the clause applies the
 * continuation's answer to the next state, and the frame's answer is applied to the state it started from.
 *
 * THE CLAIM this file makes, once per form (`framed`): a program of the handled row `F + G` is a program of
 * `Shift % ? + G` on the machine, at the same erasure — every operation of `F` in it is taken by the frame
 * before it could leave. And a program of `G` is one of `Shift % ? + G` (`widened`: rows are invariant).
 */
// public: the inline handler forms expand into every caller's code and reach it from there
object HandleFrames:

  /**
   * A HANDLER'S PROMPT: the frame `ret $ body` of a handler that runs on the machine. It says which operations are
   * its own; for one of them the machine makes the operation a capture to this frame whose body is `clause` — `k`
   * the continuation up to and including the frame, so a resumption re-installs it (a deep handler). Erased: the
   * form that makes it knows its types and makes the claim.
   */
  abstract class Handling[Y](name: String) extends Prompt[Y](name, "handler"):
    def takes(op: Any): Boolean
    /** a program of `Y` in the frame's row; `k(v)` one too */
    def clause(op: Any, k: Any => Any): Any

  /**
   * A CATCH FRAME'S PROMPT (handle-frames-catch): `ret $ body` with a `try` around everything the body runs, kept
   * as DATA on the machine's stack — so a body nested a hundred thousand deep holds no host `try` per level. A run
   * that installs one calls user code under a `try` from then on (`Delimited.guarding`); a throw goes to the
   * nearest catch frame, the frames above it dropped as a throw drops them — or, none taking it, is thrown on.
   */
  trait Catching:
    self: Prompt[?] =>
    /** the frame's answer for `t` — a program at its row — or null: not this frame's */
    def caught(t: Throwable): Any
    /** a FINALIZER (a `Resource` scope): it sees a dropped continuation's throw too (`Shift.Discontinued`), which
     * an abort through it must be told of — the machine discontinues a dropped piece only when it holds one */
    def finalizes: Boolean = false

  /**
   * THE STATE FRAME, for every loop that threads a state (handle-frames-loops): from state `s0` over `x`,
   * `step(s, op, resume)` answers the operation — `resume(s2, v)` goes on with the next state and the
   * operation's answer, and a clause that does not call it stops there (a fold that is done); `ret(s, a)` is the
   * answer at the end. Parameter-passing: the frame answers `S => program`.
   */
  def stateful[F[+_], S, A, R, G[+_]](t: TypeableK[F], ret: (S, A) => R ! G)
                                     (step: (S, Any, (S, Any) => R ! G) => R ! G)(s0: S, x: A ! F + G): Shift.U[G, R] =
    statefulOver[F, S, A, R, G, F + G](t, ret)(step)(s0, x)

  /** `stateful` over a program whose row is not `F + G` itself — a walker that RE-TELLS at another type (`Writer.map`:
   * `Writer % W` in, `Writer % V` out): what the frame does not take is still the rest of the row, at erasure */
  def statefulOver[F[+_], S, A, R, G[+_], H[+_]](t: TypeableK[F], ret: (S, A) => R ! G)
                                                (step: (S, Any, (S, Any) => R ! G) => R ! G)(s0: S, x: A ! H): Shift.U[G, R] =
    statefulAll[S, A, R, G, H](t.test, ret, null)(step)(s0, x)

  /**
   * the state frame in full: `takes` says which operations are the frame's (a frame that must SEE a forwarded
   * operation — Resource's release before a final one — takes them all and performs them again below itself);
   * `onThrow`, if not null, makes it a catch frame too, answering a throw from inside with the state in force
   * (handle-frames-catch): the frame's answer is `S => program`, applied to that state by the frame below it
   */
  def statefulAll[S, A, R, G[+_], H[+_]](takes0: Any => Boolean, ret: (S, A) => R ! G, onThrow: ((S, Throwable) => R ! G) | Null)
                                        (step: (S, Any, (S, Any) => R ! G) => R ! G)(s0: S, x: A ! H): Shift.U[G, R] =
    type Ans = S => Shift.U[G, R]
    val frame: Handling[Ans] =
      if onThrow == null then new Handling[Ans]("state"):
        def takes(op: Any): Boolean = takes0(op)
        def clause(op: Any, k: Any => Any): Any = pure[Shift % ? + G, Ans]((s: S) => clauseAt[S, R, G](step, s, op, k))
      else
        val thrown = onThrow
        new Handling[Ans]("state") with Catching:
          def takes(op: Any): Boolean = takes0(op)
          def clause(op: Any, k: Any => Any): Any = pure[Shift % ? + G, Ans]((s: S) => clauseAt[S, R, G](step, s, op, k))
          def caught(t: Throwable): Any = pure[Shift % ? + G, Ans]((s: S) => widened(thrown(s, t)))
          // `onThrow` answers every throw, a dropped continuation's included: a state frame's catch is its finalizer
          override def finalizes: Boolean = true
    val back: A => Shift.U[G, Ans] = a => pure[Shift % ? + G, Ans]((s: S) => widened(ret(s, a)))
    Shift.dollar[A, Ans, G](frame)(back)(framed[A, H, G](x)).flatMap(g => g(s0))

  /**
   * ONE STEP, BOTH FACES (handler-one-step, PROBE): a TAIL-RESUMPTIVE state handler written once, as its step —
   * `step(s, op)` answers the next state and the operation's value — with its fold and its frame both BUILT from
   * it. Inline, with the step inline: at each call site the step expands into the fold's own loop, the shape of
   * the hand-written folds (`val (s2, v) = f(s, op); loop(s2)(k(v))`). The resuming form `(s, op, resume)` cannot
   * be the fold's: the `resume` it is handed is a closure, and a loop call inside a closure is no tail call
   * (`@tailrec` refused it, measured).
   */
  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def stateRun[F[+_], S, A, R, G[+_]](t: TypeableK[F], inline ret: (S, A) => R ! G)
                                            (inline step: (S, Any) => (S, Any))(s0: S, x: A ! F + G): R ! G =
    given TypeableK[F] = t
    // a call from inside flatMap cannot be a jump; `again` takes it, so the walk stays a checked loop
    def again(d: Int)(s: S)(y: A ! F + G): R ! G = loop(d)(s)(y)
    @scala.annotation.tailrec def loop(d: Int)(s: S)(y: A ! F + G): R ! G = (y.resumeRun: @unchecked) match
      case Free.Return(a) => ret(s, a)
      // a lone operation: the program's last, its value the program's — answered in place, no node built
      case i @ Free.Inject(e) => split[F, G](e)(op => { val (s2, v) = step(s, op); ret(s2, fed[A](v)) })
                                               (_ => forwarded[F, G](i).flatMap(v => ret(s, v)))
      case Free.Bind(i @ Free.Inject(e), k) => split[F, G](e)(op => { val (s2, v) = step(s, op); loop(d)(s2)(feed(k, v)) })
                                                             (_ => forwarded[F, G](i).flatMap(v => again(d)(s)(k(v))))
      case z => loop(d)(s)(shallow(z, d))
    run[R, G](d => loop(d)(s0)(x), stateful[F, S, A, R, G](t, ret)((s, op, resume) => { val (s2, v) = step(s, op); resume(s2, v) })(s0, x))

  /**
   * `stateRun` that STOPS (handler-one-step stage 2, specs/fold-until.md): the moment the state is `done` — at the
   * start, or after a step — the run answers `end(s)` and goes no further: the continuation is not called, so
   * nothing past that operation is built and an operation of `G` that would have followed is never performed. Its
   * frame is the same: a clause whose state is done does not resume.
   */
  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def stateRunUntil[F[+_], S, A, R, G[+_]](t: TypeableK[F], inline done: S => Boolean, inline end: S => R)
                                                 (inline step: (S, Any) => (S, Any))(s0: S, x: A ! F + G): R ! G =
    given TypeableK[F] = t
    def again(d: Int)(s: S)(y: A ! F + G): R ! G = loop(d)(s)(y)
    @scala.annotation.tailrec def loop(d: Int)(s: S)(y: A ! F + G): R ! G =
      if done(s) then Free.Return(end(s))
      else (y.resumeRun: @unchecked) match
        case Free.Return(_) => Free.Return(end(s))
        case i @ Free.Inject(e) => split[F, G](e)(op => { val (s2, _) = step(s, op); Free.Return(end(s2)): R ! G })
                                                 (_ => forwarded[F, G](i).map(_ => end(s)))
        case Free.Bind(i @ Free.Inject(e), k) => split[F, G](e)(op => { val (s2, v) = step(s, op); loop(d)(s2)(feed(k, v)) })
                                                               (_ => forwarded[F, G](i).flatMap(v => again(d)(s)(k(v))))
        case z => loop(d)(s)(shallow(z, d))
    if done(s0) then Free.Return(end(s0))
    else run[R, G](d => loop(d)(s0)(x), stateful[F, S, A, R, G](t, (s, _) => pure(end(s)))(
      (s, op, resume) => { val (s2, v) = step(s, op); if done(s2) then pure(end(s2)) else resume(s2, v) })(s0, x))

  /** a step's other answer (`stateRunOr`): the run stops here, answering `answer` — allocated only on the stop */
  final class Stop[+B](val answer: B)

  /**
   * `stateRun` whose step may STOP the run on an operation (handler-one-step stage 3: Chronicle's halt): the step
   * answers `(s2, v)` to resume or a `Stop(program)` to answer the whole run with — the continuation not called, so
   * nothing past that operation is built. Its frame is the same: a clause that stops does not resume.
   */
  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def stateRunOr[F[+_], S, A, R, G[+_]](t: TypeableK[F], inline ret: (S, A) => R ! G)
                                              (inline step: (S, Any) => (S, Any) | Stop[R ! G])(s0: S, x: A ! F + G): R ! G =
    given TypeableK[F] = t
    def again(d: Int)(s: S)(y: A ! F + G): R ! G = loop(d)(s)(y)
    @scala.annotation.tailrec def loop(d: Int)(s: S)(y: A ! F + G): R ! G = (y.resumeRun: @unchecked) match
      case Free.Return(a) => ret(s, a)
      case i @ Free.Inject(e) => split[F, G](e)(op => step(s, op) match
          case st: Stop[R ! G] @unchecked => st.answer
          case (s2, v) => ret(s2, fed[A](v)))
        (_ => forwarded[F, G](i).flatMap(v => ret(s, v)))
      case Free.Bind(i @ Free.Inject(e), k) => split[F, G](e)(op => step(s, op) match
          case st: Stop[R ! G] @unchecked => st.answer
          case (s2, v) => loop(d)(s2)(feed(k, v)))
        (_ => forwarded[F, G](i).flatMap(v => again(d)(s)(k(v))))
      case z => loop(d)(s)(shallow(z, d))
    run[R, G](d => loop(d)(s0)(x), stateful[F, S, A, R, G](t, ret)((s, op, resume) => step(s, op) match
      case st: Stop[R ! G] @unchecked => st.answer
      case (s2, v) => resume(s2, v))(s0, x))

  /** THE CLAIM the fold makes of its step: the value it resumes with is the operation's answer */
  inline def feed[X, B](k: X => B, v: Any): B = k(v.asInstanceOf[X])
  /** the same claim for a lone operation, whose answer is the program's */
  inline def fed[A](v: Any): A = v.asInstanceOf[A]

  /** the clause with `resume` made of the frame's continuation: `k(v)` answers `S => program`, applied to `s2` */
  private def clauseAt[S, R, G[+_]](step: (S, Any, (S, Any) => R ! G) => R ! G, s: S, op: Any, k: Any => Any): Shift.U[G, R] =
    widened(step(s, op, (s2, v) => answered[S => Shift.U[G, R], G](k(v)).flatMap(g => g(s2)).asInstanceOf[R ! G]))

  /** the state form (`Handler.stateOf`) as a frame from state `s0` over `x` */
  def state[F[+_], S, A, G[+_]](f: [X] => (S, F[X]) => (S, X), t: TypeableK[F])(s0: S, x: A ! F + G): Shift.U[G, (S, A)] =
    stateful[F, S, A, (S, A), G](t, (s, a) => pure((s, a)))((s, op, resume) =>
      val (s2, v) = f(s, op.asInstanceOf[F[Any]])
      resume(s2, v))(s0, x)

  /** a `try` as a frame (handle-frames-catch): `h` answers a throw from anything `x` runs, `x` built under it too */
  def catching[A, G[+_]](h: Throwable => A ! G)(x: => A ! G): Shift.U[G, A] =
    val frame = new Prompt[A]("try", "catch") with Catching:
      // a dropped continuation's throw is no failure here: answering it would run on in what was dropped
      def caught(t: Throwable): Any = t match
        case _: Shift.Discontinued => null
        case _ => widened(h(t))
    Shift.dollar[A, A, G](frame)(a => pure(a))(Free.delay(() => widened(x)))

  /**
   * `onUnwind` run when a throw leaves `x` — any throw, a dropped continuation's included (a FINALIZING frame) —
   * and the throw goes on (logic-cut-releases: a search a throw leaves abandons the branch points it holds)
   */
  def unwinding[A, G[+_]](onUnwind: () => Unit)(x: => A ! G): A ! G =
    val frame = new Prompt[A]("unwind", "catch") with Catching:
      def caught(t: Throwable): Any = { onUnwind(); null }
      override def finalizes: Boolean = true
    CanTry.guardRows[A, G](x)(t => { onUnwind(); throw t })(
      Shift.dollar[A, A, G](frame)(a => pure(a))(Free.delay(() => widened(x))))

  /**
   * `x` — a continuation `d` is thrown into (`Resumption.discontinue`) — under a frame that takes `d` back
   * (resource-abort-releases): `()` when it arrives, every scope inside released; a release that failed, attached
   * to `d` suppressed, thrown on from here as the failure it is; `()` too if something inside answered after all
   */
  def dropping[G[+_]](d: Shift.Discontinued)(x: Any ! Shift % ? + G): Unit ! Shift % ? + G =
    val frame = new Prompt[Unit]("discontinue", "catch") with Catching:
      def caught(t: Throwable): Any =
        if !(t eq d) then null
        else d.getSuppressed match
          case Array() => pure[Shift % ? + G, Unit](())
          case failed =>
            val first = failed(0)
            failed.iterator.drop(1).foreach(first.addSuppressed)
            throw first
    Shift.dollar[Any, Unit, G](frame)(_ => pure(()))(x)

  /**
   * a frame whose clause gets the operation and its continuation as a function to programs of `G` — for a handler
   * that is no fold of one answer (Logic.msplit's search: the continuation run at every alternative, the ones not
   * yet taken handed out in the answer as `pending` programs)
   */
  def handling[A, B, G[+_]](name: String, takes0: Any => Boolean, ret: A => B ! G)
                           (clause0: (Any, Any => B ! G) => B ! G)(x: Any): Shift.U[G, B] =
    val frame = new Handling[B](name):
      def takes(op: Any): Boolean = takes0(op)
      def clause(op: Any, k: Any => Any): Any = widened(clause0(op, k.asInstanceOf[Any => B ! G]))
    Shift.dollar[A, B, G](frame)(a => widened(ret(a)))(answered[A, G](x))

  /** the control form (`Effects[Free].handle`, `Handler.control`) as a frame over `x`: the clause gets `k` */
  def control[F[+_], A, B, G[+_]](ret: A => Free[G, B], h: F !> Free[G, B], t: TypeableK[F])(x: Free[F + G, A]): Shift.U[G, B] =
    val frame = new Handling[B]("handle"):
      def takes(op: Any): Boolean = t.test(op)
      def clause(op: Any, k: Any => Any): Any = widened(h(op.asInstanceOf[F[Any]]) / k.asInstanceOf[Any => Free[G, B]])
    Shift.dollar[A, B, G](frame)(a => widened(ret(a)))(framed[A, F + G, G](x))

  /** a frame program as a value: stepped into by a running machine, else run on a machine of its own */
  def pending[B, G[+_]](program: Shift.U[G, B]): B ! G =
    Free.delay(Shift.nestedRun[B, G](program))

  // THE CLAIMS, made here and nowhere else (see the header)
  /** a program of the handled row is one of the frame's row: the frame takes what is not `G`'s */
  private def framed[A, H[+_], G[+_]](x: A ! H): Shift.U[G, A] = x.asInstanceOf[Shift.U[G, A]]
  /** a program of `G` is one of `Shift % ? + G` (a row coercion: rows are invariant) */
  private def widened[A, G[+_]](x: A ! G): Shift.U[G, A] = x.asInstanceOf[Shift.U[G, A]]
  /** what a frame's `k` answers: a program of the frame's answer, in its row */
  private def answered[A, G[+_]](x: Any): Shift.U[G, A] = x.asInstanceOf[Shift.U[G, A]]

  /**
   * HOW DEEP FOLDS NEST (handle-frames-loops, measured): a fold that meets a nested run FORCES it — the nested
   * fold runs on this one's stack, the old behaviour and the fast one — until it is `Limit` folds deep; there
   * the nested run runs as its FRAME on a machine of its own, and inside a machine every handler is a frame, so
   * nothing nests past it. Handing the outer fold to the machine at the first nested run instead cost a
   * composition of two handlers 7.4x (SplitBenchmark.mixedList, `State.run(Writer.run(p))`): on the machine every
   * tail-resumptive operation is a capture. The bound is the stack's: 32 folds of a few frames each, well inside a
   * 128 KB thread and Scala.js.
   */
  final val Limit = 32

  /**
   * a handler's run as ONE object (stateSmall: a holder of two closures was four allocations a run,
   * 1.55x on 100 small runs): `at(depth)` its fold, entered `depth` folds deep; `program` its frame
   */
  abstract class Run[B, G[+_]] extends Shift.Pending[B, G]:
    def at(depth: Int): B ! G
    final def apply(): B ! G = at(0)
    /** set when this run IS a stepped handler's over a program (`handled`): the next `handle` builds a stack of
     * two from it instead of nesting a second run (handler-single-pass) */
    private[okay] var markedStep: Handler.Stepped[?, Any, ?] | Null = null
    private[okay] var markedUnder: Any = null

  /** a loop's run as a value, its two faces written in place (one object, as `Run` says) */
  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def run[B, G[+_]](inline fast: Int => B ! G, inline frame: Shift.U[G, B]): B ! G =
    Free.delay(new Run[B, G]:
      def at(depth: Int): B ! G = fast(depth)
      def program: Shift.U[G, B] = frame)

  /**
   * the nested run at the head of `y` (a `Delay`, alone or under its `Bind`) FORCED, for a fold `depth` deep: as
   * its fold below the `Limit`, as its frame on a machine at it. Any other run is forced as it is.
   */
  def shallow[A, H[+_]](y: A ! H, depth: Int): A ! H = y match
    case Freer.Delay(t) => forced[A, H](t, depth)
    case Freer.Bind(Freer.Delay(t), g) => forced[Any, H](t, depth).flatMap(g.asInstanceOf[Any => A ! H])
    case other => other

  /** THE ONE CLAIM: the thunk of a `Delay` in a program of row H answers a program of row H */
  private def forced[A, H[+_]](t: () => Any, depth: Int): A ! H = t match
    case r: Run[?, ?] =>
      if depth < Limit then r.at(depth + 1).asInstanceOf[A ! H]
      else Shift.nestedRun[A, H](r.program.asInstanceOf[Shift.U[H, A]])()
    case o => o().asInstanceOf[A ! H]

  // ---- THE ONE-PASS STACK (handler-single-pass stage 2, specs/handler-single-pass.md) ----

  /** the row a stack's walk is typed at: its handlers' rows, met only at run time (`Handled`'s claim) */
  type Erased[+X] = Any

  /**
   * `p.handle(h)` for a stepped `h` (`Free.handle`): on a program that already IS a handled stack, `h` is pushed
   * onto that stack, and nothing is walked. Otherwise the program becomes a stack of one.
   */
  def handled[A, O[_], F[+_]](p: Any, h: Handler.Stepped[?, ?, ?]): O[A] ! F =
    val one = erasedStep(h)
    p match
      // already a stack: one more on it
      case Freer.Delay(st: Handled) => Free.delay(st.push(one)).asInstanceOf[O[A] ! F]
      // a stepped handler's own run over a program: the two become a stack
      case Freer.Delay(r: Run[?, ?]) if r.markedStep != null =>
        Free.delay(Handled(r.markedUnder.asInstanceOf[Any ! Erased], Array(r.markedStep.nn, one))).asInstanceOf[O[A] ! F]
      // anything else: the handler's own run, as alone it always was, marked so a next `handle` can stack on it
      case _ =>
        val own = runOf(one, p.asInstanceOf[Any ! Erased])
        own match
          case Freer.Delay(r: Run[?, ?]) =>
            r.markedStep = one
            r.markedUnder = p
          case _ => ()
        own.asInstanceOf[O[A] ! F]

  /** a stepped handler at the erased state its stack holds it at: the state is the handler's own, passed back to
   * it only (THE CLAIM: `init`'s value is what `step`, `ret` and `halted` are given, and nothing else is) */
  private def erasedStep(h: Handler.Stepped[?, ?, ?]): Handler.Stepped[?, Any, ?] = h.asInstanceOf[Handler.Stepped[?, Any, ?]]

  /**
   * A HANDLED PROGRAM AS A STACK of stepped handlers, innermost first (`stack(0)` was applied first). Its fold is
   * ONE walk: an operation goes to the innermost handler of the stack that takes it, found through a table of
   * the operation classes seen; the rest of the row leaves once. A stack starts at TWO: one handler is its own
   * `run`, marked (`Run.markedStep`), and the second `handle` turns the pair into a stack. Its frame is the handlers' own runs nested, so a machine meeting it steps into
   * frames as it always has.
   *
   * THE CLAIM this class makes, for the whole stack: `under` is a program of the innermost handler's row, every
   * handler's `ret` and `halted` answer a program of the row outside it, and `Free.handle` built the stack in
   * that order from the types it checked. Erased here because the stack is heterogeneous; typed at both ends.
   */
  final class Handled(val under: Any ! Erased, val stack: Array[Handler.Stepped[?, Any, ?]]) extends Run[Any, Erased]:
    def push(h: Handler.Stepped[?, Any, ?]): Handled = Handled(under, stack :+ h)

    /** the handlers' own runs, nested in the stack's order */
    private def nested: Any ! Erased =
      var p = under
      var i = 0
      while i < stack.length do
        p = runOf(stack(i), p)
        i += 1
      p

    def at(depth: Int): Any ! Erased =
      Walk(stack, depth).loop(0, Array.tabulate[Any](stack.length)(i => stack(i).init), under)

    def program: Shift.U[Erased, Any] = widened(nested)

  /** one stepped handler's own `run` over `p`, at the erased row (the evidence its signature asks for was checked
   * by `Free.handle` when the stack was built) */
  private def runOf[E[+_], O[_]](h: Handler.Stepped[E, Any, O], p: Any ! Erased): Any ! Erased =
    h.run[Any, Erased](p.asInstanceOf[Any ! E + Erased])(using summon[Any <:< Any], Distinct.unchecked[E + Erased](),
      Handler.Nothing.any[Erased])

  /** the walk of one stack, `depth` folds deep: its handlers' states, and the table of operation classes */
  private final class Walk(stack: Array[Handler.Stepped[?, Any, ?]], depth: Int):
    private val n = stack.length
    private var classes = new Array[Class[?]](8)
    private var index = new Array[Int](8)
    private var filled = 0

    /** the innermost handler at or past `lo` that takes `op`, or `n`: the table first, its entry checked by that
     * handler's own test (an instance by name shares its class with another), then the stack in order */
    // NOT the last class first: SW, whose effects alternate, read 1.35x with it against 1.15x without
    private def find(op: Any, lo: Int): Int =
      val c = op.getClass
      var j = 0
      while j < filled && (classes(j) ne c) do j += 1
      if j < filled then
        val i = index(j)
        if i >= lo && i < n && stack(i).takes.test(op) then return i
      var i = lo
      while i < n && !stack(i).takes.test(op) do i += 1
      if j == filled then
        if filled == classes.length then
          classes = java.util.Arrays.copyOf(classes, filled * 2)
          index = java.util.Arrays.copyOf(index, filled * 2)
        classes(filled) = c
        filled += 1
      index(j) = i
      i

    // a call from inside flatMap cannot be a jump; `again` takes it, so the walk stays a checked loop
    private def again(lo: Int, s: Array[Any], x: Any ! Erased): Any ! Erased = loop(lo, s, x)

    /** handlers `lo` and past are active, `s` their states. A program that ended is answered by the innermost
     * active handler's `ret`, and that answer is walked by the handlers outside it */
    @scala.annotation.tailrec final def loop(lo: Int, s: Array[Any], x: Any ! Erased): Any ! Erased =
      if lo == n then x
      else (x.resumeRun: @unchecked) match
        case Free.Return(a) => loop(lo + 1, s, stack(lo).ret[Any, Erased](s(lo), a))
        case i @ Free.Inject(_) => loop(lo, s, i.flatMap(v => Free.Return(v)))
        case Free.Bind(i @ Free.Inject(e), k) =>
          val h = find(e, lo)
          if h < n then
            val v = stack(h).stepAt(s, h, e)
            // the run under `h` stops: the handlers inside it are dropped, `h` answers, the outside walks on
            if v.asInstanceOf[AnyRef] eq Handler.Halted then loop(h + 1, s, stack(h).halted[Any, Erased](s(h)))
            else loop(lo, s, feed(k, v))
          else
            // the rest of the row, once: every resumption of it gets the states as they were here (a multi-shot
            // handler outside resumes it more than once)
            val kept = s.clone()
            forwarded[Erased, Erased](i).flatMap(v => again(lo, kept.clone(), feed(k, v)))
        // a run nested here: forced — its fold below Limit, its frame on a machine at it
        case y => loop(lo, s, shallow(y, depth))
