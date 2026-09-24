package okay

/**
 * A DURABLE PROGRAM'S OWN NON-DETERMINISM (dialogue-asks, stage 1b of
 * specs/durable-workflow.md).
 *
 * `Replayable` made the discipline a type: a program you intend to
 * replay may not reach outside except through `pause`. That is
 * correct and, on its own, unusable — every real workflow needs a
 * clock, an id, sometimes a die, and refusing them is not an answer.
 *
 * The answer is that those are QUESTIONS TOO, asked of the runtime
 * instead of the author's oracle, and remembered in the same journal.
 * `Wf.now()` on a first run reads the clock and writes the reading
 * down; on every replay it reads the writing. The program stays a
 * pure function of its journal, which is the only thing that makes
 * replay exact.
 *
 * WHY THE `Either`. A dialogue's question type belongs to its author,
 * so "give me the clock" has nowhere to live in it. The channel is
 * therefore a sum the LIBRARY owns — `Either[Sys, Q]` for questions,
 * `Either[SysA, A]` for answers — and the author never writes it:
 * `Wf.pause(q)` wraps and unwraps `Right`, `Wf.now()` wraps and
 * unwraps `Left`. The two alternatives were weighed in the spec; the
 * decisive one is below.
 *
 * WHAT THE TAG BUYS, AND IT IS THE WHOLE OF `patch`. A journal entry
 * now SAYS whether it answers a library question or an author's. That
 * single bit makes a program CHANGEABLE:
 *
 *   - replaying, the pending question is `Patch(id)` and the next
 *     entry is a `Left(Flag(b))` for it — use `b`;
 *   - replaying, the pending question is `Patch(id)` and the next
 *     entry is a `Right` — the run that wrote this journal did not
 *     have this patch, so the answer is `false` and the entry is NOT
 *     consumed (it still answers the question it was written for);
 *   - live — the answer is `true`, and it is recorded.
 *
 * That is Temporal's `getVersion`, and here it falls out of the
 * tagging rather than being a rule a driver has to remember. Without
 * the tag, an old journal's next answer would be eaten by a question
 * that did not exist when it was written — the same silent
 * mis-mapping the `program` field of `okay.persist.Dialogue.Entry`
 * exists to stop.
 */
object Wf:

  /** the questions the RUNTIME answers, not the author's oracle */
  enum Sys:
    case Now
    case Uuid
    case Random
    /** is this branch on, for THIS run? (Temporal's getVersion) */
    case Patch(id: String)
    // ── the three the runtime CANNOT answer when they are asked
    // (workflow-suspended-driver). Each is answered by something the
    // driver does not control: the passage of time, somebody else's
    // action, another run finishing.
    /** wake me at this wall-clock instant */
    case Timer(untilMillis: Long)
    /** wake me when this named signal arrives */
    case Signal(name: String)
    /** wake me when this child dialogue finishes */
    case Child(id: String)
    /**
     * HAS SOMEBODY ASKED THIS RUN TO STOP? A cooperative check, and
     * cooperative on purpose (workflow-cancel, 2026-09-17): a
     * `try/catch` in a `direct` block guards the BUILDING of a
     * program, not its running (TestDelimLimits pins that), so a
     * cancellation delivered as a throw could not be caught by the
     * program it is cancelling. An `if` can. The answer is journalled
     * like every other, so a replay makes the same decision.
     */
    case Cancelled

  /** their answers, tagged so a journal entry says what it answers */
  enum SysA:
    case Millis(v: Long)
    case Text(v: String)
    case Dice(v: Double)
    case Flag(v: Boolean)
    /** the deadline passed */
    case Elapsed
    /** the signal arrived, or the child finished, with this payload */
    case Got(v: String)

  /**
   * WHAT A RUN IS WAITING FOR when nobody present can answer it.
   * This is operational data ABOUT a run, never part of it: the run's
   * state is still only its journal, and a `Wait` is what somebody
   * else needs to know to append the next entry.
   */
  enum Wait:
    case Until(millis: Long)
    case Signal(name: String)
    case Child(id: String)

  /**
   * WHERE A DRIVE STOPPED. `Done` and `Asking` are the two the model
   * always had; `Waiting` is the whole of workflow-suspended-driver,
   * and it is what makes timers, signals and child workflows one
   * feature rather than three.
   */
  enum Step[Q, R]:
    case Done[Q, R](value: R) extends Step[Q, R]
    /** the AUTHOR's question — an oracle, a person or an API answers */
    case Asking[Q, R](q: Q) extends Step[Q, R]
    /** nobody here can answer it yet; this says who can */
    case Waiting[Q, R](on: Wait) extends Step[Q, R]

  /** the question a durable program asks: the library's, or its own */
  type Ask[Q] = Either[Sys, Q]

  /** the answer it is given, tagged the same way */
  type Ans[A] = Either[SysA, A]

  /** what the author writes as the body's context */
  type Asking[Q, A, R, F[+_]] = Delim.Asking[Ask[Q], Ans[A], R, Delim + F]

  /** where such a program stands */
  type Paused[Q, A, R, F[+_]] = Delim.Dialogue[Ask[Q], Ans[A], R, F]

  /** the journal of such a program */
  type Journal[A] = List[Ans[A]]

  /** a driver answered a question with the wrong KIND of answer —
   * always a bug in the driver, never in the program */
  final class Mismatched(q: Any, a: Any)
    extends RuntimeException(s"a $q was answered with $a")

  /**
   * THE EVIDENCE CARRIES THE DOORS (wf-direct-door, 2026-09-17).
   *
   * The first cut of this module made every door a method with four
   * type arguments — `Wf.pause[String, String, String, Pure]("city?")`
   * — because `Q`, `A`, `R` and the row appear only in the evidence,
   * and a method cannot read them off a `using` parameter it has not
   * been given yet. That works and reads badly, which by this
   * project's standing rule ("useful, not just works") means it is
   * not finished.
   *
   * So the evidence is not a type alias any more: it is a class that
   * KNOWS its four types and offers the doors as methods on itself.
   * A body names them once, in its own signature, and every call site
   * inside writes none:
   *
   *     def booking(using w: Wf.Asks[String, String, String, Pure]) =
   *       direct:
   *         val city = !w.pause("city?")
   *         val id   = !w.uuid
   *         if !w.patch("promo") then ... else ...
   *
   * No macro was needed for this, which is the other half of the
   * point: the inline `Delim.pause` exists because a mark gives its
   * argument no expected type, and it pays one cast for that. Here
   * the types are on the object, so there is nothing to infer and
   * nothing to cast.
   */
  final class Asks[Q, A, R, F[+_]] private[okay] (
      private[okay] val in: Delim.Asking[Ask[Q], Ans[A], R, Delim + F]):

    /** ask the outside world, through the author's own question type */
    def pause(q: Q)(using At): A ! Delim + F =
      Wf.pause[Q, A, R, F](q)(using in, summon[At])

    /** the same under the name the literature uses: an "activity" is
     * a command performed outside and a result remembered */
    def perform(cmd: Q)(using At): A ! Delim + F = pause(cmd)

    /** the wall clock, once, remembered */
    def now(using At): Long ! Delim + F = Wf.now[Q, A, R, F](using in, summon[At])

    /** a fresh id, once, remembered */
    def uuid(using At): String ! Delim + F = Wf.uuid[Q, A, R, F](using in, summon[At])

    /** a die, once, remembered */
    def random(using At): Double ! Delim + F = Wf.random[Q, A, R, F](using in, summon[At])

    /** is this branch on for THIS run? */
    def patch(id: String)(using At): Boolean ! Delim + F =
      Wf.patch[Q, A, R, F](id)(using in, summon[At])

    /**
     * HAS SOMEBODY ASKED THIS RUN TO STOP, and why?
     *
     *     if !w.cancelled.isDefined then …compensate…; "cancelled"
     *     else …carry on…
     *
     * The check is where the AUTHOR puts it, which is the honest
     * shape for this library: a cancellation cannot be delivered as
     * an exception, because a `direct` block's `try/catch` guards the
     * building of the program rather than its running. What it costs
     * is stated in the guide: a run asleep for a year learns it was
     * cancelled when it wakes, not before.
     */
    def cancelled(using At): Option[String] ! Delim + F =
      Wf.cancelled[Q, A, R, F](using in, summon[At])

    /**
     * SLEEP, DURABLY. The run stops here and the driver returns
     * `Waiting(Until(t))`; a scheduler appends the answer when the
     * instant passes, and a worker carries the run on. Nothing is
     * blocked and nothing is held in memory in between — which is
     * the difference between this and `Thread.sleep`.
     *
     * The deadline is computed from `now`, so it is JOURNALLED: a
     * replay wakes at the same instant the first run chose, not at
     * one relative to the replay.
     */
    def sleep(millis: Long)(using At): Unit ! Delim + F =
      now.flatMap(t => Wf.timer[Q, A, R, F](t + millis)(using in, summon[At]))

    /** wait for a named signal from outside; the payload is its value */
    def awaitSignal(name: String)(using At): String ! Delim + F =
      Wf.signal[Q, A, R, F](name)(using in, summon[At])

    /** wait for a child dialogue to finish, and take its answer */
    def awaitChild(id: String)(using At): String ! Delim + F =
      Wf.child[Q, A, R, F](id)(using in, summon[At])

  // ── the author's doors ───────────────────────────────────────────

  /**
   * Ask the outside world, through the author's own question type —
   * `Delim.pause` with the tag put on and taken off.
   */
  def pause[Q, A, R, F[+_]](q: Q)(using s: Asking[Q, A, R, F], at: At): A ! Delim + F =
    Delim.ask[Ask[Q], Ans[A], R, F](Right(q)).map:
      case Right(a) => a
      case other => throw Mismatched(q, other)

  /**
   * THE SAME THING UNDER THE NAME THE LITERATURE USES. An "activity"
   * in a workflow engine is a command performed outside and a result
   * remembered; here that is a question and its answer, and there is
   * no second mechanism. Naming it is the whole feature — the spec
   * listed `perform` as work, and the work turned out to be already
   * done.
   */
  def perform[Q, A, R, F[+_]](cmd: Q)(using Asking[Q, A, R, F], At): A ! Delim + F =
    pause(cmd)

  /** the wall clock, once, remembered */
  def now[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): Long ! Delim + F =
    sys[Q, A, R, F, Long](Sys.Now):
      case SysA.Millis(v) => v

  /** a fresh id, once, remembered */
  def uuid[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): String ! Delim + F =
    sys[Q, A, R, F, String](Sys.Uuid):
      case SysA.Text(v) => v

  /** a die, once, remembered */
  def random[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): Double ! Delim + F =
    sys[Q, A, R, F, Double](Sys.Random):
      case SysA.Dice(v) => v

  /**
   * IS THIS BRANCH ON FOR THIS RUN? A dialogue started before the
   * branch existed answers `false` for ever and finishes the way it
   * began; one started after answers `true`. Both decisions live in
   * the journal, so every process agrees.
   */
  def patch[Q, A, R, F[+_]](id: String)
                           (using s: Asking[Q, A, R, F], at: At): Boolean ! Delim + F =
    sys[Q, A, R, F, Boolean](Sys.Patch(id)):
      case SysA.Flag(v) => v

  /** one library question, and the shape of answer it accepts — the
   * partial function IS the expected shape, so a driver that answers
   * a clock with a die is a loud `Mismatched` rather than a cast */
  /** the three that suspend; each is an ordinary `Sys` question that
   * the runtime declines to answer in place */
  def timer[Q, A, R, F[+_]](untilMillis: Long)
                           (using s: Asking[Q, A, R, F], at: At): Unit ! Delim + F =
    sys[Q, A, R, F, Unit](Sys.Timer(untilMillis)):
      case SysA.Elapsed => ()

  def signal[Q, A, R, F[+_]](name: String)
                            (using s: Asking[Q, A, R, F], at: At): String ! Delim + F =
    sys[Q, A, R, F, String](Sys.Signal(name)):
      case SysA.Got(v) => v

  def child[Q, A, R, F[+_]](id: String)
                           (using s: Asking[Q, A, R, F], at: At): String ! Delim + F =
    sys[Q, A, R, F, String](Sys.Child(id)):
      case SysA.Got(v) => v

  def cancelled[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At)
                               : Option[String] ! Delim + F =
    sys[Q, A, R, F, Option[String]](Sys.Cancelled):
      case SysA.Text(why) => Some(why)
      case SysA.Flag(false) => None

  private def sys[Q, A, R, F[+_], X](q: Sys)(f: PartialFunction[SysA, X])
                                    (using s: Asking[Q, A, R, F], at: At): X ! Delim + F =
    Delim.ask[Ask[Q], Ans[A], R, F](Left(q)).map:
      case Left(a) if f.isDefinedAt(a) => f(a)
      case other => throw Mismatched(q, other)

  // ── the runtime's side ───────────────────────────────────────────

  /**
   * WHAT A WORKFLOW THAT BOUNDS ITS OWN HISTORY RETURNS
   * (dialogue-continue-as, 2026-09-17): Temporal's `continueAsNew`,
   * as a RESULT rather than a call.
   *
   * Replay re-runs the program over its answers, so a dialogue with
   * ten thousand answers runs the program over ten thousand answers
   * on every cold start. Chapters cut the READING; only this cuts the
   * RUNNING. A program ends a stage with `Continue(seed)`, the
   * journal is closed and restarted with the seed as its first
   * answer, and history is bounded by the author's choice of where a
   * stage ends.
   *
   * ── WHY A RESULT AND NOT A QUESTION, which is what Temporal looks
   * like and what was tried first. A question would have to CARRY the
   * seed to the driver, and the seed is the AUTHOR's type: `Sys` is a
   * non-generic library enum and `Runtime.answer(q: Sys)` has no `A`
   * to put it in. The ways out were to smuggle the seed through an
   * untyped payload, or to give `Sys`/`Wait` a type parameter — a
   * cost every workflow pays so that the few which bound their
   * history can. The RESULT channel already carries the author's
   * types, so it costs only the programs that use it.
   *
   * ── WHY THE SEED IS AN ANSWER, not a separate thing: it is written
   * into the journal, and the journal holds answers. So the seed's
   * type is the program's own answer type, and a continued run reads
   * it exactly where a fresh run reads the oracle's reply — at its
   * FIRST question. "The first pause is the input" is the contract,
   * and it is the same contract Temporal has.
   */
  enum Next[+S, +R]:
    case Continue[S](seed: S) extends Next[S, Nothing]
    case Done[R](value: R) extends Next[Nothing, R]

  object Next:
    /** what `Worker`'s `seedOf` wants: the seed, if this result is a
     * continuation. Written out so the conventional shape needs no
     * lambda at the call site. */
    def seed[S, R](n: Next[S, R]): Option[S] = n match
      case Next.Continue(s) => Some(s)
      case Next.Done(_) => None

  /** what answers the LIBRARY's questions. The default reaches for
   * the real clock; a test hands over a scripted one and gets a
   * deterministic run without touching the program. */
  trait Runtime:
    /**
     * `Left` is the whole of workflow-suspended-driver: "I cannot
     * answer this now, and here is who can". A runtime that declines
     * has not failed — it has told the driver where to stop.
     */
    def answer(q: Sys): Either[Wait, SysA]

  object Runtime:
    /** the real world */
    given live: Runtime with
      def answer(q: Sys): Either[Wait, SysA] = q match
        case Sys.Now => Right(SysA.Millis(System.currentTimeMillis()))
        case Sys.Uuid => Right(SysA.Text(java.util.UUID.randomUUID().toString))
        case Sys.Random => Right(SysA.Dice(scala.util.Random.nextDouble()))
        case Sys.Patch(_) => Right(SysA.Flag(true))   // live: the branch is on
        case Sys.Timer(t) => Left(Wait.Until(t))
        case Sys.Signal(n) => Left(Wait.Signal(n))
        case Sys.Child(id) => Left(Wait.Child(id))
        // nobody has asked this run to stop: a bare runtime has no
        // cancel topic to consult, and the WORKER is what wraps one
        case Sys.Cancelled => Right(SysA.Flag(false))

    /** a fixed one, for a test that wants to read its own output */
    def scripted(millis: Long, id: String, dice: Double): Runtime = new Runtime:
      def answer(q: Sys): Either[Wait, SysA] = q match
        case Sys.Now => Right(SysA.Millis(millis))
        case Sys.Uuid => Right(SysA.Text(id))
        case Sys.Random => Right(SysA.Dice(dice))
        case Sys.Patch(_) => Right(SysA.Flag(true))
        case Sys.Timer(t) => Left(Wait.Until(t))
        case Sys.Signal(n) => Left(Wait.Signal(n))
        case Sys.Child(id) => Left(Wait.Child(id))
        case Sys.Cancelled => Right(SysA.Flag(false))

    /**
     * THE SAME RUNTIME, BUT THIS RUN CAN BE TOLD TO STOP. `why` is
     * read at the moment the program ASKS, not when the runtime is
     * built, so a request that arrives mid-run is seen by the next
     * check and not by the checks already behind it.
     *
     * It is a wrapper rather than a field on `Runtime` because a
     * cancellation is per-DIALOGUE and a runtime is per-worker: the
     * worker is what knows which id it is driving, and it is the
     * worker that hands the topic in (`okay.persist.Cancels`).
     */
    def cancellable(rt: Runtime)(why: => Option[String]): Runtime = new Runtime:
      def answer(q: Sys): Either[Wait, SysA] = q match
        case Sys.Cancelled => Right(why.fold(SysA.Flag(false))(SysA.Text(_)))
        case other => rt.answer(other)

  /** start a program that may ask the runtime as well as the world */
  def resumable[Q, A, R, F[+_]](body: Asks[Q, A, R, F] ?=> R ! Delim + F)
                               (using Delim.OneMachine[F], At): Paused[Q, A, R, F] ! F =
    Delim.resumable[Ask[Q], Ans[A], R, F](body(using Asks(summon)))

  /**
   * Run to the end: the LIBRARY's questions are answered by the
   * runtime and the author's by the oracle, and every answer is
   * handed back so the caller can journal it. The pair is what a
   * durable driver appends.
   */
  def drive[Q, A, R, F[+_]](p: Paused[Q, A, R, F])(oracle: Q => A ! F)
                           (using rt: Runtime, om: Delim.OneMachine[F])
                           : (Step[Q, R], Journal[A]) ! F =
    loop(p, Nil)(q => Some(oracle(q)))

  /**
   * THE WORKER'S PRIMITIVE: advance as far as the runtime alone can
   * take it, and say where it stopped. No oracle, so the author's own
   * questions come back as `Asking` for whoever is standing by — a
   * person, an HTTP call, a task queue.
   */
  def advance[Q, A, R, F[+_]](p: Paused[Q, A, R, F])
                             (using rt: Runtime, om: Delim.OneMachine[F])
                             : (Step[Q, R], Journal[A]) ! F =
    loop(p, Nil)(_ => None)

  /** one loop for both drivers: the difference is only whether the
   * author's questions have somebody to answer them */
  private def loop[Q, A, R, F[+_]](p: Paused[Q, A, R, F], acc: Journal[A])
                                  (own: Q => Option[A ! F])
                                  (using rt: Runtime, om: Delim.OneMachine[F])
                                  : (Step[Q, R], Journal[A]) ! F =
    p match
      case Delim.Paused.Done(r) => pure((Step.Done(r), acc))
      case Delim.Paused.Ask(Left(q), _, _) =>
        rt.answer(q) match
          // the runtime declined: the drive is over, and the caller
          // now knows what has to happen before it can go on
          case Left(w) => pure((Step.Waiting(w), acc))
          case Right(sa) =>
            val a: Ans[A] = Left(sa)
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a)
              .flatMap((next, _) => loop(next, acc :+ a)(own))
      case Delim.Paused.Ask(Right(q), _, _) =>
        own(q) match
          case None => pure((Step.Asking(q), acc))
          case Some(prog) => prog.flatMap: v =>
            val a: Ans[A] = Right(v)
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a)
              .flatMap((next, _) => loop(next, acc :+ a)(own))

  /**
   * Where a program stands, from its journal — and the ONE place the
   * tag earns the `Either`. A `Patch` whose decision is not in the
   * journal, on a journal that is not yet exhausted, was written by a
   * run that did not have this branch: it answers `false` and does
   * NOT eat the entry, which still answers the question it was
   * written for.
   */
  def replay[Q, A, R, F[+_]](body: Asks[Q, A, R, F] ?=> R ! Delim + F)
                            (j: Journal[A])
                            (using Delim.OneMachine[F], Replayable[Delim + F], At)
                            : Paused[Q, A, R, F] ! F =
    replaying[Q, A, R, F](body)(j).map(_._1)

  /**
   * THE SAME WALK, SAYING WHAT IT ANSWERED ON THE WAY
   * (workflow-retire, 2026-09-17).
   *
   * A journal holds ANSWERS, and the id of a `patch` lives in the
   * QUESTION — so "which branch does this `Flag(true)` belong to" is
   * not a fact about the journal at all, and nothing that reads
   * records can recover it. Only running the program pairs them up
   * again, which is why a retirement census replays.
   *
   * It is a generalisation rather than a second walk for the reason
   * `runUntil` was: the decision about `Patch` is subtle enough that
   * a copy of it would drift, and this way `replay` is one line over
   * it and the two can never disagree.
   */
  def replaying[Q, A, R, F[+_]](body: Asks[Q, A, R, F] ?=> R ! Delim + F)
                               (j: Journal[A])
                               (using Delim.OneMachine[F], Replayable[Delim + F], At)
                               : (Paused[Q, A, R, F], List[(Ask[Q], Ans[A])]) ! F =
    def go(p: Paused[Q, A, R, F], left: Journal[A], seen: List[(Ask[Q], Ans[A])])
          : (Paused[Q, A, R, F], List[(Ask[Q], Ans[A])]) ! F = p match
      case Delim.Paused.Done(_) => pure((p, seen.reverse))
      case Delim.Paused.Ask(q, _, _) =>
        (q, left) match
          case (_, Nil) => pure((p, seen.reverse))      // caught up: it is live now
          case (Left(Sys.Patch(_)), (r @ Right(_)) :: _) =>
            // the journal has no decision here, and its next entry
            // answers something else: this run predates the branch
            val _ = r
            val no: Ans[A] = Left(SysA.Flag(false))
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(no)
              .flatMap((next, _) => go(next, left, (q, no) :: seen))
          case (_, a :: rest) =>
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a)
              .flatMap((next, _) => go(next, rest, (q, a) :: seen))
    resumable[Q, A, R, F](body).flatMap(go(_, j, Nil))

  // ── the STATIC half: the same workflow as a term ──────────────────
  //
  // specs/static-workflow.md, with the factoring of specs/arrows-plan.md
  // Decision 1. Everything above is the monadic front end and stays
  // exactly as it is; what follows says that a durable program's
  // questions are a SIGNATURE, so `okay.Proc` — the free arrow, which
  // knows nothing about workflows — can carry one.

  /**
   * THE QUESTIONS A DURABLE PROCEDURE ASKS, as one signature indexed
   * by the ANSWER it expects.
   *
   * The monadic doors above spell each question twice: once as a
   * `Sys` case and once as the partial function that reads its answer
   * back (`case SysA.Millis(v) => v`). A GADT says it once — `Now`
   * IS a `Question[Q, A, Long]` — so a term built from these cannot
   * put a clock's answer where a die's belongs, and the reading that
   * used to be a `Mismatched` at run time is a type.
   *
   * `Ask` is the author's own question and the only case that
   * mentions their types; everything else is the library's, which is
   * rule 2 of the engine kept exactly: the author's `Q` never grows.
   */
  enum Question[Q, A, +R]:
    case Ask[Q, A](q: Q) extends Question[Q, A, A]
    case Now[Q, A]() extends Question[Q, A, Long]
    case Uuid[Q, A]() extends Question[Q, A, String]
    case Random[Q, A]() extends Question[Q, A, Double]
    case Patched[Q, A](id: String) extends Question[Q, A, Boolean]
    case Timer[Q, A](untilMillis: Long) extends Question[Q, A, Unit]
    case Signalled[Q, A](name: String) extends Question[Q, A, String]
    case Childed[Q, A](id: String) extends Question[Q, A, String]
    case Cancelled[Q, A]() extends Question[Q, A, Option[String]]

  /** a durable procedure: a `Proc` over the questions above */
  type Proc[Q, A, X, Y] = okay.Proc[[R] =>> Question[Q, A, R], X, Y]

  /** the signature alone, for a natural transformation's sake */
  type Asked[Q, A] = [R] =>> Question[Q, A, R]

  object Proc:
    import okay.Proc.op

    /** ask the outside world, through the author's own question type */
    def ask[Q, A, X](q: X => Q): Wf.Proc[Q, A, X, A] = asking("ask")(q)

    /**
     * THE SAME, UNDER A NAME THE PICTURE CAN SHOW (proc-form-consumer,
     * 2026-09-18). A leaf is drawn by its operation's name, and for a
     * workflow that is exactly right: `charge`, `ship`, `notify`. For a
     * FORM every leaf is the same operation — ask a field — so a term
     * of them draws as "ask, ask, ask" and the picture says nothing
     * about which field the run is standing at.
     *
     * Inside a `Proc.direct` block the name comes from the FUNCTION the
     * author called, so a form written there can name its fields by
     * writing a helper per field. That is fine when the fields are
     * code and impossible when they are DATA — a form derived from a
     * `Schema` has no place to put one def per field — which is why
     * the name is a parameter here.
     */
    def asking[Q, A, X](name: String)(q: X => Q): Wf.Proc[Q, A, X, A] =
      op(name)(x => Question.Ask(q(x)))

    /** the same under the name the literature uses */
    def perform[Q, A, X](cmd: X => Q): Wf.Proc[Q, A, X, A] = ask(cmd)

    def now[Q, A, X]: Wf.Proc[Q, A, X, Long] = op("now")(_ => Question.Now())
    def uuid[Q, A, X]: Wf.Proc[Q, A, X, String] = op("uuid")(_ => Question.Uuid())
    def random[Q, A, X]: Wf.Proc[Q, A, X, Double] = op("random")(_ => Question.Random())

    def patch[Q, A, X](id: String): Wf.Proc[Q, A, X, Boolean] =
      op(s"patch:$id")(_ => Question.Patched(id))

    def timer[Q, A]: Wf.Proc[Q, A, Long, Unit] =
      op("timer")((t: Long) => Question.Timer(t))

    /**
     * SLEEP, DURABLY — and the deadline is JOURNALLED, not computed
     * on the fly, exactly as the monadic `sleep` is: `now` is a leaf
     * whose answer is written down, so every later process wakes at
     * the instant the FIRST one chose. A sleep relative to the
     * reading process's clock slides its deadline forward for ever on
     * every restart, which is the durable-timer bug every engine has
     * had once.
     *
     * Written with the arrow's own combinators, which is the point:
     * `now >>> arr(_ + millis) >>> timer`.
     */
    def sleep[Q, A, X](millis: Long): Wf.Proc[Q, A, X, Unit] =
      val A = okay.Proc.procArrow[Asked[Q, A]]
      A.compose(timer[Q, A], A.compose(A.arr((t: Long) => t + millis), now[Q, A, X]))

    def awaitSignal[Q, A, X](name: String): Wf.Proc[Q, A, X, String] =
      op(s"signal:$name")(_ => Question.Signalled(name))

    def awaitChild[Q, A, X](id: String): Wf.Proc[Q, A, X, String] =
      op(s"child:$id")(_ => Question.Childed(id))

    def cancelled[Q, A, X]: Wf.Proc[Q, A, X, Option[String]] =
      op("cancelled")(_ => Question.Cancelled())

    /**
     * THE BRIDGE, and it is the whole of why stage 1 is small: a term
     * becomes an ordinary durable program, so `Dialogue.workflow`,
     * `Worker`, timers, signals, children, retries, cancellation and
     * `continueAs` all work on day one with nothing changed in any of
     * them.
     *
     * Its row is `Delim + Pure`, which is `Replayable` by
     * construction — the discipline stage 1 of the spec makes a type
     * has nothing left to police here, because a term's only effects
     * ARE its leaves.
     */
    def program[Q, A, R, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X)
                              (using w: Asks[Q, A, R, Pure], at: At): Y ! Delim + Pure =
      p.foldMap[[Z] =>> Z ! Delim + Pure](
        [Z] => (q: Question[Q, A, Z]) => answerOf[Q, A, R, Z](q))(x)

    /**
     * ONE QUESTION, AS THE MONADIC DOOR THAT ALREADY EXISTS — and
     * every arm goes through `up`, which is the one place this
     * encoding costs anything.
     *
     * `Question` must be COVARIANT in its answer for `okay.Proc`,
     * whose signature parameter is `F[+_]`, to accept it at all. So
     * matching `Ask` proves `A <: Z` rather than `A = Z` — and since
     * free-answer-variance (2026-09-23) a program `A ! Row` IS a
     * `Z ! Row`, `Free` being covariant in its answer: no node, no
     * cast, the compiler still checks every arm. Until then each leaf
     * paid an `up` = `map(v => v)` for exactly this widening.
     */
    private def answerOf[Q, A, R, Z](q: Question[Q, A, Z])
                                    (using w: Asks[Q, A, R, Pure], at: At): Z ! Delim + Pure =
      q match
        case Question.Ask(a) => w.pause(a)
        case Question.Now() => w.now
        case Question.Uuid() => w.uuid
        case Question.Random() => w.random
        case Question.Patched(id) => w.patch(id)
        case Question.Timer(t) => Wf.timer[Q, A, R, Pure](t)(using w.in, at)
        case Question.Signalled(n) => w.awaitSignal(n)
        case Question.Childed(i) => w.awaitChild(i)
        case Question.Cancelled() => w.cancelled

    /**
     * WHERE THE PROCEDURE STANDS, DERIVED FROM THE TERM — the second
     * of the two readings of a position, and the one a monadic
     * program cannot have.
     *
     * `Wf.replay` re-derives a place by RUNNING the program over its
     * answers; this folds the TERM over them and performs nothing at
     * all. Its signature is the proof, the way `Wf.replay` taking no
     * `Runtime` is: there is no row, no monad and no runtime in it,
     * so a deploy check can ask "does this journal still fit this
     * program" of ten thousand runs without starting one.
     *
     * The two must agree, and the property that says so is the
     * keystone of the whole arc: `walk` is what the deploy check and
     * the picture trust, `replay` is what the engine trusts, and a
     * disagreement is a bug found before a journal is.
     */
    def walk[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X, journal: Journal[A])
                        : Either[Stranded, Standing[Q, A, Y]] =
      go(p, x, journal, 0, okay.Proc.Path.root, fresh) match
        case Walked.Ran(y, _, _) => Right(Standing.Done(y))
        // ONE question is still an `Asking`, which is what keeps this
        // additive: a term with no `Par` in it answers exactly what it
        // answered before the node existed, and every consumer that
        // matches on `Asking` is untouched
        case Walked.Asking(on, used) =>
          if on.sizeIs == 1 then Right(Standing.Asking(on.head._1, on.head._2, used))
          else Right(Standing.Waiting(on, used))
        case Walked.Bad(at, rec, why) => Left(Stranded(at, rec, why))

    /**
     * A QUESTION IN THE JOURNAL'S OWN SPELLING. The GADT is what the
     * TERM is built from; `Ask[Q] = Either[Sys, Q]` is what the
     * journal and the monadic driver speak. One function translates,
     * so "the static and the monadic front end ask the same thing"
     * is a comparison anybody can make — which is exactly what the
     * property tying `walk` to `Wf.replay` needs.
     */
    def tag[Q, A](q: Question[Q, A, ?]): Ask[Q] = q match
      case Question.Ask(a) => Right(a)
      case Question.Now() => Left(Sys.Now)
      case Question.Uuid() => Left(Sys.Uuid)
      case Question.Random() => Left(Sys.Random)
      case Question.Patched(id) => Left(Sys.Patch(id))
      case Question.Timer(t) => Left(Sys.Timer(t))
      case Question.Signalled(n) => Left(Sys.Signal(n))
      case Question.Childed(i) => Left(Sys.Child(i))
      case Question.Cancelled() => Left(Sys.Cancelled)

    /**
     * WHAT THIS RUN WOULD HAVE TO UNDO, AS A TERM
     * (specs/static-workflow.md, stage 5).
     *
     * Walk the term over the journal exactly as `walk` does, and
     * every `Undo` node whose step COMPLETED contributes one piece:
     * its compensation, fed the input and output that step actually
     * had. The pieces come back in REVERSE — the last thing done is
     * the first thing undone, which is what a saga means — composed
     * into one ordinary `Wf.Proc`.
     *
     * A TERM, and that is the feature rather than a spelling. The
     * compensation runs on the same engine, writes to the same
     * journal, draws itself, and — if the compensation is itself
     * interrupted halfway — resumes from its own position like any
     * other durable run. `okay.persist.Saga` does this for a LINEAR
     * sequence with a journal of its own; this does it for a SHAPE,
     * because branches and loops are nodes and a walk goes through
     * them.
     *
     * It decides nothing about WHEN. A term that can fail threads
     * `Either[E, ·]` and `OnRight` passes a `Left` through untouched,
     * so a failure short-circuits the rest by the ordinary choice;
     * whether that is a reason to compensate is the author's, and
     * they call this when they have decided it is.
     *
     * A run with no `Undo` nodes, or one that has not reached any,
     * answers the identity — a term that asks nothing, which the
     * engine runs to `()` without touching the journal.
     */
    def compensating[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X, journal: Journal[A])
                                : Wf.Proc[Q, A, Unit, Unit] =
      val buf = fresh[Q, A]
      val _ = go(p, x, journal, 0, okay.Proc.Path.root, buf)
      buf.reverseIterator.reduceOption(okay.Proc.andThen)
        .getOrElse(okay.Proc.arr[Wf.Asked[Q, A], Unit, Unit](identity))

    /** the deploy check: this journal still fits this program */
    def accepts[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X, journal: Journal[A]): Boolean =
      walk(p)(x, journal).isRight

    /**
     * THE DEPLOY CHECK OVER A WHOLE TOPIC: which live runs would this
     * term STRAND, and where.
     *
     * It is a pure function, and that is the feature rather than an
     * implementation note — `walk` performs nothing, so this asks ten
     * thousand journals whether they still fit the code you are about
     * to ship WITHOUT starting one of them, before the deploy rather
     * than during it. `Retire.states` cannot do that: it replays, so
     * it needs a row and a runtime and it costs a run apiece.
     *
     * A run the term ACCEPTS is not in the answer. What comes back is
     * the stranding and where it happens, which is what an operator
     * has to decide from: a `patch` for the runs that predate the
     * change, or a wait for them to drain.
     *
     * The journals come from the caller — `okay.persist.Dialogue`'s
     * `recovered.answers` is the usual source — for the reason
     * `Retire.patches` takes them too: a tool over data, not a second
     * way to open a dialogue.
     */
    def strands[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X)
                           (journals: List[(String, Journal[A])])
                           : Map[String, Stranded] =
      journals.foldLeft(Map.empty[String, Stranded]): (acc, idAndJ) =>
        val (id, j) = idAndJ
        walk(p)(x, j) match
          case Left(bad) => acc + (id -> bad)
          case Right(_) => acc

    /** where a fold of the term over a journal ended */
    enum Standing[Q, A, +Y]:
      case Done[Q, A, Y](value: Y) extends Standing[Q, A, Y]
      /** waiting, at this path, on this question, with this many
       * records of the journal accepted */
      case Asking[Q, A](at: okay.Proc.Path, q: Question[Q, A, ?], accepted: Int)
        extends Standing[Q, A, Nothing]
      /**
       * WAITING ON MORE THAN ONE QUESTION AT ONCE — a `Par` whose
       * branches are both outstanding (specs/static-workflow.md,
       * stage 5).
       *
       * `on` is in the order the journal will record the answers,
       * which is term order: its HEAD is the question the engine will
       * consume next and the one `Wf.replay` reports, and the rest
       * are questions a front end may put to the world today rather
       * than after the head comes back. That ordering is the whole
       * contract — an answer to the second cannot be committed before
       * an answer to the first, because a record says nothing about
       * which question it answers.
       */
      case Waiting[Q, A](on: Vector[(okay.Proc.Path, Question[Q, A, ?])], accepted: Int)
        extends Standing[Q, A, Nothing]

      /** every question this run is waiting on: none when it is done,
       * one for an `Asking`, two or more for a `Par` — so a caller
       * that wants "what is outstanding" need not know which */
      def pending: Vector[(okay.Proc.Path, Question[Q, A, ?])] = this match
        case Done(_) => Vector.empty
        case Asking(at, q, _) => Vector((at, q))
        case Waiting(on, _) => on

    /** the journal does not fit the term, and where */
    final case class Stranded(at: okay.Proc.Path, record: Int, why: String):
      override def toString = s"stranded at ${at.show} on record $record: $why"

    /** where the fold puts the compensations it passed */
    private type Undos[Q, A] = scala.collection.mutable.ArrayBuffer[Wf.Proc[Q, A, Unit, Unit]]
    private def fresh[Q, A]: Undos[Q, A] = scala.collection.mutable.ArrayBuffer.empty

    private enum Walked[Q, A, +V]:
      case Ran[Q, A, V](value: V, left: Journal[A], used: Int) extends Walked[Q, A, V]
      /**
       * WAITING — on one question, or on SEVERAL when a `Par` has
       * branches outstanding.
       *
       * A vector rather than a single pair, so that every arm below
       * that forwards a stop (`case stop: Walked.Asking => stop`)
       * stayed exactly as it was when the `Par` case arrived: the
       * shape of a stop did not change, only how much it can carry.
       * They are in the order the journal will record their answers.
       */
      case Asking[Q, A](on: Vector[(okay.Proc.Path, Question[Q, A, ?])], used: Int)
        extends Walked[Q, A, Nothing]
      case Bad[Q, A](at: okay.Proc.Path, record: Int, why: String) extends Walked[Q, A, Nothing]

    import okay.Proc.Path./

    /**
     * THE FOLD, WITH ONE PLACE TO PUT WHAT IT PASSED
     * — `undos` collects a compensation term for every `Undo` node
     * whose step COMPLETED, in the order they ran.
     *
     * A parameter rather than a second fold: `walk` hands it a buffer
     * it then ignores, and `compensating` hands it one it reads. The
     * alternative was a mirror of this function that only collects,
     * and a second copy of a fold with the `Patch` rule in it is
     * exactly the drift this file warns about elsewhere.
     */
    private def go[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y], x: X, j: Journal[A],
                               used: Int, at: okay.Proc.Path,
                               undos: Undos[Q, A]): Walked[Q, A, Y] =
      p match
        case okay.Proc.Arr(f) => Walked.Ran(f(x), j, used)

        case okay.Proc.Op(_, run) =>
          val q = run(x)
          j match
            // caught up with the journal: this is where the run stands
            case Nil => Walked.Asking(Vector((at, q)), used)
            case (r @ Right(_)) :: _ if isPatch(q) =>
              // THE NON-CONSUMING RULE, and it is the one subtle
              // thing in this fold. The journal has no decision for
              // this patch and its next record answers something
              // else, so the run that wrote it PREDATES the branch:
              // the answer is `false` and the record is NOT eaten —
              // it still answers the question it was written for.
              // Copied from `Wf.replaying` in shape and asserted
              // against it by property, never re-derived.
              val _ = r
              readInto(q, Left(SysA.Flag(false))) match
                case Left(why) => Walked.Bad(at, used, why)
                case Right(v) => Walked.Ran(v, j, used)
            case a :: rest =>
              readInto(q, a) match
                case Left(why) => Walked.Bad(at, used, why)
                case Right(v) => Walked.Ran(v, rest, used + 1)

        case okay.Proc.Then(f, g) =>
          go(f, x, j, used, at / okay.Proc.Step.Fst, undos) match
            case Walked.Ran(v, left, u) => go(g, v, left, u, at / okay.Proc.Step.Snd, undos)
            case stop: Walked.Asking[Q, A] => stop
            case stop: Walked.Bad[Q, A] => stop

        case okay.Proc.First(f) =>
          go(f, x._1, j, used, at / okay.Proc.Step.In, undos) match
            case Walked.Ran(v, left, u) => Walked.Ran((v, x._2), left, u)
            case stop: Walked.Asking[Q, A] => stop
            case stop: Walked.Bad[Q, A] => stop

        case okay.Proc.OnRight(f) =>
          x match
            case Left(c) => Walked.Ran(Left(c), j, used)
            case Right(a) =>
              go(f, a, j, used, at / okay.Proc.Step.In, undos) match
                case Walked.Ran(v, left, u) => Walked.Ran(Right(v), left, u)
                case stop: Walked.Asking[Q, A] => stop
                case stop: Walked.Bad[Q, A] => stop

        case okay.Proc.Undo(step, undo) =>
          go(step, x, j, used, at / okay.Proc.Step.Back(false), undos) match
            case Walked.Ran(y, left, u) =>
              // BUILT HERE, WHERE THE TYPES ARE STILL KNOWN. `x` and
              // `y` are this node's own input and output, so the piece
              // needs no cast — which is the whole reason the
              // collection happens inside the fold rather than over a
              // list of paths afterwards.
              undos += okay.Proc.andThen(okay.Proc.Arr((_: Unit) => (x, y)), undo)
              Walked.Ran(y, left, u)
            case stop: Walked.Asking[Q, A] => stop
            case stop: Walked.Bad[Q, A] => stop

        case okay.Proc.Par(f, g) =>
          go(f, x, j, used, at / okay.Proc.Step.Side(0), undos) match
            case Walked.Ran(y, left, u) =>
              go(g, x, left, u, at / okay.Proc.Step.Side(1), undos) match
                case Walked.Ran(z, l2, u2) => Walked.Ran((y, z), l2, u2)
                case stop: Walked.Asking[Q, A] => stop
                case stop: Walked.Bad[Q, A] => stop
            case waiting @ Walked.Asking(on, u) =>
              // THE LEFT BRANCH IS WAITING, SO THE JOURNAL IS EMPTY —
              // `Asking` is produced in exactly one place, the `Op`
              // case's `Nil` arm. So the right branch starts from an
              // empty journal too, and its own first question is
              // knowable here without a second pass over anything.
              // A THROWAWAY BUFFER. This walk is SPECULATIVE — it asks
              // what the right branch would ask, over a journal that
              // has not reached it — so anything it "completes" has
              // not happened, and a compensation collected here would
              // undo a step nobody took.
              go(g, x, Nil, u, at / okay.Proc.Step.Side(1), fresh) match
                case Walked.Asking(more, _) => Walked.Asking(on ++ more, u)
                // the right branch asks nothing: there is one question
                // outstanding and this is an ordinary wait
                case _: Walked.Ran[Q, A, ?] => waiting
                case stop: Walked.Bad[Q, A] => stop
            case stop: Walked.Bad[Q, A] => stop

        case okay.Proc.Iter(body) =>
          def loop(cur: X, left: Journal[A], u: Int, round: Int): Walked[Q, A, Y] =
            go(body, cur, left, u, at / okay.Proc.Step.Round(round), undos) match
              case Walked.Ran(Left(again), l2, u2) => loop(again, l2, u2, round + 1)
              case Walked.Ran(Right(y), l2, u2) => Walked.Ran(y, l2, u2)
              case stop: Walked.Asking[Q, A] => stop
              case stop: Walked.Bad[Q, A] => stop
          loop(x, j, used, 0)

    private def isPatch[Q, A](q: Question[Q, A, ?]): Boolean = q match
      case Question.Patched(_) => true
      case _ => false

    /**
     * ONE ANSWER, READ BACK AT THE QUESTION'S OWN TYPE — the GADT
     * paying for itself. The monadic doors each carry a partial
     * function for this and throw `Mismatched` when it does not
     * match; here the match is total over the pairs that make sense
     * and every other pair is DATA (a `Stranded`), because a fold
     * that throws cannot be a deploy check.
     */
    private def readInto[Q, A, Z](q: Question[Q, A, Z], a: Ans[A]): Either[String, Z] =
      def no = Left(s"$q cannot take $a")
      q match
        case Question.Ask(_) => a match
          case Right(v) => Right(v)
          case _ => no
        case Question.Now() => a match
          case Left(SysA.Millis(v)) => Right(v)
          case _ => no
        case Question.Uuid() => a match
          case Left(SysA.Text(v)) => Right(v)
          case _ => no
        case Question.Random() => a match
          case Left(SysA.Dice(v)) => Right(v)
          case _ => no
        case Question.Patched(_) => a match
          case Left(SysA.Flag(v)) => Right(v)
          case _ => no
        case Question.Timer(_) => a match
          case Left(SysA.Elapsed) => Right(())
          case _ => no
        case Question.Signalled(_) => a match
          case Left(SysA.Got(v)) => Right(v)
          case _ => no
        case Question.Childed(_) => a match
          case Left(SysA.Got(v)) => Right(v)
          case _ => no
        case Question.Cancelled() => a match
          case Left(SysA.Text(why)) => Right(Some(why))
          case Left(SysA.Flag(false)) => Right(None)
          case _ => no
