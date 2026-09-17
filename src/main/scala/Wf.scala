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
    def pause(q: Q)(using At): A ! (Delim + F) =
      Wf.pause[Q, A, R, F](q)(using in, summon[At])

    /** the same under the name the literature uses: an "activity" is
     * a command performed outside and a result remembered */
    def perform(cmd: Q)(using At): A ! (Delim + F) = pause(cmd)

    /** the wall clock, once, remembered */
    def now(using At): Long ! (Delim + F) = Wf.now[Q, A, R, F](using in, summon[At])

    /** a fresh id, once, remembered */
    def uuid(using At): String ! (Delim + F) = Wf.uuid[Q, A, R, F](using in, summon[At])

    /** a die, once, remembered */
    def random(using At): Double ! (Delim + F) = Wf.random[Q, A, R, F](using in, summon[At])

    /** is this branch on for THIS run? */
    def patch(id: String)(using At): Boolean ! (Delim + F) =
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
    def cancelled(using At): Option[String] ! (Delim + F) =
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
    def sleep(millis: Long)(using At): Unit ! (Delim + F) =
      now.flatMap(t => Wf.timer[Q, A, R, F](t + millis)(using in, summon[At]))

    /** wait for a named signal from outside; the payload is its value */
    def awaitSignal(name: String)(using At): String ! (Delim + F) =
      Wf.signal[Q, A, R, F](name)(using in, summon[At])

    /** wait for a child dialogue to finish, and take its answer */
    def awaitChild(id: String)(using At): String ! (Delim + F) =
      Wf.child[Q, A, R, F](id)(using in, summon[At])

  // ── the author's doors ───────────────────────────────────────────

  /**
   * Ask the outside world, through the author's own question type —
   * `Delim.pause` with the tag put on and taken off.
   */
  def pause[Q, A, R, F[+_]](q: Q)(using s: Asking[Q, A, R, F], at: At): A ! (Delim + F) =
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
  def perform[Q, A, R, F[+_]](cmd: Q)(using Asking[Q, A, R, F], At): A ! (Delim + F) =
    pause(cmd)

  /** the wall clock, once, remembered */
  def now[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): Long ! (Delim + F) =
    sys[Q, A, R, F, Long](Sys.Now):
      case SysA.Millis(v) => v

  /** a fresh id, once, remembered */
  def uuid[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): String ! (Delim + F) =
    sys[Q, A, R, F, String](Sys.Uuid):
      case SysA.Text(v) => v

  /** a die, once, remembered */
  def random[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At): Double ! (Delim + F) =
    sys[Q, A, R, F, Double](Sys.Random):
      case SysA.Dice(v) => v

  /**
   * IS THIS BRANCH ON FOR THIS RUN? A dialogue started before the
   * branch existed answers `false` for ever and finishes the way it
   * began; one started after answers `true`. Both decisions live in
   * the journal, so every process agrees.
   */
  def patch[Q, A, R, F[+_]](id: String)
                           (using s: Asking[Q, A, R, F], at: At): Boolean ! (Delim + F) =
    sys[Q, A, R, F, Boolean](Sys.Patch(id)):
      case SysA.Flag(v) => v

  /** one library question, and the shape of answer it accepts — the
   * partial function IS the expected shape, so a driver that answers
   * a clock with a die is a loud `Mismatched` rather than a cast */
  /** the three that suspend; each is an ordinary `Sys` question that
   * the runtime declines to answer in place */
  def timer[Q, A, R, F[+_]](untilMillis: Long)
                           (using s: Asking[Q, A, R, F], at: At): Unit ! (Delim + F) =
    sys[Q, A, R, F, Unit](Sys.Timer(untilMillis)):
      case SysA.Elapsed => ()

  def signal[Q, A, R, F[+_]](name: String)
                            (using s: Asking[Q, A, R, F], at: At): String ! (Delim + F) =
    sys[Q, A, R, F, String](Sys.Signal(name)):
      case SysA.Got(v) => v

  def child[Q, A, R, F[+_]](id: String)
                           (using s: Asking[Q, A, R, F], at: At): String ! (Delim + F) =
    sys[Q, A, R, F, String](Sys.Child(id)):
      case SysA.Got(v) => v

  def cancelled[Q, A, R, F[+_]](using s: Asking[Q, A, R, F], at: At)
                               : Option[String] ! (Delim + F) =
    sys[Q, A, R, F, Option[String]](Sys.Cancelled):
      case SysA.Text(why) => Some(why)
      case SysA.Flag(false) => None

  private def sys[Q, A, R, F[+_], X](q: Sys)(f: PartialFunction[SysA, X])
                                    (using s: Asking[Q, A, R, F], at: At): X ! (Delim + F) =
    Delim.ask[Ask[Q], Ans[A], R, F](Left(q)).map:
      case Left(a) if f.isDefinedAt(a) => f(a)
      case other => throw Mismatched(q, other)

  // ── the runtime's side ───────────────────────────────────────────

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
  def resumable[Q, A, R, F[+_]](body: Asks[Q, A, R, F] ?=> R ! (Delim + F))
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
  def replay[Q, A, R, F[+_]](body: Asks[Q, A, R, F] ?=> R ! (Delim + F))
                            (j: Journal[A])
                            (using Delim.OneMachine[F], Replayable[Delim + F], At)
                            : Paused[Q, A, R, F] ! F =
    def go(p: Paused[Q, A, R, F], left: Journal[A]): Paused[Q, A, R, F] ! F = p match
      case Delim.Paused.Done(_) => pure(p)
      case Delim.Paused.Ask(q, _, _) =>
        (q, left) match
          case (_, Nil) => pure(p)                      // caught up: it is live now
          case (Left(Sys.Patch(_)), (r @ Right(_)) :: _) =>
            // the journal has no decision here, and its next entry
            // answers something else: this run predates the branch
            val _ = r
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(Left(SysA.Flag(false)))
              .flatMap((next, _) => go(next, left))
          case (_, a :: rest) =>
            Delim.answer[Ask[Q], Ans[A], R, F](p, Nil)(a).flatMap((next, _) => go(next, rest))
    resumable[Q, A, R, F](body).flatMap(go(_, j))
