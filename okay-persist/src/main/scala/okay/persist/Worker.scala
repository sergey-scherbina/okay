package okay.persist

import okay.{!, +, At, Delim, Replayable, RowLift, Wf, pure}
import okay.RowLift.up
import okay.codec.Schema

/**
 * THE THING THAT CARRIES WORKFLOWS FORWARD (workflow-worker,
 * 2026-09-17).
 *
 * `Wf` says where a run stands and `Timers` says when to come back;
 * a worker is what turns those into progress. It is built for ONE
 * workflow type — one journal topic, one program name, one body — and
 * it moves ids of that type:
 *
 *   start(id)        a new run, driven as far as it goes
 *   advance(id)      an existing one, driven as far as it goes
 *   tick(now)        every run whose deadline has passed
 *
 * Each of those ends by telling the timers what it learned: a run
 * that is sleeping is armed, a run that finished or is waiting on
 * something else is disarmed. That is the whole loop.
 *
 * ── WHAT MAKES IT SAFE TO KILL A WORKER, which is the architecture's
 * third rule. Every move it makes is either idempotent or guarded by
 * `expect`: an answer it appends carries the position it expected, so
 * a second worker's answer for the same position is REJECTED by the
 * fold rather than accepted twice. Two workers on one dialogue
 * therefore produce one journal, not a corrupted one. A lease
 * (`Leases`) makes that rare; it is not what makes it correct, and
 * the distinction matters because a lease can always be lost.
 *
 * ── THE ONE CHECK THAT IS NOT OPTIONAL. A due timer is a HINT, not
 * an instruction: the worker looks at where the dialogue actually
 * stands before it appends anything, and injects `Elapsed` only when
 * the run is genuinely waiting on a timer whose instant has passed.
 * A stale timer record — one for a run that has moved on, or been
 * cancelled, or been woken by a signal — then costs a read and
 * nothing else. Without that check, an old record in an operational
 * topic could put an answer into a journal that nobody asked for,
 * which is the one thing the journal must never contain.
 */
final class Worker[Q, A, R, F[+_], G[+_]](topic: Topic, program: String, timers: Timers,
                                   /**
                                    * THE ORACLE, AND THE KEY IT MAY ASK FOR
                                    * (worker-oracle-attempt, 2026-09-17).
                                    * `Dialogue.Attempt(id, index)` is stable across
                                    * restarts because it is the journal's own
                                    * position -- exactly what a non-idempotent
                                    * external call needs. It arrives as CONTEXT, so
                                    * an oracle that does not need it is written
                                    * exactly as before.
                                    */
                                   oracle: Q => Dialogue.Attempt ?=> A ! G,
                                   snapshots: Option[Snapshots] = None,
                                   snapshotEvery: Int = 0,
                                   signals: Option[Signals] = None,
                                   statuses: Option[Statuses] = None,
                                   cancels: Option[Cancels] = None,
                                   /**
                                    * HOW A RESULT SAYS "THIS IS NOT THE END"
                                    * (dialogue-continue-as, 2026-09-17). A program
                                    * that bounds its own history returns a seed
                                    * instead of a value; this is how the worker
                                    * reads one out of the author's own result type.
                                    * `Wf.Next.seed` is the conventional shape and
                                    * needs no lambda. The default says no program
                                    * ever continues, which is what every workflow
                                    * written before this lane meant.
                                    */
                                   seedOf: R => Option[A] = (_: R) => None,
                                   /** how many chapters one drive will run through
                                    * before handing back. A program that continues
                                    * without ever pausing would otherwise spin in
                                    * here for ever; this turns that into a value the
                                    * caller can see. */
                                   continuations: Int = 64,
                                   /**
                                    * WHERE A FINISHED RUN LEAVES ITS RESULT, and
                                    * where a waiting parent reads one
                                    * (workflow-children, 2026-09-17). A worker given
                                    * one records EVERY run it finishes, which is
                                    * what makes any of them adoptable as a child --
                                    * a run does not need to know it has a parent.
                                    */
                                   children: Option[Children] = None,
                                   /** how a result becomes the string `SysA.Got`
                                    * carries. `toString` is the default because most
                                    * results are already text; a structured one
                                    * encodes, exactly as an activity's answer does. */
                                   resultText: R => String = (r: R) => r.toString,
                                   /**
                                    * AN ADVISORY LEASE (workflow-lease,
                                    * 2026-09-17). `expect` is what makes two
                                    * workers SAFE; this only makes them rare. A
                                    * worker that finds the lease held by somebody
                                    * else reports `Busy` and drives nothing --
                                    * which saves the wasted attempt, and is worth
                                    * nothing to rely on. See `Leases`.
                                    */
                                   leases: Option[Leases] = None,
                                   /** who this worker says it is, when it takes a
                                    * lease. Two workers sharing a name cannot tell
                                    * each other apart and will not exclude. */
                                   owner: String = "worker",
                                   leaseMillis: Long = 30_000L,
                                   /** wall time, for the lease only: the PROGRAM's
                                    * clock is `Wf.Runtime`, and it is journalled.
                                    * These two must not be confused -- one is
                                    * operational, the other is state. */
                                   clock: () => Long = () => System.currentTimeMillis(),
                                   /**
                                    * KEEPING THE PROGRAM BETWEEN CALLS
                                    * (dialogue-resume-cache, 2026-09-17). Without
                                    * one, every advance replays the journal; with
                                    * one, n answers to a dialogue this process is
                                    * holding cost one replay and n steps. It is a
                                    * per-process optimisation over a journal that
                                    * stays the only state -- drop it, restart, or
                                    * run two, and nothing changes but how often a
                                    * replay happens. See `Resume`.
                                    */
                                   resume: Option[Resume[Wf.Ask[Q], Wf.Ans[A], R, F]] = None,
                                   /**
                                    * HOW TO KEEP ONE RUN'S FAILURE FROM TAKING THE
                                    * BATCH (worker-tick-isolation, 2026-09-17).
                                    *
                                    * An oracle whose retries are exhausted THROWS,
                                    * by design: nothing is journalled and the run
                                    * still stands, so a later worker asks again.
                                    * For `advance` that throw belongs to the caller,
                                    * who asked about one run. For `tick` it ends the
                                    * pass, skips every run after the failing one and
                                    * DISCARDS what the earlier ones did.
                                    *
                                    * Catching it needs the ROW, and `G` is abstract
                                    * here -- so the ability is carried as a
                                    * parameter rather than searched for, which is
                                    * the same rule `Delim.answer` follows and the
                                    * one ProbeRowCrash states. `Worker.isolating`
                                    * is the instance for `Async`. Left out, `tick`
                                    * behaves exactly as before.
                                    */
                                   isolate: Option[Worker.Isolate[G]] = None)
                                  (body: Wf.Asks[Q, A, R, F] ?=> R ! Delim + F)
                                  (using Schema[Wf.Ans[A]], Replayable[Delim + F],
                                   Delim.OneMachine[F], At, Wf.Runtime,
                                   RowLift.Sub[F, G]):

  /** the dialogue this worker drives, for an id */
  def dialogue(id: String): Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F] =
    Dialogue.workflow[Q, A, R, F](topic, id, program, snapshots, snapshotEvery)(body)

  /**
   * THE RUNTIME THIS WORKER ANSWERS WITH, FOR ONE ID. The ambient
   * one, wrapped so that `w.cancelled` consults the cancel topic. It
   * is built per call and not per worker because `requested` is read
   * at the moment the program asks: a request that arrives mid-drive
   * is seen by the next check and not by the ones already behind it.
   */
  private def runtime(id: String): Wf.Runtime =
    cancels match
      case None => summon[Wf.Runtime]
      case Some(c) => Wf.Runtime.cancellable(summon[Wf.Runtime])(c.requested(id))

  /**
   * ASK A RUN TO STOP. Cooperative, and the header of `Cancels` says
   * why it has to be: the run sees this at its next `w.cancelled`,
   * and a program with no check is not cancellable. `false` means
   * this worker was built without a cancel topic, so there was
   * nowhere to put the request — a refusal rather than a silent one.
   */
  def cancel(id: String, why: String): Boolean = cancels match
    case Some(c) => c.cancel(id, why); true
    case None => false

  /** a new run, or an existing one: the journal decides which, so
   * these are the same call and `start` is only a name */
  def start(id: String): Worker.Progress[R] ! G = advance(id)

  /**
   * Drive one run as far as it goes, then tell the timers what
   * happened. Everything it answered on the way is already durable
   * before this returns.
   */
  def advance(id: String): Worker.Progress[R] ! G = leases match
    case Some(ls) if !ls.acquire(id, owner, clock() + leaseMillis, clock()) =>
      // somebody else is on it. Nothing is enforced by this: if the
      // check is wrong, `expect` still gives one journal -- so the
      // only thing lost by a wrong answer here is a wasted drive.
      pure(Worker.Progress.Busy(ls.held(id, clock()).map(_.owner).getOrElse("unknown")))
    case _ =>
      step(id).flatMap: p =>
        note(id, p).map: _ =>
          // released whatever the outcome: a run that is sleeping or
          // waiting is not being worked on, and holding its lease
          // would only delay whoever wakes it
          leases.foreach(_.release(id, owner, clock()))
          p

  /** tell the index what was learned, if anybody is keeping one. It
   * is written AFTER the journal, never instead of it: a status is
   * something to look at, never something to decide from, so a worker
   * that dies between the two leaves a stale line and no wrong run. */
  private def note(id: String, p: Worker.Progress[R]): Unit ! G =
    def write(asking: Option[String], where: Option[String]): Unit =
      statuses.foreach(_.put(Statuses.Status(id, program, Worker.state(p),
        asking, where, System.currentTimeMillis())))
    statuses match
      case None => pure(())
      case Some(_) => p match
        // THE STATUS WRITE IS A DOOR TOO (statuses-verdicts,
        // 2026-09-17): asking what a run is standing at RE-RUNS the
        // program, so a bad deploy threw out of the write that was
        // supposed to record the verdict.
        //
        // These four have no live place to describe anyway — there is
        // nothing to ask of a finished run, and nothing to rebuild for
        // one whose rebuild is the problem — so the look is skipped,
        // which is also one fold less per advance.
        case Worker.Progress.Finished(_) | Worker.Progress.Broken(_)
           | Worker.Progress.Incompatible(_) | Worker.Progress.Failed(_) =>
          pure(write(None, None))
        // ...and the rest goes through `placed` like every other
        // rebuild (note-through-placed, 2026-09-17). Reaching here
        // means the drive JUST built this place, so the rebuild cannot
        // refuse — but that is an argument, and three of the four
        // doors were closed after an argument like it turned out to be
        // about the wrong door. One rule, one implementation, nothing
        // left to re-derive.
        case _ => placed(dialogue(id)).map:
          case Right((at, _)) => write(at.asking.map(_.toString), at.where)
          case Left(_) => write(None, None)

  /**
   * A STOP, WITH A LINE IN IT (delim-diagnostics-position,
   * 2026-09-17). `Stopped` names an offset; `diagnosis` replays the
   * part of the journal this program DID accept and adds where this
   * program stands, so a bad deploy points at code. It costs a second
   * fold, on the one path where nothing else is going to happen
   * anyway.
   */
  private def broken(d: Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F],
                     stopped: Dialogue.Stopped): Worker.Progress[R] ! G =
    d.diagnosis.up[G].map: found =>
      Worker.Progress.Broken(
        found.getOrElse(Dialogue.Diagnosis(stopped, 0, None, None)))

  private def step(id: String): Worker.Progress[R] ! G =
    resume.flatMap(_.get(id)) match
      // A CACHE HIT SKIPS THE LOOK (dialogue-resume-cache): the
      // journal folded when this program was cached and `undisturbed`
      // says nothing has been written since, so it cannot have become
      // unreadable in between. The check below is for the cold path,
      // which is the only path that folds.
      case Some(h) => driving(id, 0, h)
      case None =>
        // LOOK BEFORE DRIVING (workflow-docs, 2026-09-17). A journal
        // the fold cannot read — damage, or a record from another
        // program — makes the DRIVER throw, because a driver has
        // nowhere to put that answer. A worker does: `Progress.Broken`.
        // Without this the throw takes the whole `tick` with it, and
        // one unreadable run stops every other run on the box, which
        // is the opposite of what a worker loop is for.
        //
        // `standing` is one fold for both halves — the program and the
        // position — so looking now costs nothing extra: the drive
        // needed them anyway.
        val d = dialogue(id)
        placed(d).flatMap:
          case Left(verdict) => pure(verdict)
          case Right((p, at)) => driving(id, 0, Resume.Held(d, p, at))

  /**
   * REBUILD THE PLACE, AND NAME THE TWO WAYS IT CAN REFUSE
   * (worker-incompatible, 2026-09-17).
   *
   * `Left` is a verdict the caller should return as it stands;
   * `Right` is where the program is and how many records put it
   * there.
   *
   * A throw HERE is a bad deploy: the program is being run over
   * history it ACCEPTED, before any question is asked, and replay is
   * deterministic by the `Replayable` discipline — so it throws again
   * on every pass, for ever, and `Failed` would promise a retry that
   * cannot help. It needs `isolate` for the same reason `tick` does:
   * catching belongs to a concrete row.
   *
   * ONE FUNCTION because there are TWO doors that rebuild a place —
   * a cold start and a chapter boundary — and the first version of
   * this guarded only the cold one, so a program that could not read
   * the seed it had just written escaped the worker instead of being
   * named. Two copies of a rule is one copy too many.
   */
  private def placed(d: Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F])
      : Either[Worker.Progress[R],
               (Delim.Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], Int)] ! G =
    val look: Either[Throwable, Either[Dialogue.Stopped,
                (Delim.Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], Int)]] ! G =
      isolate match
        case None => d.standing.up[G].map(Right(_))
        case Some(iso) => iso(d.standing.up[G])
    look.flatMap:
      case Left(e) => pure(Left(Worker.Progress.Incompatible(e.toString)))
      case Right(Left(stopped)) => broken(d, stopped).map(Left(_))
      case Right(Right(where)) => pure(Right(where))

  private def driving(id: String, chapters: Int,
                      from: Resume.Held[Wf.Ask[Q], Wf.Ans[A], R, F])
                     : Worker.Progress[R] ! G =
    // ONE dialogue for the whole chapter, because its `seen` is what
    // makes the won-the-race check free; a fresh one per answer would
    // re-fold the journal every time
    val d = from.dialogue
    d.runWorkflowFromIn[G](from.paused, from.at)(oracle)(
        using runtime(id), summon[RowLift.Sub[F, G]]).flatMap:
      case (Right(r), _, _) => seedOf(r) match
        case None =>
          timers.disarm(id)
          // AFTER the journal, never instead of it: a worker that dies
          // between the two leaves a parent waiting, and a waiting
          // parent is recoverable where a wrong answer is not
          children.foreach(_.completed(id, resultText(r)))
          // a finished program is not worth keeping alive
          resume.foreach(_.drop(id))
          pure(Worker.Progress.Finished(r))
        case Some(seed) =>
          // the program bounded its own history: close the chapter and
          // carry the run on from the seed. The `expect` is the RECORD
          // count, which a continuation does not reset — so a second
          // worker still standing in the old chapter loses this race
          // exactly as it would lose a race to answer.
          val _ = d.continueAs(Right(seed), d.recovered.accepted)
          resume.foreach(_.drop(id))     // the chapter it held is gone
          if chapters + 1 >= continuations then
            pure(Worker.Progress.Continued(chapters + 1))
          else placed(d).flatMap:
            case Left(verdict) => pure(verdict)
            case Right((p, at)) => driving(id, chapters + 1, Resume.Held(d, p, at))
      case (Left(wait), p, at) =>
        // KEEP IT: this is where the cache earns its name. The program
        // is a closure and cannot be written down, but it can be held,
        // and the next call over the same id then starts warm.
        resume.foreach(_.put(id, d, p, at))
        wait match
          case Wf.Wait.Until(t) =>
            timers.arm(id, t)
            pure(Worker.Progress.Sleeping(t))
          case Wf.Wait.Signal(name) =>
            // the mail may already be here: a signal can arrive long
            // before the run reaches the `awaitSignal` that wants it,
            // and this is the moment it becomes an answer
            signals.flatMap(_.next(id, name)) match
              case Some((off, payload)) =>
                d.answer(Left(Wf.SysA.Got(payload))).up[G].flatMap: _ =>
                  // the cursor moves ONLY after the journal took it, so
                  // a crash in between re-delivers and `expect` refuses
                  // the duplicate — a repeated attempt, never a doubled
                  // answer
                  signals.foreach(_.delivered(id, name, off))
                  resume.foreach(_.drop(id))   // the journal moved
                  step(id)
              case None =>
                timers.disarm(id)
                pure(Worker.Progress.Waiting(Wf.Wait.Signal(name)))
          case Wf.Wait.Child(kid) =>
            // the child may already be done: it can finish long before
            // the parent reaches the `awaitChild` that wants it, and
            // this is the moment its result becomes an answer. Unlike a
            // signal there is no cursor — a child finishes once and its
            // result does not change, so `expect` is the whole guard.
            children.flatMap(_.resultOf(kid)) match
              case Some(result) =>
                d.answer(Left(Wf.SysA.Got(result))).up[G].flatMap: _ =>
                  resume.foreach(_.drop(id))   // the journal moved
                  step(id)
              case None =>
                timers.disarm(id)
                pure(Worker.Progress.Waiting(Wf.Wait.Child(kid)))

  /**
   * One pass over the deadlines that have passed. A due id is woken
   * only if it is REALLY waiting on a timer that has expired — see
   * the class header on why that check is not optional.
   */
  def tick(nowMillis: Long): List[(String, Worker.Progress[R])] ! G =
    def go(ids: List[String], acc: List[(String, Worker.Progress[R])])
         : List[(String, Worker.Progress[R])] ! G = ids match
      case Nil => pure(acc.reverse)
      case id :: rest => isolate match
        case None => wake(id, nowMillis).flatMap(p => go(rest, (id, p) :: acc))
        case Some(iso) => iso(wake(id, nowMillis)).flatMap:
          case Right(p) => go(rest, (id, p) :: acc)
          // the run is exactly where it was -- nothing was journalled,
          // so the next pass asks again. This says so rather than
          // ending the pass.
          case Left(e) => go(rest, (id, Worker.Progress.Failed(e.toString)) :: acc)
    go(timers.due(nowMillis), Nil)

  /** append the timer's answer if the run is genuinely waiting on it,
   * then carry the run forward */
  def wake(id: String, nowMillis: Long): Worker.Progress[R] ! G =
    val d = dialogue(id)
    // THE FOURTH DOOR (incompatible-at-wake, 2026-09-17). This one is
    // how `tick` reaches a run at all, and it rebuilt the place
    // directly — so a bad deploy on a SLEEPING run came back as
    // `Failed`, which is the exact lie `worker-incompatible` was
    // written to remove, surviving at a door that lane did not count.
    // It was found by counting the doors instead of remembering them.
    placed(d).flatMap:
      case Left(verdict) =>
        // A JOURNAL that cannot be folded will not mend itself, so it
        // comes off the clock, as it always did. A DEPLOY that cannot
        // read a good journal might — the next release fixes it — so
        // that one stays armed and resumes by itself. The price is one
        // fold per tick while it is broken, and `needsAttention` is
        // where that shows.
        verdict match
          case Worker.Progress.Incompatible(_) => ()
          case _ => timers.disarm(id)
        pure(verdict)
      case Right((p, _)) => p.asking match
        case Some(Left(Wf.Sys.Timer(t))) if t <= nowMillis =>
          d.answer(Left(Wf.SysA.Elapsed)).up[G].flatMap(_ => advance(id))
        case _ =>
          // the record was stale: this run is not waiting on a timer,
          // or not on one that has passed. Cost: one read.
          advance(id)

object Worker:

  /**
   * THE ABILITY TO CATCH, CARRIED AS A VALUE (worker-tick-isolation,
   * 2026-09-17). A worker's driver row `G` is abstract, and catching
   * belongs to a concrete row -- so a caller who wants `tick` to
   * survive one run's failure hands in the way to do it. The rule is
   * the repository's: an obligation over a row is a parameter, never
   * a search at an abstract row.
   */
  trait Isolate[G[+_]]:
    def apply[X](p: X ! G): Either[Throwable, X] ! G

  /** the instance for the row workers are actually built in */
  def isolating(using okay.Scheduler): Isolate[okay.Async] = new Isolate[okay.Async]:
    def apply[X](p: X ! okay.Async): Either[Throwable, X] ! okay.Async =
      okay.Async.attempt(p)


  /**
   * AN ORACLE THAT RETRIES, AND A JOURNAL THAT DOES NOT NOTICE
   * (workflow-retries, 2026-09-17).
   *
   * An activity fails for reasons that have nothing to do with the
   * program: a connection reset, a 503, a lock held elsewhere. The
   * workflow should not see those — it asked one question and is
   * owed one answer — so the RETRY IS THE DRIVER'S, and the journal
   * gains exactly one entry however many attempts it took. That is
   * the whole of it, and it is three lines because the two halves it
   * needs already exist: `Retry`'s policies (which are streams of
   * delays, so `take`, `map` and `++` are the policy algebra) and
   * the activity row, which is where an attempt is allowed to fail.
   *
   * WHAT HAPPENS WHEN THE POLICY IS EXHAUSTED, and it is the useful
   * half: the last error is thrown, the drive ends with NOTHING
   * appended for that question, and the run is still standing at it.
   * A later worker — the next tick, the next process — asks again
   * from the log. So "give up" here means "give up for now", not
   * "lose the run", and there is a test that walks that path.
   */
  def retrying[Q, A](policy: LazyList[Long])
                    (oracle: Q => Dialogue.Attempt ?=> A ! okay.Async)
                    (using okay.Scheduler, okay.Timer)
                    : Q => Dialogue.Attempt ?=> A ! okay.Async =
    // the key is threaded through the retries rather than lost at
    // them: every attempt at one question carries the SAME
    // `(id, index)`, which is what makes it usable as an idempotency
    // key by a service that is called more than once for one answer
    q => (at: Dialogue.Attempt) ?=> okay.Retry.async(policy)(oracle(q)(using at))

  /** the index's word for what a worker learned */
  def state[R](p: Progress[R]): Statuses.State = p match
    case Progress.Finished(r) => Statuses.State.Finished(r.toString)
    case Progress.Sleeping(t) => Statuses.State.Sleeping(t)
    case Progress.Waiting(okay.Wf.Wait.Signal(n)) => Statuses.State.Waiting(s"signal:$n")
    case Progress.Waiting(okay.Wf.Wait.Child(c)) => Statuses.State.Waiting(s"child:$c")
    case Progress.Waiting(okay.Wf.Wait.Until(t)) => Statuses.State.Sleeping(t)
    // a run that opened a new chapter is not WAITING for anything —
    // nobody has to do a thing for it to go on
    case Progress.Continued(_) => Statuses.State.Running
    case Progress.Busy(who) => Statuses.State.Waiting(s"busy:$who")
    // the index keeps these APART rather than prefixing one string:
    // the difference between them is who has to act, which is the
    // only difference a dashboard exists to show
    case Progress.Failed(why) => Statuses.State.Failed(why)
    case Progress.Incompatible(why) => Statuses.State.Incompatible(why)
    // the status line now names a LINE, not just an offset
    case Progress.Broken(d) => Statuses.State.Broken(d.toString)

  /** what a worker learned about one run */
  enum Progress[+R]:
    case Finished[R](value: R) extends Progress[R]
    case Sleeping(untilMillis: Long) extends Progress[Nothing]
    case Waiting(on: Wf.Wait) extends Progress[Nothing]
    /** the run ended a chapter and started another, and this drive
     * has run through as many as it will in one call. Nothing is
     * wrong: call `advance` again to carry it on. */
    case Continued(chapters: Int) extends Progress[Nothing]
    /** somebody else holds this run's lease, so this worker drove
     * nothing. ADVISORY: it is a reason to come back later, never a
     * guarantee that the other worker is actually running. */
    case Busy(owner: String) extends Progress[Nothing]
    /** the drive threw and `tick` caught it, because a pass over many
     * runs must not end on one of them. NOTHING was journalled and the
     * run stands where it did, so the next pass asks again -- this is
     * "not now", not "broken". A journal that cannot be folded is
     * `Broken`; this is an activity that failed. */
    case Failed(why: String) extends Progress[Nothing]
    /**
     * THE PROGRAM THREW WHILE REBUILDING ITS OWN PLACE: it cannot
     * replay history it accepted. Distinct from `Failed` on purpose —
     * replay is deterministic, so this happens again on every pass,
     * and "we will retry" would be a lie. Distinct from `Broken` too:
     * the JOURNAL is readable, the CODE is what refuses it, which is
     * what a bad deploy looks like from here. A person has to choose
     * between fixing the code and retiring the run; nothing a worker
     * does will help.
     */
    case Incompatible(why: String) extends Progress[Nothing]
    /** the journal could not be folded, and `why` says where in the
     * CODE this program stands as well as where in the log the
     * trouble is */
    case Broken(why: Dialogue.Diagnosis) extends Progress[Nothing]
