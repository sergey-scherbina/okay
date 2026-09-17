package okay.persist

import okay.{!, +, At, Delim, Replayable, Wf, pure}
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
final class Worker[Q, A, R, F[+_]](topic: Topic, program: String, timers: Timers,
                                   oracle: Q => A ! F,
                                   snapshots: Option[Snapshots] = None,
                                   snapshotEvery: Int = 0,
                                   signals: Option[Signals] = None,
                                   statuses: Option[Statuses] = None)
                                  (body: Wf.Asks[Q, A, R, F] ?=> R ! (Delim + F))
                                  (using Schema[Wf.Ans[A]], Replayable[Delim + F],
                                   Delim.OneMachine[F], At, Wf.Runtime):

  /** the dialogue this worker drives, for an id */
  def dialogue(id: String): Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F] =
    Dialogue.workflow[Q, A, R, F](topic, id, program, snapshots, snapshotEvery)(body)

  /** a new run, or an existing one: the journal decides which, so
   * these are the same call and `start` is only a name */
  def start(id: String): Worker.Progress[R] ! F = advance(id)

  /**
   * Drive one run as far as it goes, then tell the timers what
   * happened. Everything it answered on the way is already durable
   * before this returns.
   */
  def advance(id: String): Worker.Progress[R] ! F =
    step(id).flatMap(p => note(id, p).map(_ => p))

  /** tell the index what was learned, if anybody is keeping one. It
   * is written AFTER the journal, never instead of it: a status is
   * something to look at, never something to decide from, so a worker
   * that dies between the two leaves a stale line and no wrong run. */
  private def note(id: String, p: Worker.Progress[R]): Unit ! F =
    statuses match
      case None => pure(())
      case Some(ix) => dialogue(id).at.map: place =>
        val (asking, where) = place.toOption match
          case Some(d) => (d.asking.map(_.toString), d.where)
          case None => (None, None)
        ix.put(Statuses.Status(id, program, Worker.state(p), asking, where,
          System.currentTimeMillis()))

  private def step(id: String): Worker.Progress[R] ! F =
    dialogue(id).runWorkflow(oracle).flatMap:
      case Right(r) =>
        timers.disarm(id)
        pure(Worker.Progress.Finished(r))
      case Left(Wf.Wait.Until(t)) =>
        timers.arm(id, t)
        pure(Worker.Progress.Sleeping(t))
      case Left(Wf.Wait.Signal(name)) =>
        // the mail may already be here: a signal can arrive long
        // before the run reaches the `awaitSignal` that wants it, and
        // this is the moment it becomes an answer
        signals.flatMap(_.next(id, name)) match
          case Some((off, payload)) =>
            dialogue(id).answer(Left(Wf.SysA.Got(payload))).flatMap: _ =>
              // the cursor moves ONLY after the journal took it, so a
              // crash in between re-delivers and `expect` refuses the
              // duplicate — a repeated attempt, never a doubled answer
              signals.foreach(_.delivered(id, name, off))
              step(id)
          case None =>
            timers.disarm(id)
            pure(Worker.Progress.Waiting(Wf.Wait.Signal(name)))
      case Left(w) =>
        // a child wakes this one, not the clock
        timers.disarm(id)
        pure(Worker.Progress.Waiting(w))

  /**
   * One pass over the deadlines that have passed. A due id is woken
   * only if it is REALLY waiting on a timer that has expired — see
   * the class header on why that check is not optional.
   */
  def tick(nowMillis: Long): List[(String, Worker.Progress[R])] ! F =
    def go(ids: List[String], acc: List[(String, Worker.Progress[R])])
         : List[(String, Worker.Progress[R])] ! F = ids match
      case Nil => pure(acc.reverse)
      case id :: rest =>
        wake(id, nowMillis).flatMap(p => go(rest, (id, p) :: acc))
    go(timers.due(nowMillis), Nil)

  /** append the timer's answer if the run is genuinely waiting on it,
   * then carry the run forward */
  def wake(id: String, nowMillis: Long): Worker.Progress[R] ! F =
    val d = dialogue(id)
    d.at.flatMap:
      case Left(stopped) =>
        timers.disarm(id)
        pure(Worker.Progress.Broken(stopped))
      case Right(p) => p.asking match
        case Some(Left(Wf.Sys.Timer(t))) if t <= nowMillis =>
          d.answer(Left(Wf.SysA.Elapsed)).flatMap(_ => advance(id))
        case _ =>
          // the record was stale: this run is not waiting on a timer,
          // or not on one that has passed. Cost: one read.
          advance(id)

object Worker:

  /** the index's word for what a worker learned */
  def state[R](p: Progress[R]): Statuses.State = p match
    case Progress.Finished(r) => Statuses.State.Finished(r.toString)
    case Progress.Sleeping(t) => Statuses.State.Sleeping(t)
    case Progress.Waiting(okay.Wf.Wait.Signal(n)) => Statuses.State.Waiting(s"signal:$n")
    case Progress.Waiting(okay.Wf.Wait.Child(c)) => Statuses.State.Waiting(s"child:$c")
    case Progress.Waiting(okay.Wf.Wait.Until(t)) => Statuses.State.Sleeping(t)
    case Progress.Broken(why) => Statuses.State.Broken(why.toString)

  /** what a worker learned about one run */
  enum Progress[+R]:
    case Finished[R](value: R) extends Progress[R]
    case Sleeping(untilMillis: Long) extends Progress[Nothing]
    case Waiting(on: Wf.Wait) extends Progress[Nothing]
    case Broken(why: Dialogue.Stopped) extends Progress[Nothing]
