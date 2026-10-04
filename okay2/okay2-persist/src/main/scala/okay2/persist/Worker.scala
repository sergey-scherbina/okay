package okay2.persist

import okay2.{!, +, At, Replayable, Row, Shift, Wf, pure}
import okay2.async.{Async, Retry, Scheduler, Timer}
import okay2.codec.Schema

/**
 * THE DURABLE WORKFLOW ENGINE (okay-persist's Worker.scala;
 * specs/durable-workflow.md): one program, many runs, each a `Dialogue`
 * over one topic, driven as far as it goes and left standing where it
 * stops. A run that sleeps arms a durable `Timers` entry and `tick`
 * wakes it; one that waits on a signal or a child is advanced again when
 * that arrives; one that returns a seed (`seedOf`) continues as a fresh
 * chapter (Temporal's `continueAsNew`), at most `continuations` in one
 * advance. The side tables — signals, statuses, cancels, children,
 * leases, the warm `Resume` cache — are each optional and each the log.
 *
 * The program's row `F` stays replayable; the oracle runs in `G`, any row
 * `F` widens to (`G <: F`), so an activity may reach outside. `isolate`
 * turns an activity's throw into a `Failed` progress instead of an
 * exception out of `tick`.
 */
final class Worker[Q, A, R, F <: Row, G <: F](topic: Topic, program: String, timers: Timers,
                                              oracle: (Q, Dialogue.Attempt) => A ! G,
                                              snapshots: Option[Snapshots] = None,
                                              snapshotEvery: Int = 0,
                                              signals: Option[Signals] = None,
                                              statuses: Option[Statuses] = None,
                                              cancels: Option[Cancels] = None,
                                              seedOf: R => Option[A] = (_: R) => None,
                                              continuations: Int = 64,
                                              children: Option[Children] = None,
                                              resultText: R => String = (r: R) => r.toString,
                                              leases: Option[Leases] = None,
                                              owner: String = "worker",
                                              leaseMillis: Long = 30000L,
                                              clock: () => Long = () => System.currentTimeMillis(),
                                              resume: Option[Resume[Wf.Ask[Q], Wf.Ans[A], R, F]] = None,
                                              isolate: Option[Worker.Isolate[G]] = None)
                                             (body: Wf.Asks[Q, A, R, F] => R ! (Shift[Any] + F))
                                             (implicit sa: Schema[Wf.Ans[A]], rp: Replayable[Shift[Any] + F],
                                              om: Shift.Machine[F], at: At, rt: Wf.Runtime) {

  private type Held = Resume.Held[Wf.Ask[Q], Wf.Ans[A], R, F]
  private type Here = (Shift.Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], Int)

  /** the run under this id, as a durable dialogue */
  def dialogue(id: String): Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F] =
    Dialogue.workflow[Q, A, R, F](topic, id, program, snapshots, snapshotEvery)(body)

  private def runtime(id: String): Wf.Runtime = cancels match {
    case None => rt
    case Some(c) => Wf.Runtime.cancellable(rt)(c.requested(id))
  }

  /** ask a run to stop; the program sees it when it asks `cancelled` */
  def cancel(id: String, why: String): Boolean = cancels match {
    case Some(c) => c.cancel(id, why); true
    case None => false
  }

  /** start a run (the same move as advancing one: the log decides) */
  def start(id: String): Worker.Progress[R] ! G = advance(id)

  /** drive a run as far as it goes, under the lease if there is one, and
   * note where it stopped */
  def advance(id: String): Worker.Progress[R] ! G = leases match {
    case Some(ls) if !ls.acquire(id, owner, clock() + leaseMillis, clock()) =>
      pure[G, Worker.Progress[R]](Worker.Progress.Busy(ls.held(id, clock()).map(_.owner).getOrElse("unknown")))
    case _ =>
      step(id).flatMap[G, Worker.Progress[R]] { p =>
        note(id, p).map { _ =>
          leases.foreach(_.release(id, owner, clock()))
          p
        }
      }
  }

  private def note(id: String, p: Worker.Progress[R]): Unit ! G = {
    def write(asking: Option[String], where: Option[String]): Unit =
      statuses.foreach(_.put(Statuses.Status(id, program, Worker.state(p), asking, where, System.currentTimeMillis())))
    statuses match {
      case None => pure[G, Unit](())
      case Some(_) => p match {
        case Worker.Progress.Finished(_) | Worker.Progress.Broken(_)
             | Worker.Progress.Incompatible(_) | Worker.Progress.Failed(_) =>
          pure[G, Unit](write(None, None))
        case _ => placed(dialogue(id)).map {
          case Right((p0, _)) => write(p0.asking.map(_.toString), p0.where)
          case Left(_) => write(None, None)
        }
      }
    }
  }

  private def broken(d: Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F], stopped: Dialogue.Stopped): Worker.Progress[R] ! G =
    d.diagnosis.map(found => Worker.Progress.Broken(found.getOrElse(Dialogue.Diagnosis(stopped, 0, None, None))))

  private def step(id: String): Worker.Progress[R] ! G =
    resume.flatMap(_.get(id)) match {
      case Some(h) => driving(id, 0, h)
      case None =>
        val d = dialogue(id)
        placed(d).flatMap[G, Worker.Progress[R]] {
          case Left(verdict) => pure[G, Worker.Progress[R]](verdict)
          case Right((p, at)) => driving(id, 0, Resume.Held(d, p, at))
        }
    }

  /** where the run stands, or the verdict that it cannot be placed: a
   * stopped fold is `Broken`, a replay that THROWS (a program that no
   * longer reads its own journal) is `Incompatible` when isolated */
  private def placed(d: Dialogue[Wf.Ask[Q], Wf.Ans[A], R, F]): Either[Worker.Progress[R], Here] ! G = {
    val look: Either[Throwable, Either[Dialogue.Stopped, Here]] ! G = isolate match {
      case None => d.standing.map(Right(_))
      case Some(iso) => iso(d.standing)
    }
    look.flatMap[G, Either[Worker.Progress[R], Here]] {
      case Left(e) => pure[G, Either[Worker.Progress[R], Here]](Left(Worker.Progress.Incompatible(e.toString)))
      case Right(Left(stopped)) => broken(d, stopped).map(Left(_))
      case Right(Right(where)) => pure[G, Either[Worker.Progress[R], Here]](Right(where))
    }
  }

  // trampolined: every recursion below is deferred into a flatMap, and
  // the continuations are bounded by `continuations`
  private def driving(id: String, chapters: Int, from: Held): Worker.Progress[R] ! G = {
    val d = from.dialogue
    d.runWorkflowFromIn[G](from.paused, from.at)(oracle)(runtime(id)).flatMap[G, Worker.Progress[R]] {
      case (Right(r), _, _) => seedOf(r) match {
        case None =>
          timers.disarm(id)
          children.foreach(_.completed(id, resultText(r)))
          resume.foreach(_.drop(id))
          pure[G, Worker.Progress[R]](Worker.Progress.Finished(r))
        case Some(seed) =>
          val _ = d.continueAs(Right(seed), d.recovered.accepted)
          resume.foreach(_.drop(id))     // the chapter it held is gone
          if (chapters + 1 >= continuations) pure[G, Worker.Progress[R]](Worker.Progress.Continued(chapters + 1))
          else placed(d).flatMap[G, Worker.Progress[R]] {
            case Left(verdict) => pure[G, Worker.Progress[R]](verdict)
            case Right((p, at)) => driving(id, chapters + 1, Resume.Held(d, p, at))
          }
      }
      case (Left(wait), p, at) =>
        resume.foreach(_.put(id, d, p, at))
        wait match {
          case Wf.Wait.Until(t) =>
            timers.arm(id, t)
            pure[G, Worker.Progress[R]](Worker.Progress.Sleeping(t))
          case Wf.Wait.Signal(name) =>
            signals.flatMap(_.next(id, name)) match {
              case Some((off, payload)) =>
                d.answer(Left(Wf.SysA.Got(payload))).flatMap[G, Worker.Progress[R]] { _ =>
                  signals.foreach(_.delivered(id, name, off))
                  resume.foreach(_.drop(id))   // the journal moved
                  step(id)
                }
              case None =>
                timers.disarm(id)
                pure[G, Worker.Progress[R]](Worker.Progress.Waiting(Wf.Wait.Signal(name)))
            }
          case Wf.Wait.Child(kid) =>
            children.flatMap(_.resultOf(kid)) match {
              case Some(result) =>
                d.answer(Left(Wf.SysA.Got(result))).flatMap[G, Worker.Progress[R]] { _ =>
                  resume.foreach(_.drop(id))   // the journal moved
                  step(id)
                }
              case None =>
                timers.disarm(id)
                pure[G, Worker.Progress[R]](Worker.Progress.Waiting(Wf.Wait.Child(kid)))
            }
        }
    }
  }

  /** wake every run whose timer is due, in a stable order; an isolated
   * activity's throw is that run's `Failed`, never the tick's */
  def tick(nowMillis: Long): List[(String, Worker.Progress[R])] ! G = {
    def go(ids: List[String], acc: List[(String, Worker.Progress[R])]): List[(String, Worker.Progress[R])] ! G = ids match {
      case Nil => pure[G, List[(String, Worker.Progress[R])]](acc.reverse)
      case id :: rest => isolate match {
        case None => wake(id, nowMillis).flatMap[G, List[(String, Worker.Progress[R])]](p => go(rest, (id, p) :: acc))
        case Some(iso) => iso(wake(id, nowMillis)).flatMap[G, List[(String, Worker.Progress[R])]] {
          case Right(p) => go(rest, (id, p) :: acc)
          case Left(e) => go(rest, (id, Worker.Progress.Failed(e.toString)) :: acc)
        }
      }
    }
    go(timers.due(nowMillis), Nil)
  }

  /** one run's timer fired: answer `Elapsed` if it is still asking for
   * that timer, and advance */
  def wake(id: String, nowMillis: Long): Worker.Progress[R] ! G = {
    val d = dialogue(id)
    placed(d).flatMap[G, Worker.Progress[R]] {
      case Left(verdict) =>
        verdict match {
          case Worker.Progress.Incompatible(_) => ()
          case _ => timers.disarm(id)
        }
        pure[G, Worker.Progress[R]](verdict)
      case Right((p, _)) => p.asking match {
        case Some(Left(Wf.Sys.Timer(t))) if t <= nowMillis =>
          d.answer(Left(Wf.SysA.Elapsed)).flatMap[G, Worker.Progress[R]](_ => advance(id))
        case _ => advance(id)
      }
    }
  }
}

object Worker {

  /** run a program in `G`, its throw caught as data */
  trait Isolate[G <: Row] {
    def apply[X](p: X ! G): Either[Throwable, X] ! G
  }

  /** the isolation an `Async` worker has: `Async.attempt` */
  def isolating(implicit S: Scheduler): Isolate[Async] = new Isolate[Async] {
    def apply[X](p: X ! Async): Either[Throwable, X] ! Async = Async.attempt(p)
  }

  /** an activity retried by a policy (`Retry`'s delays); the same
   * `Attempt` reaches every retry */
  def retrying[Q, A](policy: LazyList[Long])(oracle: (Q, Dialogue.Attempt) => A ! Async)
                    (implicit S: Scheduler, T: Timer): (Q, Dialogue.Attempt) => A ! Async =
    (q, at) => Retry.async(policy)(oracle(q, at))

  /** the status a progress is written as */
  def state[R](p: Progress[R]): Statuses.State = p match {
    case Progress.Finished(r) => Statuses.State.Finished(r.toString)
    case Progress.Sleeping(t) => Statuses.State.Sleeping(t)
    case Progress.Waiting(Wf.Wait.Signal(n)) => Statuses.State.Waiting(s"signal:$n")
    case Progress.Waiting(Wf.Wait.Child(c)) => Statuses.State.Waiting(s"child:$c")
    case Progress.Waiting(Wf.Wait.Until(t)) => Statuses.State.Sleeping(t)
    case Progress.Continued(_) => Statuses.State.Running
    case Progress.Busy(who) => Statuses.State.Waiting(s"busy:$who")
    case Progress.Failed(why) => Statuses.State.Failed(why)
    case Progress.Incompatible(why) => Statuses.State.Incompatible(why)
    case Progress.Broken(d) => Statuses.State.Broken(d.toString)
  }

  /** where an advance left a run */
  sealed trait Progress[+R]
  object Progress {
    final case class Finished[R](value: R) extends Progress[R]
    final case class Sleeping(untilMillis: Long) extends Progress[Nothing]
    final case class Waiting(on: Wf.Wait) extends Progress[Nothing]
    final case class Continued(chapters: Int) extends Progress[Nothing]
    final case class Busy(owner: String) extends Progress[Nothing]
    final case class Failed(why: String) extends Progress[Nothing]
    final case class Incompatible(why: String) extends Progress[Nothing]
    final case class Broken(why: Dialogue.Diagnosis) extends Progress[Nothing]
  }
}
