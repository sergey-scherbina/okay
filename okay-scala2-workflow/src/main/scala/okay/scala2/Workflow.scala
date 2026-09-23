package okay.scala2

import okay.{!, +, At, Delim, Wf}
import okay.given
import Rows.coerce

/*
 * okay-workflow for Scala 2.13 (specs/scala2-facade.md, stage 15.7).
 *
 * Probed first. The driver's data is plain and Scala 2 reads it
 * directly: `Wf.Step` (Done / Asking / Waiting), `Wf.Wait`, `Wf.SysA`,
 * `Wf.Runtime` (a trait Scala 2 can implement, and `Wf.Runtime.scripted`).
 * What Scala 2 cannot use is the program: a durable body is a context
 * function over `Wf.Asks`, whose doors answer `A ! (Delim + F)` and need
 * the evidence that exists only while the driver runs the body.
 *
 * So a Scala 2 workflow is an ordinary program, `Eff[Workflow[Q, A], R]`,
 * whose operations are the GADT below. Inside `Wf.resumable`, `!.translate`
 * rewrites each operation into the `Asks` door it stands for, and from
 * there everything is okay's own engine: the same journal, the same
 * replay, the same `patch` rule, the same timers and signals.
 */

/** the operations; the last parameter is the answer's type */
private[scala2] enum WfOp[Q, A, +X] derives okay.Effect:
  case Pause[Q, A](q: Q) extends WfOp[Q, A, A]
  case Now[Q, A]() extends WfOp[Q, A, Long]
  case Uuid[Q, A]() extends WfOp[Q, A, String]
  case Random[Q, A]() extends WfOp[Q, A, Double]
  case Patch[Q, A](id: String) extends WfOp[Q, A, Boolean]
  case Sleep[Q, A](millis: Long) extends WfOp[Q, A, Unit]
  case Signal[Q, A](name: String) extends WfOp[Q, A, String]
  case Child[Q, A](id: String) extends WfOp[Q, A, String]
  case Cancelled[Q, A]() extends WfOp[Q, A, Option[String]]

/**
 * A durable program's doors, and the capability its programs carry:
 * `Q` is the author's question type, `A` the answer's. Every door's
 * answer is JOURNALLED, so a replay reads it back instead of asking.
 */
final class Workflow[Q, A] private () {
  /** ask the outside world (an oracle, a person, an API) */
  def ask(q: Q): Eff[Workflow[Q, A], A] = op(WfOp.Pause[Q, A](q))
  /** the wall clock, read once */
  def now: Eff[Workflow[Q, A], Long] = op(WfOp.Now[Q, A]())
  def uuid: Eff[Workflow[Q, A], String] = op(WfOp.Uuid[Q, A]())
  def random: Eff[Workflow[Q, A], Double] = op(WfOp.Random[Q, A]())
  /** is this branch on for THIS run? false for a run whose journal
   * predates it (Temporal's getVersion) */
  def patch(id: String): Eff[Workflow[Q, A], Boolean] = op(WfOp.Patch[Q, A](id))
  /** sleep durably: the run stops (`Waiting(Until(t))`) and resumes when
   * the instant has passed; nothing is held in memory meanwhile */
  def sleep(millis: Long): Eff[Workflow[Q, A], Unit] = op(WfOp.Sleep[Q, A](millis))
  /** wait for a named signal; its payload is the answer */
  def awaitSignal(name: String): Eff[Workflow[Q, A], String] = op(WfOp.Signal[Q, A](name))
  /** wait for a child run to finish; its answer is this one's */
  def awaitChild(id: String): Eff[Workflow[Q, A], String] = op(WfOp.Child[Q, A](id))
  /** has somebody asked this run to stop? Some(reason) if so */
  def cancelled: Eff[Workflow[Q, A], Option[String]] = op(WfOp.Cancelled[Q, A]())

  private def op[X](o: WfOp[Q, A, X]): Eff[Workflow[Q, A], X] =
    Eff.of(coerce(okay.effect[[Y] =>> WfOp[Q, A, Y], X](o)))
}

object Workflow {
  def apply[Q, A]: Workflow[Q, A] = new Workflow[Q, A]
}

/**
 * The drivers. A run's whole state is its JOURNAL, the answers so far,
 * oldest first: `Right(a)` answers the author's question, `Left(...)` the
 * runtime's. Keep it anywhere (a table, okay-persist, a file); give it
 * back to resume. Each driver answers the new entries to APPEND.
 */
object Workflows {

  /** the entry that answers `Waiting(Until(t))` once the instant passed */
  val elapsed: Either[Wf.SysA, Nothing] = Left(Wf.SysA.Elapsed)

  /** the entry that answers `Waiting(Signal(n))` or `Waiting(Child(id))` */
  def got(payload: String): Either[Wf.SysA, Nothing] = Left(Wf.SysA.Got(payload))

  /** replay `journal`, then go on as far as the runtime alone can take
   * the run: the author's next question comes back as `Asking(q)` */
  def advance[Q, A, R](wf: Eff[Workflow[Q, A], R], journal: List[Either[Wf.SysA, A]],
                       runtime: Wf.Runtime = Wf.Runtime.live): (Wf.Step[Q, R], List[Either[Wf.SysA, A]]) =
    okay.!.run(Wf.replay[Q, A, R, okay.Pure](body(wf))(journal)
      .flatMap(p => Wf.advance[Q, A, R, okay.Pure](p)(using runtime, summon)))

  /** replay `journal`, then run on, `oracle` answering the author's
   * questions, until the run finishes or waits for time or a signal */
  def drive[Q, A, R](wf: Eff[Workflow[Q, A], R], journal: List[Either[Wf.SysA, A]],
                     runtime: Wf.Runtime = Wf.Runtime.live)(oracle: Q => A): (Wf.Step[Q, R], List[Either[Wf.SysA, A]]) =
    okay.!.run(Wf.replay[Q, A, R, okay.Pure](body(wf))(journal)
      .flatMap(p => Wf.drive[Q, A, R, okay.Pure](p)(q => okay.pure(oracle(q)))(using runtime, summon)))

  /** the answer, if `journal` takes the run to its end; nothing is asked
   * and no clock is read */
  def replay[Q, A, R](wf: Eff[Workflow[Q, A], R], journal: List[Either[Wf.SysA, A]]): Option[R] =
    okay.!.run(Wf.replay[Q, A, R, okay.Pure](body(wf))(journal)).finished

  /** the program as okay's engine runs it: each operation becomes the
   * door it stands for */
  private def body[Q, A, R](wf: Eff[Workflow[Q, A], R]): Wf.Asks[Q, A, R, okay.Pure] ?=> R ! (Delim + okay.Pure) = {
    val w = summon[Wf.Asks[Q, A, R, okay.Pure]]
    given At = At("okay.scala2.Workflow")
    type F[X] = WfOp[Q, A, X]
    okay.!.translate[R, F, Delim + okay.Pure](coerce(wf.program))(
      [X] => (o: F[X]) => (o match {
        case WfOp.Pause(q) => w.pause(q)
        case WfOp.Now() => w.now
        case WfOp.Uuid() => w.uuid
        case WfOp.Random() => w.random
        case WfOp.Patch(id) => w.patch(id)
        case WfOp.Sleep(ms) => w.sleep(ms)
        case WfOp.Signal(n) => w.awaitSignal(n)
        case WfOp.Child(id) => w.awaitChild(id)
        case WfOp.Cancelled() => w.cancelled
      }): X ! (Delim + okay.Pure))
  }
}
