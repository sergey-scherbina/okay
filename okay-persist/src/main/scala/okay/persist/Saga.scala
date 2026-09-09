package okay.persist

import okay.{!, Async, Scheduler, pure}
import okay.codec.{Codecs, Schema}

/**
 * The saga (specs/persist.md, "The saga"): a multi-item change as a
 * journaled sequence of steps, each with a compensation — the shape
 * specs/data.md and `Docs` name instead of multi-document
 * transactions, now packaged rather than hand-rolled per consumer.
 *
 * Intent FIRST, like every journal in this stack: `Intent(i)` lands
 * before step i runs, `Done(i, state)` after, so recovery can tell
 * "never ran", "ran, answer lost" (the crash window) and "ran" apart.
 * A step that fails journals `Failed` and the steps before it are
 * compensated in reverse, each `Undone(j, state)` journaled; the end
 * is `Finished(state)` or `Aborted(state, step, error)`, and both are
 * answers a later `recover` simply returns.
 *
 * Recovery folds the saga's records and acts by the declared
 * `Policy`: `Forward` finishes the remaining steps (re-running a step
 * whose answer was lost — so a step's effect must be idempotent at
 * its far end: a CAS, a WithKey, a unique constraint, the Durable
 * rule applied one more time); `Backward` compensates everything done
 * (a compensation, likewise, must tolerate "maybe it happened").
 *
 * One saga = one key = one partition, the TopicJournal convention;
 * the state travels as CBOR bytes inside the records (Schema at the
 * edge, the persist layering) and the records through the Typed
 * envelope, so the format evolves by upcast.
 */
final class Saga[S](topic: Topic, val id: String, steps: Vector[Saga.Step[S]], policy: Saga.Policy)
                   (using Schema[S], Scheduler):
  import Saga.*
  require(steps.nonEmpty, "a saga has at least one step")

  private val typed = Typed[Rec](topic, version = 1, upcasts = Map.empty)
  private val codec = Codecs.cbor(summon[Schema[S]])
  private val key = id.getBytes("UTF-8")
  private val partition = Topic.route(key, topic.partitions)

  private def put(r: Rec): Unit = typed.append(partition, key, r, Ack.Durable): Unit
  private def bytes(s: S): Array[Byte] = codec.encode(s)
  private def stateOf(b: Array[Byte]): S =
    codec.decode(b).fold(e => throw IllegalStateException(s"saga $id: a journaled state does not decode: $e"), identity)

  /** the first run: from `init` through every step */
  def run(init: S): Outcome[S] ! Async =
    put(Rec.Started(bytes(init)))
    forward(0, init)

  /** after a restart: what the journal says, finished by the policy;
   * a saga already at its end answers that end and touches nothing */
  def recover(): Outcome[S] ! Async =
    val f = fold()
    f.ended match
      case Some(o) => pure(o)
      case None =>
        if f.started.isEmpty then throw IllegalStateException(s"saga $id: no such saga")
        f.failed match
          case Some((i, err)) => backward(f.undoFrom.getOrElse(i - 1), f.state, i, err)
          case None => policy match
            case Policy.Forward => forward(f.pendingIntent.getOrElse(f.doneUpTo + 1), f.state)
            case Policy.Backward =>
              backward(f.pendingIntent.getOrElse(f.doneUpTo), f.state, -1, "recovered backward by policy")

  /** where the saga stands, as a value */
  def status: Status =
    val f = fold()
    val phase = f.ended match
      case Some(_: Outcome.Finished[?]) => "finished"
      case Some(_: Outcome.Aborted[?]) => "aborted"
      case None if f.started.isEmpty => "unknown"
      case None if f.failed.isDefined => "compensating"
      case None => "running"
    Status(id, phase, f.done.map(steps(_).name), f.undone.map(steps(_).name),
      f.pendingIntent.map(steps(_).name), f.failed.map(_._2))

  private def forward(i: Int, s: S): Outcome[S] ! Async =
    if i >= steps.length then
      put(Rec.Finished(bytes(s)))
      pure(Outcome.Finished(s))
    else
      put(Rec.Intent(i))
      Async.attempt(steps(i).forward(s)).flatMap {
        case Right(s2) =>
          put(Rec.Done(i, bytes(s2)))
          forward(i + 1, s2)
        case Left(h: Halt) => throw h   // the process dies here: the intent stands, nothing else is written
        case Left(e) =>
          val err = Option(e.getMessage).getOrElse(e.getClass.getName)
          put(Rec.Failed(i, err))
          backward(i - 1, s, i, err)
      }

  /** compensate steps j, j-1, ... 0 from state `s`, then Aborted */
  private def backward(j: Int, s: S, failedStep: Int, err: String): Outcome[S] ! Async =
    if j < 0 then
      put(Rec.Aborted(bytes(s), failedStep, err))
      pure(Outcome.Aborted(s, if failedStep >= 0 then Some(steps(failedStep).name) else None, err))
    else
      put(Rec.Undoing(j))
      Async.attempt(steps(j).compensate(s)).flatMap {
        case Right(s2) =>
          put(Rec.Undone(j, bytes(s2)))
          backward(j - 1, s2, failedStep, err)
        case Left(h: Halt) => throw h
        case Left(e) =>
          // a compensation that fails is the one thing a saga cannot
          // answer for: leave the journal as it is and say so
          throw Stuck(id, steps(j).name, Option(e.getMessage).getOrElse(e.getClass.getName), e)
      }

  private final class Fold:
    var started: Option[S] = None
    var state: S = null.asInstanceOf[S]   // set by Started before any read
    var doneUpTo: Int = -1
    var done: Vector[Int] = Vector.empty
    var undone: Vector[Int] = Vector.empty
    var pendingIntent: Option[Int] = None
    var undoFrom: Option[Int] = None      // the compensation to (re)do next
    var failed: Option[(Int, String)] = None
    var ended: Option[Outcome[S]] = None

  private def fold(): Fold =
    val f = Fold()
    var from = topic.begin(partition)
    var going = true
    while going do
      typed.read(partition, from, 256) match
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            for d <- rs if going do
              d match
                case Typed.Decoded.Ok(off, _, k, rec) =>
                  if k.sameElements(key) then apply(f, rec)
                  from = off + 1
                case Typed.Decoded.Bad(off, _) =>
                  going = false
                  from = off
    f

  private def apply(f: Fold, rec: Rec): Unit = rec match
    case Rec.Started(b) =>
      val s = stateOf(b); f.started = Some(s); f.state = s
    case Rec.Intent(i) => f.pendingIntent = Some(i)
    case Rec.Done(i, b) =>
      f.pendingIntent = None; f.doneUpTo = i; f.done :+= i; f.state = stateOf(b)
    case Rec.Failed(i, err) =>
      f.pendingIntent = None; f.failed = Some((i, err)); f.undoFrom = Some(i - 1)
    case Rec.Undoing(j) => f.undoFrom = Some(j)
    case Rec.Undone(j, b) =>
      f.undone :+= j; f.undoFrom = Some(j - 1); f.state = stateOf(b)
    case Rec.Finished(b) => f.ended = Some(Outcome.Finished(stateOf(b)))
    case Rec.Aborted(b, i, err) =>
      f.ended = Some(Outcome.Aborted(stateOf(b), if i >= 0 then Some(steps(i).name) else None, err))

object Saga:

  /** what recovery does with an unfinished saga */
  enum Policy:
    case Forward, Backward

  /** one step: its effect and the compensation that undoes it, both
   * over the saga's state */
  final case class Step[S](name: String, forward: S => S ! Async, compensate: S => S ! Async)

  enum Outcome[S]:
    case Finished(state: S)
    case Aborted(state: S, failedStep: Option[String], error: String)

  /** the saga's standing, a Schema value for an ops topic or /metrics */
  final case class Status(id: String, phase: String, done: Vector[String], undone: Vector[String],
                          pending: Option[String], error: Option[String]) derives Schema

  /** the journal's records: the state as CBOR bytes at the edge */
  enum Rec derives Schema:
    case Started(state: Array[Byte])
    case Intent(step: Int)
    case Done(step: Int, state: Array[Byte])
    case Failed(step: Int, error: String)
    case Undoing(step: Int)
    case Undone(step: Int, state: Array[Byte])
    case Finished(state: Array[Byte])
    case Aborted(state: Array[Byte], step: Int, error: String)

  /** the process dying between a step's effect and its answer, as a
   * value: a step (or compensation) that throws it leaves the intent
   * in the journal and nothing else — the crash window, for tests
   * and for a deliberate stop */
  final class Halt extends RuntimeException("halt") with scala.util.control.NoStackTrace

  /** a compensation failed: the journal stands where it is, and a
   * human or a retry of `recover` decides */
  final class Stuck(val saga: String, val step: String, val error: String, cause: Throwable)
    extends RuntimeException(s"saga $saga: compensation of '$step' failed: $error", cause)

  def apply[S: Schema](topic: Topic, id: String, policy: Policy = Policy.Forward)(steps: Step[S]*)
                      (using Scheduler): Saga[S] =
    new Saga[S](topic, id, steps.toVector, policy)
