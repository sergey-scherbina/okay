package okay

import okay.!.*
import scala.collection.mutable
import scala.util.Random

/**
 * Deterministic concurrency simulation (specs/sim.md): many fibers,
 * one single-threaded scheduler, every choice drawn from a SEED —
 * an interleaving becomes a VALUE. A found bug is a seed; a fix is
 * verified by replaying it; a night of CI explores millions of
 * interleavings instead of the handful the OS scheduler happens to
 * serve. Born from a day that found three real races by FLAKES.
 *
 * The mechanism is delimited control, used as the PRIMARY road (the
 * operator's rule: primary where nothing else serves): a fiber is a
 * freer-tree program and its `k` at every operation IS the captured
 * delimited continuation — the Cont foundation this stack is built
 * on, made scheduler food. Blocking primitives become OPERATIONS
 * (`SimChannel` send/receive suspend to the scheduler instead of
 * parking a thread), which is exactly why the real Channel cannot
 * be simulated but the simulated one can be chosen: the multi-
 * prompt `Delim` effect joins this seam unchanged when fibers
 * carry intervening delimiters of their own.
 *
 * The virtual clock advances only when nothing is runnable (the
 * classic rule: time moves when nothing else can); a sleep costs
 * no wall time. When nothing is runnable, nothing sleeps, and
 * fibers still wait — that is a DEADLOCK, reported as the outcome
 * rather than as a hung test.
 */
object Sim {

  /** a simulated fiber's handle */
  final class Fiber private[Sim] (val id: Int)

  /** a simulated channel: same contract shape as the real one —
   * send parks when full, receive parks when empty, close ends the
   * stream after the buffered elements drain */
  final class SimChannel[A] private[Sim] (private[Sim] val id: Int,
                                          private[Sim] val capacity: Int):
    private[Sim] val q = mutable.Queue.empty[Any]
    private[Sim] var closed = false

  /** the simulation's operations — the scheduling points */
  enum Op[+A]:
    case Fork(prog: Unit ! Op) extends Op[Fiber]
    case Sleep(millis: Long) extends Op[Unit]
    case Now() extends Op[Long]
    case Chan(capacity: Int) extends Op[SimChannel[Any]]
    case Send(ch: SimChannel[Any], a: Any) extends Op[Unit]
    case Receive(ch: SimChannel[Any]) extends Op[Option[Any]]
    case Close(ch: SimChannel[Any]) extends Op[Unit]

  // ── the program-side surface ───────────────────────────────────

  def fork(prog: Unit ! Op): Fiber ! Op = effect(Op.Fork(prog))
  def sleep(millis: Long): Unit ! Op = effect(Op.Sleep(millis))
  def now: Long ! Op = effect(Op.Now())
  def channel[A](capacity: Int = Int.MaxValue): SimChannel[A] ! Op =
    effect(Op.Chan(capacity)).asInstanceOf[SimChannel[A] ! Op]
  def send[A](ch: SimChannel[A], a: A): Unit ! Op =
    effect(Op.Send(ch.asInstanceOf[SimChannel[Any]], a))
  def receive[A](ch: SimChannel[A]): Option[A] ! Op =
    effect(Op.Receive(ch.asInstanceOf[SimChannel[Any]])).asInstanceOf[Option[A] ! Op]
  def close[A](ch: SimChannel[A]): Unit ! Op =
    effect(Op.Close(ch.asInstanceOf[SimChannel[Any]]))

  /** what a run answered: how it ended, the virtual time it took,
   * and the decision TRACE — two runs are the same run iff their
   * traces are equal, which is what reproducibility MEANS here */
  enum Outcome:
    case Done
    case Deadlock(blockedFibers: Int)
  final case class Trace(outcome: Outcome, virtualMillis: Long, steps: Vector[String])

  /** the fault plan, drawn outside and replayed by seed: the nth
   * SEND (global op ordinal) is delayed — requeued instead of
   * performed once */
  final case class Plan(delaySendAt: Set[Long] = Set.empty)

  // ── the scheduler ──────────────────────────────────────────────

  def run(seed: Long, plan: Plan = Plan())(main: Unit ! Op): Trace = {
    val rng = Random(seed)
    var nextFiber = 0
    var nextChan = 0
    var now = 0L
    var sends = 0L
    val steps = Vector.newBuilder[String]

    // prog is a THUNK: a continuation is applied when its fiber is
    // SCHEDULED, never when it is merely enqueued — side effects in
    // a map's closure belong to the step that runs them
    final case class Task(fiber: Int, prog: () => Unit ! Op)
    val runnable = mutable.ArrayBuffer.empty[Task]
    val sleeping = mutable.ArrayBuffer.empty[(Long, Task)]
    val recvWait = mutable.Map.empty[Int, mutable.Queue[(Int, Option[Any] => Unit ! Op)]]
    val sendWait = mutable.Map.empty[Int, mutable.Queue[(Int, Any, Unit => Unit ! Op)]]
    val chans = mutable.Map.empty[Int, SimChannel[Any]]

    def spawn(prog: Unit ! Op): Fiber =
      val f = new Fiber(nextFiber)
      nextFiber += 1
      runnable += Task(f.id, () => prog)
      f

    val _ = spawn(main)

    def wakeReceivers(ch: SimChannel[Any]): Unit =
      val ws = recvWait.getOrElse(ch.id, mutable.Queue.empty)
      // a woken receiver re-runs its receive against the queue
      while ws.nonEmpty && (ch.q.nonEmpty || ch.closed) do
        val (fid, k) = ws.dequeue()
        if ch.q.nonEmpty then
          val a = ch.q.dequeue()
          runnable += Task(fid, () => k(Some(a)))
          wakeSenders(ch)
        else runnable += Task(fid, () => k(None))

    def wakeSenders(ch: SimChannel[Any]): Unit =
      val ws = sendWait.getOrElse(ch.id, mutable.Queue.empty)
      while ws.nonEmpty && ch.q.size < ch.capacity do
        val (fid, a, k) = ws.dequeue()
        ch.q.enqueue(a)
        runnable += Task(fid, () => k(()))

    def step(t: Task): Unit =
      (t.prog().resume: @unchecked) match
        case Pure(_) =>
          steps += s"${t.fiber}:done"
        case Effect(e) =>
          // a terminal operation: perform it, nothing continues
          perform(t.fiber, e.asInstanceOf[Op[Any]], _ => okay.pure(()))
        case Bind(Effect(e), k) =>
          // the freer tree's own k IS the captured delimited
          // continuation; the answer type is the operation's, erased
          // here and honest by the signature (the produced precedent)
          val kk = k.asInstanceOf[Any => Unit ! Op]
          perform(t.fiber, e.asInstanceOf[Op[Any]], kk)

    def perform(fid: Int, op: Op[Any], k: Any => Unit ! Op): Unit = op match
      case Op.Fork(prog) =>
        val f = spawn(prog)
        steps += s"$fid:fork(${f.id})"
        runnable += Task(fid, () => k(f))
      case Op.Sleep(ms) =>
        steps += s"$fid:sleep($ms)"
        sleeping += ((now + ms, Task(fid, () => k(()))))
      case Op.Now() =>
        val n = now
        runnable += Task(fid, () => k(n))
      case Op.Chan(cap) =>
        val ch = new SimChannel[Any](nextChan, cap)
        nextChan += 1
        chans(ch.id) = ch
        steps += s"$fid:chan(${ch.id})"
        runnable += Task(fid, () => k(ch))
      case Op.Send(ch, a) =>
        sends += 1
        if plan.delaySendAt.contains(sends) then
          steps += s"$fid:send-delayed(${ch.id})"
          runnable += Task(fid, () => effect[Op, Unit](Op.Send(ch, a)).flatMap(k))
        else if ch.q.size < ch.capacity then
          ch.q.enqueue(a)
          steps += s"$fid:send(${ch.id})"
          runnable += Task(fid, () => k(()))
          wakeReceivers(ch)
        else
          steps += s"$fid:send-park(${ch.id})"
          sendWait.getOrElseUpdate(ch.id, mutable.Queue.empty)
            .enqueue((fid, a, (u: Unit) => k(u)))
      case Op.Receive(ch) =>
        if ch.q.nonEmpty then
          val a = ch.q.dequeue()
          steps += s"$fid:recv(${ch.id})"
          runnable += Task(fid, () => k(Some(a)))
          wakeSenders(ch)
        else if ch.closed then
          steps += s"$fid:recv-end(${ch.id})"
          runnable += Task(fid, () => k(None))
        else
          steps += s"$fid:recv-park(${ch.id})"
          recvWait.getOrElseUpdate(ch.id, mutable.Queue.empty)
            .enqueue((fid, (o: Option[Any]) => k(o)))
      case Op.Close(ch) =>
        ch.closed = true
        steps += s"$fid:close(${ch.id})"
        runnable += Task(fid, () => k(()))
        wakeReceivers(ch)

    var going = true
    while going do
      if runnable.nonEmpty then
        // THE seeded choice: who runs next
        val i = rng.nextInt(runnable.size)
        val t = runnable.remove(i)
        step(t)
      else if sleeping.nonEmpty then
        // time moves only when nothing else can
        val wake = sleeping.map(_._1).min
        now = math.max(now, wake)
        val due = sleeping.filter(_._1 <= now)
        sleeping.filterInPlace(_._1 > now)
        due.foreach((_, t) => runnable += t)
        steps += s"clock:$now"
      else going = false

    val blocked = recvWait.values.map(_.size).sum + sendWait.values.map(_.size).sum
    Trace(
      if blocked > 0 then Outcome.Deadlock(blocked) else Outcome.Done,
      now, steps.result())
  }
}
