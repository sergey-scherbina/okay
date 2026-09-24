package okay2

import scala.collection.mutable
import scala.util.Random
import Free.{Return, Inject, Bind}

/**
 * Deterministic concurrency simulation: many fibers, one
 * single-threaded scheduler, every choice drawn from a SEED — an
 * interleaving becomes a VALUE. A found bug is a seed; a fix is
 * verified by replaying it; a night of CI explores millions of
 * interleavings instead of the handful the OS scheduler serves.
 *
 * The mechanism is delimited control as the primary road: a fiber is a
 * freer-tree program and its `k` at every operation IS the captured
 * continuation, made scheduler food. Blocking primitives are
 * OPERATIONS (`SimChannel` send/receive suspend to the scheduler).
 * The virtual clock advances only when nothing is runnable; a sleep
 * costs no wall time. When nothing is runnable, nothing sleeps and
 * fibers still wait, that is a DEADLOCK, reported as the outcome
 * rather than as a hung test.
 *
 * The row is `Sim.Op`, as in the Scala 3 core; its operations live in
 * `Sim.Op`'s companion. The scheduler holds every continuation at
 * `Any => Unit ! Op` (stage 8's answer type), which a channel's typed
 * queue of `Option[A] => Unit ! Op` takes by contravariance — so the
 * scheduler never casts a payload.
 */
object Sim {

  /** a simulated fiber's handle */
  final class Fiber private[Sim] (val id: Int)

  /** a simulated channel: send parks when full, receive parks when
   * empty, close ends the stream after the buffered elements drain */
  final class SimChannel[A] private[Sim] (private[Sim] val id: Int, private[Sim] val capacity: Int) {
    private[Sim] val q = mutable.Queue.empty[A]
    private[Sim] var closed = false
    /** the parked receivers and senders of THIS channel, typed by its
     * element — the queues live on the channel so the scheduler never
     * holds an erased payload */
    private[Sim] val recvWait = mutable.Queue.empty[(Int, Option[A] => Unit ! Op)]
    private[Sim] val sendWait = mutable.Queue.empty[(Int, A, Unit => Unit ! Op)]
  }

  /** the simulation's row */
  sealed trait Op extends Row { type Op[+A] = Sim.Op.Oper[A] }

  /** the simulation's operations — the scheduling points */
  object Op {
    sealed trait Oper[+A]
    /** SCOPED: the node carries a computation closed over this
     * signature, so nothing hides in it from a handler of the row */
    final case class Fork(prog: Unit ! Op) extends Oper[Fiber]
    final case class Sleep(millis: Long) extends Oper[Unit]
    final case class Now() extends Oper[Long]
    final case class Chan[A](capacity: Int) extends Oper[SimChannel[A]]
    final case class Send[A](ch: SimChannel[A], a: A) extends Oper[Unit]
    final case class Receive[A](ch: SimChannel[A]) extends Oper[Option[A]]
    final case class Close[A](ch: SimChannel[A]) extends Oper[Unit]
    /** a scheduling point and nothing else */
    final case class Yield() extends Oper[Unit]

    implicit val effect: Effect[Op] = Effect.of[Op]
  }

  def fork(prog: Unit ! Op): Fiber ! Op = Free.inject[Op, Fiber](Op.Fork(prog))
  def sleep(millis: Long): Unit ! Op = Free.inject[Op, Unit](Op.Sleep(millis))
  def now: Long ! Op = Free.inject[Op, Long](Op.Now())
  def channel[A](capacity: Int = Int.MaxValue): SimChannel[A] ! Op = Free.inject[Op, SimChannel[A]](Op.Chan[A](capacity))
  def send[A](ch: SimChannel[A], a: A): Unit ! Op = Free.inject[Op, Unit](Op.Send(ch, a))
  def receive[A](ch: SimChannel[A]): Option[A] ! Op = Free.inject[Op, Option[A]](Op.Receive(ch))
  def yieldNow: Unit ! Op = Free.inject[Op, Unit](Op.Yield())
  def close[A](ch: SimChannel[A]): Unit ! Op = Free.inject[Op, Unit](Op.Close(ch))

  /** how a run ended */
  sealed trait Outcome
  object Outcome {
    case object Done extends Outcome
    final case class Deadlock(blockedFibers: Int) extends Outcome
  }

  /** what a run answered: the outcome, the virtual time it took, and the
   * decision TRACE — two runs are the same run iff their traces are
   * equal, which is what reproducibility MEANS here */
  final case class Trace(outcome: Outcome, virtualMillis: Long, steps: Vector[String])

  /** the fault plan, replayed by seed: the nth SEND (global ordinal) is
   * delayed — requeued instead of performed once */
  final case class Plan(delaySendAt: Set[Long] = Set.empty)

  def run(seed: Long, plan: Plan = Plan())(main: Unit ! Op): Trace = {
    val rng = new Random(seed)
    var nextFiber = 0
    var nextChan = 0
    var now = 0L
    var sends = 0L
    val steps = Vector.newBuilder[String]

    final case class Task(fiber: Int, prog: () => Unit ! Op)
    val runnable = mutable.ArrayBuffer.empty[Task]
    val sleeping = mutable.ArrayBuffer.empty[(Long, Task)]
    val chans = mutable.ArrayBuffer.empty[SimChannel[_]]

    def spawn(prog: Unit ! Op): Fiber = {
      val f = new Fiber(nextFiber)
      nextFiber += 1
      runnable += Task(f.id, () => prog)
      f
    }

    val _ = spawn(main)

    def wakeReceivers[A](ch: SimChannel[A]): Unit = {
      val ws = ch.recvWait
      while (ws.nonEmpty && (ch.q.nonEmpty || ch.closed)) {
        val (fid, k) = ws.dequeue()
        if (ch.q.nonEmpty) {
          val a = ch.q.dequeue()
          runnable += Task(fid, () => k(Some(a)))
          wakeSenders(ch)
        } else runnable += Task(fid, () => k(None))
      }
    }

    def wakeSenders[A](ch: SimChannel[A]): Unit = {
      val ws = ch.sendWait
      while (ws.nonEmpty && ch.q.size < ch.capacity) {
        val (fid, a, k) = ws.dequeue()
        ch.q.enqueue(a)
        runnable += Task(fid, () => k(()))
      }
    }

    def step(t: Task): Unit = Free.resume(t.prog()) match {
      case Return(_) => steps += s"${t.fiber}:done"
      case Inject(e) => perform(t.fiber, Split.only[Op, Any](e), (_: Any) => pure[Op, Unit](()))
      case Bind(Inject(e), k) => perform(t.fiber, Split.only[Op, Any](e), k)
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    def perform(fid: Int, op: Op.Oper[Any], k: Any => Unit ! Op): Unit = op match {
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
      case c: Op.Chan[a] =>
        val ch = new SimChannel[a](nextChan, c.capacity)
        nextChan += 1
        chans += ch
        steps += s"$fid:chan(${ch.id})"
        runnable += Task(fid, () => k(ch))
      case s: Op.Send[a] =>
        val ch = s.ch
        sends += 1
        if (plan.delaySendAt.contains(sends)) {
          steps += s"$fid:send-delayed(${ch.id})"
          runnable += Task(fid, () => Free.inject[Op, Unit](Op.Send(ch, s.a)).flatMap(k))
        } else if (ch.q.size < ch.capacity) {
          ch.q.enqueue(s.a)
          steps += s"$fid:send(${ch.id})"
          runnable += Task(fid, () => k(()))
          wakeReceivers(ch)
        } else {
          steps += s"$fid:send-park(${ch.id})"
          ch.sendWait.enqueue((fid, s.a, k))
        }
      case r: Op.Receive[a] =>
        val ch = r.ch
        if (ch.q.nonEmpty) {
          val a = ch.q.dequeue()
          steps += s"$fid:recv(${ch.id})"
          runnable += Task(fid, () => k(Some(a)))
          wakeSenders(ch)
        } else if (ch.closed) {
          steps += s"$fid:recv-end(${ch.id})"
          runnable += Task(fid, () => k(None))
        } else {
          steps += s"$fid:recv-park(${ch.id})"
          ch.recvWait.enqueue((fid, k))
        }
      case Op.Yield() =>
        steps += s"$fid:yield"
        runnable += Task(fid, () => k(()))
      case c: Op.Close[a] =>
        val ch = c.ch
        ch.closed = true
        steps += s"$fid:close(${ch.id})"
        runnable += Task(fid, () => k(()))
        wakeReceivers(ch)
    }

    var going = true
    while (going) {
      if (runnable.nonEmpty) {
        val i = rng.nextInt(runnable.size)
        val t = runnable.remove(i)
        step(t)
      } else if (sleeping.nonEmpty) {
        val wake = sleeping.map(_._1).min
        now = math.max(now, wake)
        val due = sleeping.filter(_._1 <= now)
        sleeping.filterInPlace(_._1 > now)
        due.foreach { case (_, t) => runnable += t }
        steps += s"clock:$now"
      } else going = false
    }

    val blocked = chans.map(ch => ch.recvWait.size + ch.sendWait.size).sum
    Trace(if (blocked > 0) Outcome.Deadlock(blocked) else Outcome.Done, now, steps.result())
  }
}
