package okay.actor

import okay.*
import okay.given

/**
 * An actor: a mailbox, a loop that reads it one message at a time,
 * and state threaded through that loop.
 *
 * ALMOST NOTHING IS INVENTED HERE, which is the design (specs/actor.md
 * and okay-cluster's own note). The mailbox is a `Channel` — with
 * backpressure and a choice of contract, which most actor runtimes do
 * not offer; one-message-at-a-time is one consumer on it; private
 * state needing no synchronisation is a CONSEQUENCE of that rather
 * than a separate mechanism; the address is the channel, and
 * `okay-cluster` already makes a remote one indistinguishable from a
 * local one.
 *
 * What composition does not give, and what this file is actually for,
 * is SUPERVISION: what happens when a behaviour throws.
 */
type Behavior[S, M] = (S, M) => S ! Async

/**
 * What to do when a behaviour throws.
 *
 * The default is `Stop`, and that is a decision: an actor that fails
 * and quietly carries on is how a system goes wrong without saying
 * so. Supervision is opt-in because choosing it means having thought
 * about it.
 */
// COVARIANT so that `Stop`, `Resume` and `Escalate` -- which say
// nothing about the state -- are one value each rather than one per
// actor type
enum Supervise[+S]:
  /** close the mailbox; the stream ends with the failure */
  case Stop extends Supervise[Nothing]
  /** keep the state as it was, drop the message */
  case Resume extends Supervise[Nothing]
  /** start again from a fresh state, drop the message */
  case Restart[S](fresh: () => S) extends Supervise[S]
  /** hand the throwable to someone else, then stop */
  case Escalate(to: Throwable => Unit) extends Supervise[Nothing]

/**
 * A handle on a running actor.
 *
 * `tell` answers `false` on a stopped actor rather than throwing: a
 * producer that outlives its actor is ordinary, not exceptional, and
 * that is the reading `Channel.send` already takes.
 */
final class ActorRef[M] private[actor] (private[actor] val mailbox: Channel[M]):

  private val children =
    java.util.concurrent.atomic.AtomicReference[List[ActorRef[?]]](Nil)

  /**
   * An actor whose life is bounded by this one's: stopping the parent
   * stops it first.
   *
   * There is no automatic parent RESTART to go with this, and that is
   * deliberate: restarting a subtree means deciding what happens to
   * the messages in flight in every mailbox under it, and that
   * decision belongs to whoever built the tree rather than to a
   * default here.
   */
  def spawnChild[S2, M2](init: S2, mailbox2: Channel[M2] = Channel[M2](256),
                         supervise: Supervise[S2] = Supervise.Stop)
                        (b: Behavior[S2, M2])
                        (using Scheduler, CanBlock): ActorRef[M2] ! Async =
    Actor.spawn(init, mailbox2, supervise)(b).map: child =>
      var go = true
      while go do
        val cur = children.get
        if children.compareAndSet(cur, child :: cur) then go = false
      child

  /** fire and forget; false if the actor has stopped */
  def tell(m: M): Boolean ! Async = mailbox.send(m)

  /**
   * Send, and wait for the reply the actor puts in the box.
   *
   * `within` has NO DEFAULT, and that is the point: an ask that can
   * wait for ever is a deadlock with good manners. The caller knows
   * how long the answer is worth waiting for; this module does not.
   *
   * The reply travels in a one-shot channel, so the correlation is
   * the box itself rather than a table of pending requests keyed by
   * id -- nothing to leak, nothing to clean up, and a reply that
   * arrives after the timeout simply lands in a box nobody holds.
   */
  def ask[R](f: Reply[R] => M, within: Long)(using Timer): Option[R] ! Async =
    val box = Reply[R]()
    mailbox.send(f(box)).flatMap: accepted =>
      if !accepted then okay.pure(None)
      else box.await(within)

  /** stop, DRAINING: with the strong contract every message already
   * accepted is handled first. An actor built on a weak channel stops
   * abruptly instead, and the difference is visible where it is
   * built, not here.
   *
   * A SUPERVISED stop is different: when a behaviour fails under
   * `Supervise.Stop` or `Escalate`, the messages behind the poisonous
   * one are accepted but will never be handled -- the actor is dead
   * -- and the loop drains and DISCARDS them so that `stopped` comes
   * true. A caller who needs them handled wanted `Resume` or
   * `Restart`; a caller who needs to know they were dropped watches
   * `stopped` on a `tell` that answered true. */
  def stop(): Unit ! Async = async {
    // LEAVES INWARD: a child must not outlive its parent's mailbox,
    // so the parent's stop completes only after every child has
    // closed AND drained. Closing the parent first would leave a
    // child holding messages nobody will ever read
    children.get.foreach: c =>
      c.stopBlocking()
    mailbox.close()
  }

  private[actor] def stopBlocking(): Unit =
    children.get.foreach(_.stopBlocking())
    mailbox.close()
    // drained, not merely closed: the strong contract says the
    // accepted messages are still coming, and law 8 says a parent
    // waits for them
    val deadline = System.currentTimeMillis() + 5000
    while !mailbox.finished && System.currentTimeMillis() < deadline do Thread.`yield`()

  def stopped: Boolean = mailbox.finished

/**
 * The box an `ask` waits on: one value, once.
 *
 * A channel of capacity two rather than a bespoke promise, because a
 * channel already answers every question this needs -- delivery,
 * closing, and a reader that waits -- and inventing a second
 * mechanism for one value would be a mechanism to keep correct
 * twice. Capacity two, not one: a ring cannot express one (see
 * `Ring.capacity`), and the difference is invisible here because
 * only one value is ever sent.
 */
final class Reply[R] private[actor] ():
  private val box = Channel[R](2)

  /** answer the ask; a second answer is dropped, so a behaviour that
   * replies twice is a bug in the behaviour and not a corruption
   * here */
  def apply(r: R): Unit = { box.offer(r): Unit; box.close() }

  /**
   * The wait is ONE operation: the box's asynchronous receive and the
   * platform timer, whichever answers first, the other cancelled.
   * It used to be `Async.race(box.receive, Async.sleep(within))`,
   * which is the same contest with a fiber spawned for each side --
   * two fibers per ask, measured at most of an ask's 4.4 KB
   * (docs/benchmarks.md §17e). A reply that arrives after the timer
   * fired lands in a box nobody reads, as before.
   */
  private[actor] def await(within: Long)(using T: Timer): Option[R] ! Async =
    Async.await[Option[R]]: k =>
      val done = java.util.concurrent.atomic.AtomicBoolean(false)
      val cancelTimer = T.after(within): () =>
        if !done.getAndSet(true) then k(Right(None))
      box.receiveAsync: r =>
        if !done.getAndSet(true) then
          cancelTimer()
          k(r)
      () => { if !done.getAndSet(true) then cancelTimer() }

object Actor:

  /** the unsupervised spawn: a behaviour that throws stops the actor */
  def spawn[S, M](init: S)(b: Behavior[S, M])
                 (using Scheduler, CanBlock): ActorRef[M] ! Async =
    spawn(init, Channel[M](256), Supervise.Stop)(b)

  /**
   * The full spawn: your own mailbox — so the contract, the capacity
   * and the mechanism are yours — and a supervision policy.
   */
  def spawn[S, M](init: S, mailbox: Channel[M], supervise: Supervise[S])
                 (b: Behavior[S, M])
                 (using sch: Scheduler, cb: CanBlock): ActorRef[M] ! Async =
    async {
      sch.fork { () => async {
        var state = init
        var running = true
        // A supervised Stop or Escalate closes the mailbox with the
        // messages behind the poisonous one still ACCEPTED inside it.
        // Nobody will ever read them, so drain and discard them here:
        // otherwise `finished` -- "every accepted element handed over"
        // -- never comes true and `ActorRef.stopped` lies for ever
        // (actor-stop-strands). The channel has been failed, so the
        // drain ends in the failure; that is the end it is waiting for.
        def discardRest(): Unit =
          try while mailbox.receiveBlocking().isDefined do ()
          catch case _: Throwable => ()
        while running do
          mailbox.receiveBlocking() match
            case None => running = false
            case Some(m) =>
              try state = b(state, m).runWith
              catch case e: Throwable =>
                // THE FAILED MESSAGE IS GONE, never retried. Redelivery
                // is how a system loops for ever on one poisonous
                // message, and the loop hides because every attempt
                // looks like a fresh failure
                supervise match
                  case Supervise.Resume => ()
                  case Supervise.Restart(fresh) => state = fresh()
                  case Supervise.Escalate(to) =>
                    to(e); mailbox.fail(e); mailbox.close(); discardRest(); running = false
                  case Supervise.Stop =>
                    mailbox.fail(e); mailbox.close(); discardRest(); running = false
      }}: Unit
      ActorRef(mailbox)
    }
