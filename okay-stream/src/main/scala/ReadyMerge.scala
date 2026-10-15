package okay




import okay.std.*
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicReference, AtomicReferenceArray}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Merge by READINESS on one thread of control (specs/ready-merge.md).
 *
 * The ring holds the SOURCES, not their data: each source's current
 * continuation sits in its slot, and one `resume` of it says what the
 * merge needs to know — ended, an element ready (`Say`), work to do now
 * (`Async.Run`), or not ready yet (`Async.Await`, which is Pending plus
 * the callback that says when). A ready element is told out and its
 * source goes to the back of the ring; a not-ready source parks in its
 * slot and its callback wakes the merge. What crosses threads is an
 * INDEX per wake-up, never an element, and a source with elements ready
 * is read with no synchronisation at all.
 *
 * No fiber, no scheduler: this is a program, and runs wherever `Async`
 * is handled. Parallelism is the SOURCE's choice — `Channel.buffer(n)(s)
 * .drained` gives one side its own fiber and turns its pull into an
 * Await, which the ring treats like any other.
 */
private[okay] object ReadyMerge:

  /** `onPark` runs each time the merge registers its own park — a
   * test's way to know the merge IS parked (TestReadyMerge's cancel
   * law), since nothing else outside the drive can see it */
  /** `quantum`: how many elements a READY source may tell in a row
   * before its turn passes. 1 (`mergeReady`) is the strict round-robin
   * its law states; `Source.merge`, which promises no order between its
   * sides, passes a batch's worth (source-merge-via-ready, Results). */
  /** `onRegister` runs each time the merge registers a SIDE's callback —
   * a test's way to know a pollable side was registered, and when
   * (poll-then-park: only once the ring ran dry) */
  /** `wait`/`pause` (poll-then-park's hybrid, specs/ready-merge.md): how
   * a dry ring waits before it registers its idle sides — the strategy
   * and the platform's rungs, both givens a caller may swap */
  def apply[A](sources: Seq[Source[A]], onPark: () => Unit = () => (), quantum: Int = 1,
               onRegister: () => Unit = () => (), release: () => Unit = () => ())
              (using waiting: Wait, pause: Pause): Source[A] =
    // the state is built per RUN, inside the program: a Source is a
    // value, and running it twice must merge twice
    // the merge OPENS A CANCEL SCOPE with its drive first, and closes it
    // when every source has ended (ready-merge-cancel-under-consumer-ops):
    // a cancel — or a consumer that stops early — then reaches the
    // sources it has parked even while its code sits inside the
    // consumer's continuation and it never parks itself
    defer:
      val r = new Run[A](sources, onPark, quantum, onRegister, waiting, pause, release)
      effect[Async, Unit](Async.Run(Async.Enter(r.scope))).at[Writer % A + Async].flatMap(_ => r.again())

  /** a registration's answer, when it came before the drive moved on */
  /** a tell: its value, and the rest past it */
  private def told[A, X, R[_]](o: Walk.Op[Writer % A, X, Unit, R]): (A, Unit ! R) = o.op match
    case Writer.Say(w) => (w, o.resume(()))

  private final class Answer[X](val r: Either[Throwable, X])
  private object Moved

  private final class Run[A](sources: Seq[Source[A]], onPark: () => Unit, quantum: Int, onRegister: () => Unit,
                             waiting: Wait, pause: Pause, release: () => Unit):
    private given Pause = pause
    private type R = Writer % A + Async
    private val n = sources.length

    /** each source's current continuation. Written by a callback only
     * while its source is parked, and published through `woken` */
    // each source WALKED (`Walk`): to its next tell, its Async on the way walked out to the merge, which answers it
    // itself — a slot holds the walk, a program needing Async alone, resumed where it stopped
    private type W = Walk.Step[Writer % A, Unit, R] ! Async
    private val slot: ArrayBuffer[W] = ArrayBuffer.from(sources.map(src => Walk.next[Writer % A, Unit, R](src)))

    // the READY sources, by index — touched by the drive alone, which
    // runs one step at a time and hands over between threads with a
    // happens-before. Each index is in at most one place (ring, woken,
    // parked, or in hand), so n slots never overflow. Every source
    // starts READY, in the order given.
    private val ring = Array.tabulate(math.max(n, 1))(identity)
    private var head = 0
    private var size = n
    /** sources not yet ended: in the ring, parked, or woken */
    private var live = n

    /** DRAIN, THEN FAIL (specs/source-merge-via-ready.md): a source that
     * fails drops out, the others run to their end, and the merge then
     * fails with the FIRST failure — `Channel.merge`'s rule, so the
     * consumer gets everything actually produced before it hears */
    private var failure: Throwable | Null = null
    private def failed(e: Throwable): Unit =
      if failure == null then failure = e
      live -= 1

    /** indices a callback made ready */
    private val woken = ConcurrentLinkedQueue[Integer]()
    /** the merge's own callback, set only while it is parked; whoever
     * takes it fires it, so it fires once */
    private val waker = AtomicReference[(Either[Throwable, Unit] => Unit) | Null](null)
    /** each PARKED source's canceller — at most one registration per
     * source, since a parked source is out of the ring until it wakes */
    private val cancels = AtomicReferenceArray[(() => Unit) | Null](math.max(n, 1))

    private def pushBack(i: Int): Unit =
      val at = head + size
      ring(if at >= n then at - n else at) = i
      size += 1

    private def pushFront(i: Int): Unit =
      head = if head == 0 then n - 1 else head - 1
      ring(head) = i
      size += 1

    /** the source that told last, and how many in a row */
    private var last = -1
    private var streak = 0


    private def pop(): Int =
      val i = ring(head)
      head = if head == n - 1 then 0 else head + 1
      size -= 1
      i

    /** an IDLE side (poll-then-park): an Await whose registration can
     * be asked without registering, met while the merge had other work.
     * Held TYPED, so the poll's answer meets the continuation without
     * a cast; polled when a turn passes and when the ring runs dry,
     * registered only when the ring is dry and the poll found nothing */
    private final class Held[X](val i: Int, reg: (Either[Throwable, X] => Unit) => (() => Unit),
                                p: () => (Either[Throwable, X] | Null), k: X => W,
                                fail: Throwable => W):
      /** true when the poll answered — the side is out of `idle` either
       * way: back in the ring, or dropped (drain, then fail) */
      def poll(): Boolean =
        try
          val r = p()
          if r == null then false
          else
            r match
              case Right(x) => slot(i) = k(x); pushBack(i)
              case Left(e) => failed(e)
            true
        catch case e: Throwable => { failed(e); true }
      def register(): Unit = try registerNow(i, reg, k, fail) catch case e: Throwable => failed(e)

    private val idle = new Array[Held[?]](math.max(n, 1))
    private var idleN = 0

    /** every idle side asked once; an answered one leaves `idle` */
    private def pollIdle(): Unit =
      var j = 0
      while j < idleN do
        if idle(j).poll() then
          idleN -= 1
          idle(j) = idle(idleN)
        else j += 1

    /** the ring is dry: WAIT by the given strategy, each look a poll of
     * every idle side, and register what is still empty — as many as
     * it takes for the ring to hold something, or all of them, and
     * then the merge parks. A look answers true when the ring holds
     * something, or when no side is left to wait for */
    private def settleIdle(): Unit =
      if idleN > 0 && size == 0 then
        val _ = waiting.until(() => { pollIdle(); size > 0 || idleN == 0 })
      while idleN > 0 && size == 0 do
        idleN -= 1
        idle(idleN).register()

    private def drainWoken(): Unit =
      var x = woken.poll()
      while x != null do
        pushBack(x.intValue)
        x = woken.poll()

    private def fire(): Unit =
      val w = waker.getAndSet(null)
      if w != null then w(Right(()))

    private def cancelAll(): Unit =
      var j = 0
      while j < n do
        val c = cancels.getAndSet(j, null)
        if c != null then c()
        j += 1

    /** this run's cancel scope: its release is `cancelAll`, idempotent
     * (every slot is taken once) and safe from any thread */
    // the collector's door is `release` alone (the channels' close):
    // `cancelAll` is this run's, and a run the collector can reach has
    // no registrations left to cancel — a door that held the run would
    // keep the scope reachable for good (abandoned-lazylist-releases-nothing)
    val scope: Async.CancelScope = Async.CancelScope(() => { cancelAll(); release() }, release)

    /** the re-entry `step` takes through `flatMap`, so `@tailrec` still
     * checks the loop */
    def again(): Unit ! R = step()

    /** `again` as the continuation of a told value: ONE function per
     * merge, not a lambda per element (merge-cap256-gap) */
    private val resumeMerge: Any => Unit ! R = _ => again()

    @tailrec private def step(): Unit ! R =
      drainWoken()
      if size == 0 && idleN > 0 then settleIdle()
      if size == 0 then
        if live > 0 then park()
        else
          // every source has ended: the scope closes, then the answer
          val f = failure
          val done =
            if f == null then pure[Unit, R](())
            else effect[Async, Unit](Async.Run[Unit](() => throw f)).at[R]
          effect[Async, Unit](Async.Run(Async.Exit(scope))).at[R].flatMap(_ => done)
      else
        val i = pop()
        // `resume` runs the source's own code (a `Bind(Return(x), f)`
        // applies `f`), so a throw here is that source failing
        val node = try Walk.view(slot(i)) catch case e: Throwable => { failed(e); null }
        node match
          case null => step()
          case Walk.Done(told) => told match
            case Walk.Done(_) =>
              live -= 1
              step()
            case o: Walk.Op[Writer % A, x, Unit, R] =>
              val (w, rest) = ReadyMerge.told(o)
              slot(i) = Walk.next[Writer % A, Unit, R](rest)
              if i != last then { last = i; streak = 0 }
              streak += 1
              if streak < quantum then pushFront(i)
              else
                streak = 0
                pushBack(i)
                if idleN > 0 then pollIdle()
              Writer.tell(w).at[R].flatMap(resumeMerge)
          case o: Walk.Op[Async, x, Walk.Step[Writer % A, Unit, R], Async] =>
            turn[x](i, o.op, o.resume, o.fail)
            step()

    private def turn[X](i: Int, g: Async[X], k: X => W, fail: Throwable => W): Unit =
      try operate(i, g, k, fail) catch case e: Throwable => failed(e)

    private def operate[X](i: Int, g: Async[X], k: X => W, fail: Throwable => W): Unit = g match
      case Async.Run(f) =>
        val x = f()
        slot(i) = k(x)
        pushFront(i)
      case Async.Await(reg, poll) =>
        // a pollable side is never registered from here: it is polled
        // when a turn passes, and on a dry ring `settleIdle` polls it
        // once more and registers it only then
        if poll != null then
          idle(idleN) = Held(i, reg, poll, k, fail)
          idleN += 1
        else registerNow(i, reg, k, fail)

    private def registerNow[X](i: Int, reg: (Either[Throwable, X] => Unit) => (() => Unit),
                               k: X => W, fail: Throwable => W): Unit =
        onRegister()
        val cell = AtomicReference[Answer[X] | Moved.type | Null](null)
        val c = reg { r =>
          if !cell.compareAndSet(null, Answer(r)) then
            // asynchronous: build the continuation WITHOUT running it —
            // `k` is the source's code, and only the drive runs that
            slot(i) = r match
              case Right(x) => k(x)
              case Left(e) => fail(e)
            cancels.set(i, null)
            woken.add(i): Unit
            fire()
        }
        cancels.set(i, c)
        cell.getAndSet(Moved) match
          case a: Answer[X] =>
            cancels.set(i, null)
            a.r match
              case Right(x) =>
                slot(i) = k(x)
                pushFront(i)
              case Left(e) => failed(e)
          case _ => ()

    /** every live source is parked: the merge parks ONCE. The callback
     * enqueues and then takes the waker; this sets the waker and then
     * reads the queue — Dekker's handshake, so a wake-up between the two
     * is seen by one side or the other */
    private def park(): Unit ! R =
      effect[Async, Unit](Async.Await[Unit](parking)).at[R].flatMap(_ => again())

    private val cancelling: () => Unit = () => cancelAll()

    /** the park's registration, ONE per merge, and a `Discontinue`
     * (ready-merge-own-cancel-window): a callback drive that is
     * cancelled BETWEEN two operations stops before the next one without
     * performing it, and when that next one is this park the sources the
     * merge already registered would stay registered. The drive asks the
     * leftmost node to `discontinue` (Async.Drive.discontinue), and
     * this answers with the canceller the park itself would have given */
    private object parking extends ((Either[Throwable, Unit] => Unit) => (() => Unit)), Discontinue:
      def apply(cb: Either[Throwable, Unit] => Unit): () => Unit =
        waker.set(cb)
        if !woken.isEmpty then fire()
        onPark()
        cancelling
      def discontinue(): Unit = cancelAll()
