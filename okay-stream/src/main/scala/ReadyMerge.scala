package okay

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
  def apply[A](sources: Seq[Source[A]], onPark: () => Unit = () => (), quantum: Int = 1): Source[A] =
    // the state is built per RUN, inside the program: a Source is a
    // value, and running it twice must merge twice
    okay.pure[Writer % A + Async, Unit](()).flatMap(_ => new Run[A](sources, onPark, quantum).again())

  /** a registration's answer, when it came before the drive moved on */
  private final class Answer[X](val r: Either[Throwable, X])
  private object Moved

  private final class Run[A](sources: Seq[Source[A]], onPark: () => Unit, quantum: Int):
    private type R = Writer % A + Async
    private val n = sources.length

    /** each source's current continuation. Written by a callback only
     * while its source is parked, and published through `woken` */
    private val slot = ArrayBuffer.from(sources)

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

    /** the re-entry `step` takes through `flatMap`, so `@tailrec` still
     * checks the loop */
    def again(): Unit ! R = step()

    @tailrec private def step(): Unit ! R =
      import !.*
      drainWoken()
      if size == 0 then
        if live > 0 then park()
        else
          val f = failure
          if f == null then okay.pure(())
          else okay.effect[R, Unit](Async.Run[Unit](() => throw f))
      else
        val i = pop()
        // `resume` runs the source's own code (a `Bind(Return(x), f)`
        // applies `f`), so a throw here is that source failing
        val node = try slot(i).resume catch case e: Throwable => { failed(e); null }
        if node == null then step()
        else (node: @unchecked) match
          case Free.Return(_) =>
            live -= 1
            step()
          // Writer tested first, for `Writer.uncons`'s reason
          case Inject(e) => split[Writer % A, Async](e)
            { w0 => (w0: @unchecked) match
                case Writer.Say(a) =>
                  live -= 1
                  okay.effect[R, Unit](Writer(a)).flatMap(_ => again()) }
            { g =>
                turn(i, g, _ => okay.pure[R, Unit](()))
                step() }
          case Bind(Inject(e), k) => split[Writer % A, Async](e)
            { w0 => (w0: @unchecked) match
                case Writer.Say(a) =>
                  // `k(())` is the source's code too: the element it
                  // told is still delivered, the source is dropped
                  try
                    slot(i) = k(())
                    if i != last then { last = i; streak = 0 }
                    streak += 1
                    if streak < quantum then pushFront(i)
                    else { streak = 0; pushBack(i) }
                  catch case e: Throwable => failed(e)
                  okay.effect[R, Unit](Writer(a)).flatMap(_ => again()) }
            { g =>
                turn(i, g, k)
                step() }

    /** source `i`'s Async operation: a Run is performed in its own turn
     * (it goes back to the FRONT), an Await answered during its own
     * registration likewise, and an Await that is not answered parks the
     * source until its callback publishes it. Anything the source's own
     * code throws on the way — the Run, the registration, the
     * continuation — drops that source (drain, then fail) */
    private def turn[X](i: Int, g: Async[X], k: X => Source[A]): Unit =
      try operate(i, g, k) catch case e: Throwable => failed(e)

    private def operate[X](i: Int, g: Async[X], k: X => Source[A]): Unit = g match
      case Async.Run(f) =>
        slot(i) = k(f())
        pushFront(i)
      case Async.Await(reg) =>
        val cell = AtomicReference[Answer[X] | Moved.type | Null](null)
        val c = reg { r =>
          if !cell.compareAndSet(null, Answer(r)) then
            // asynchronous: build the continuation WITHOUT running it —
            // `k` is the source's code, and only the drive runs that
            slot(i) = r match
              case Right(x) => okay.pure[R, X](x).flatMap(k)
              case Left(e) => okay.effect[R, X](Async.Run[X](() => throw e)).flatMap(k)
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
      okay.effect[R, Unit](Async.Await[Unit] { cb =>
        waker.set(cb)
        if !woken.isEmpty then fire()
        onPark()
        () => cancelAll()
      }).flatMap(_ => again())
