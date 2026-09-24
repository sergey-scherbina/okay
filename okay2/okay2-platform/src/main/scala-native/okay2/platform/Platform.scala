package okay2.platform

import okay2._
import okay2.async._

/**
 * Scala Native's platform — the Scala 3 core's `scala-native/Platform.scala`
 * (okay2-cross). Native parks OS threads: that is the platform's
 * ability, so `CanBlock` is wait/notify and a fiber is one honest
 * thread — no Loom here, and no `CompletableFuture` assumed in the
 * javalib, so the cells are hand-rolled.
 */
object Platform {

  val canBlock: CanBlock = new CanBlock {
    def block[A](register: (A => Unit) => (() => Unit)): A = {
      val lock = new Object
      var done = false
      var v: Option[A] = None
      val cancel = register { a =>
        lock.synchronized {
          v = Some(a)
          done = true
          lock.notifyAll()
        }
      }
      // the interrupt is read FIRST, before the answer, as the JVM
      // platform does: an answer that is already there must not be
      // taken by a caller that has already been cancelled
      try {
        if (Thread.interrupted()) throw new InterruptedException()
        lock.synchronized { while (!done) lock.wait() }
      } catch { case e: Throwable => cancel(); throw e }
      v.get // done implies Some: the callback wrote it under the lock
    }

    def blockAccepted(register: Accepted => (() => Unit)): Boolean = {
      val lock = new Object
      var done = false
      var v = false
      val cancel = register(new Accepted {
        def apply(a: Boolean): Unit = lock.synchronized {
          v = a
          done = true
          lock.notifyAll()
        }
      })
      try {
        if (Thread.interrupted()) throw new InterruptedException()
        lock.synchronized { while (!done) lock.wait() }
      } catch { case e: Throwable => cancel(); throw e }
      v
    }

    def handoff[A](): Handoff[A] = new MonitorHandoff[A]

    def await(h: Handoff[_]): Unit =
      if (Thread.interrupted()) throw new InterruptedException()
      else if (!h.filled) h match {
        case m: MonitorHandoff[_] => m.synchronized { while (!m.filled) m.wait() }
        case other => throw new IllegalStateException("a handoff not made by this CanBlock: " + other.getClass.getName)
      }
  }

  /** the timer: a thread sleeps for the duration; cancelling interrupts
   * it out of the sleep */
  val timer: Timer = new Timer {
    def after(millis: Long)(k: () => Unit): () => Unit = {
      val t = new Thread(() => try { Thread.sleep(millis); k() } catch { case _: InterruptedException => () })
      t.start()
      () => t.interrupt()
    }
  }

  val scheduler: Scheduler = Schedulers.threads

  val net: Net = new Net {
    def connect(host: String, port: Int): NetConn ! Async = Async {
      val s = new java.net.Socket(host, port)
      s.setTcpNoDelay(true)
      new SocketConn(s)
    }
  }
}

/** the Native handoff: the object is its own monitor; `filled` is
 * written before the waiter is notified */
private final class MonitorHandoff[A] extends Handoff[A] {
  protected def signal(): Unit = this.synchronized { this.notifyAll() }
}

/**
 * Native schedulers. `threads` is one OS thread per fiber and the
 * default: a fiber that genuinely blocks on a SHARED pool thread can
 * starve every worker at once, so `pool` is opt-in, sized by a consumer
 * whose fibers do not park.
 */
object Schedulers {

  /** one OS thread per fiber */
  val threads: Scheduler = new Scheduler {
    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val cell = new FiberCell[A]
      val t = new Thread(() =>
        try cell.complete(Right(Effects.runFree(prog())(Async.handler(Platform.canBlock))))
        catch { case e: Throwable => cell.complete(Left(e)) })
      t.start()
      new Fiber[A] {
        def onComplete(k: Either[Throwable, A] => Unit): Unit = cell.subscribe(k)
        def cancel(): Unit = t.interrupt()
      }
    }
  }

  /** a fixed pool of worker threads pulling fiber-start tasks from a
   * hand-rolled queue — fibers become cheap; a park still costs a whole
   * worker for as long as it parks */
  def pool(size: Int = 4): Scheduler = new Scheduler {
    private val q = new TaskQueue
    for (_ <- 0 until math.max(1, size)) {
      val t = new Thread(() => while (true) q.take().run())
      t.setDaemon(true)
      t.start()
    }

    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val cell = new FiberCell[A]
      val task = new Task(() =>
        try cell.complete(Right(Effects.runFree(prog())(Async.handler(Platform.canBlock))))
        catch { case e: Throwable => cell.complete(Left(e)) })
      q.offer(task)
      new Fiber[A] {
        def onComplete(k: Either[Throwable, A] => Unit): Unit = cell.subscribe(k)
        // best effort: a queued task is skipped, a RUNNING one is
        // interrupted through exactly the worker running it
        def cancel(): Unit = task.cancel()
      }
    }
  }
}

/** one queued unit of work, cancellable while queued or running — the
 * runner is set only for the task actually executing on it */
private final class Task(body: () => Unit) {
  @volatile private var cancelled = false
  @volatile private var runner: Thread = null

  def run(): Unit =
    if (!cancelled) {
      runner = Thread.currentThread()
      if (!cancelled) body()
    }

  def cancel(): Unit = {
    cancelled = true
    val r = runner
    if (r != null) r.interrupt()
  }
}

/** a plain FIFO queue, hand-rolled: workers block in wait() */
private final class TaskQueue {
  private val lock = new Object
  private val q = scala.collection.mutable.Queue.empty[Task]

  def offer(task: Task): Unit = lock.synchronized {
    q.enqueue(task)
    lock.notify()
  }

  def take(): Task = lock.synchronized {
    while (q.isEmpty) lock.wait()
    q.dequeue()
  }
}

/** one result, many subscribers: a `TRef` holds the state, and the
 * callbacks fire OUTSIDE the modify, which may run more than once */
private final class FiberCell[A] {
  import FiberCell.State
  private val cell = TRef(State[A]())

  def complete(r: Either[Throwable, A]): Unit = {
    val toRun = cell.modify(s => if (s.result.isDefined) (s, Nil) else (State[A](Some(r)), s.subs))
    toRun.foreach(_(r))
  }

  def subscribe(k: Either[Throwable, A] => Unit): Unit = {
    val now = cell.modify(s => s.result match {
      case done @ Some(_) => (s, done)
      case None => (s.copy(subs = k :: s.subs), None)
    })
    now.foreach(k)
  }
}

private object FiberCell {
  /** in the companion: a case class nested in the generic cell trips
   * -Xlint's outer-reference check on every type test */
  final case class State[A](result: Option[Either[Throwable, A]] = None, subs: List[Either[Throwable, A] => Unit] = Nil)
}
