package okay

/**
 * Native parks OS threads: that is the platform's ability, so
 * CanBlock is wait/notify and a fiber is one honest thread (no Loom
 * here, no CompletableFuture in the javalib — a hand-rolled cell).
 */
given CanBlock = new:
  def block[A](register: (A => Unit) => (() => Unit)): A =
    val lock = new Object
    var done = false
    var v: Option[A] = None
    val cancel = register { a =>
      lock.synchronized:
        v = Some(a)
        done = true
        lock.notifyAll()
    }
    try
      lock.synchronized:
        while !done do lock.wait()
    catch case e: Throwable => { cancel(); throw e }
    v.get   // done implies Some: the callback wrote it under the lock

/** the timer: a thread sleeps for the duration; cancelling
 * interrupts it out of the sleep */
given Timer = new:
  def after(millis: Long)(k: () => Unit): () => Unit =
    val t = Thread(() =>
      try { Thread.sleep(millis); k() }
      catch case _: InterruptedException => ())
    t.start()
    () => t.interrupt()

/** one OS thread per fiber */
given Scheduler = new:
  def fork[A](prog: () => A ! Async): Fiber[A] =
    val cell = FiberCell[A]()
    val t = Thread(() =>
      try cell.complete(Right(prog().runWith))
      catch case e: Throwable => cell.complete(Left(e)))
    t.start()
    new Fiber[A]:
      def onComplete(k: Either[Throwable, A] => Unit): Unit = cell.subscribe(k)
      def cancel(): Unit = t.interrupt()

/** one result, many subscribers (stm-sessions, specs/stm.md): a
 * `TRef[State]` instead of a hand-rolled `synchronized` cell — the
 * mutation and "who gets notified" decision are one `modify`, and
 * firing the callbacks OUTSIDE it (never inside `f`, which may run
 * more than once) is exactly the shape `TRef.modify`'s own doc
 * comment names: "the Channel returns its callbacks this way" */
private final class FiberCell[A]:
  private case class State(result: Option[Either[Throwable, A]] = None,
                           subs: List[Either[Throwable, A] => Unit] = Nil)
  private val cell = TRef(State())

  def complete(r: Either[Throwable, A]): Unit =
    val toRun = cell.modify { s =>
      if s.result.isDefined then (s, Nil) else (State(Some(r)), s.subs)
    }
    toRun.foreach(_(r))

  def subscribe(k: Either[Throwable, A] => Unit): Unit =
    val now = cell.modify { s =>
      s.result match
        case done @ Some(_) => (s, done)
        case None => (s.copy(subs = k :: s.subs), None)
    }
    now.foreach(k)
