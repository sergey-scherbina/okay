package okay

/**
 * Native parks OS threads: that is the platform's ability, so
 * CanBlock is wait/notify and a fiber is one honest thread (no Loom
 * here, no CompletableFuture in the javalib — a hand-rolled cell).
 */
given CanBlock = new:
  def block[A](register: (A => Unit) => Unit): A =
    val lock = new Object
    var done = false
    var v: A = null.asInstanceOf[A]
    register { a =>
      lock.synchronized:
        v = a
        done = true
        lock.notifyAll()
    }
    lock.synchronized:
      while !done do lock.wait()
    v

/** the timer: a thread sleeps for the duration */
given Timer = new:
  def after(millis: Long)(k: () => Unit): Unit =
    val t = Thread(() =>
      try { Thread.sleep(millis); k() }
      catch case _: InterruptedException => ())
    t.start()

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

/** one result, many subscribers */
private final class FiberCell[A]:
  private var result: Option[Either[Throwable, A]] = None
  private var subs: List[Either[Throwable, A] => Unit] = Nil

  def complete(r: Either[Throwable, A]): Unit =
    val run = synchronized:
      if result.isDefined then Nil
      else
        result = Some(r)
        val s = subs
        subs = Nil
        s
    run.foreach(_(r))

  def subscribe(k: Either[Throwable, A] => Unit): Unit =
    val now = synchronized:
      if result.isEmpty then subs = k :: subs
      result
    now.foreach(k)
