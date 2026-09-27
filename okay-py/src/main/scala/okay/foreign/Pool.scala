package okay.foreign

/**
 * A POOL of interpreters (foreign-one-pool: the ONE pool; before it the
 * cluster's `Pool`, `PyWorkers`' queue and the facade's handle pools were
 * three): `size` at most, opened on demand (or all at once, `prime`), one
 * borrowed per exchange (`use`) or kept longer (`lease` — a program in
 * flight, a partition's state), a dead one closed and replaced by the next
 * open. Idle interpreters are taken in the order they were given back, so
 * the load goes round the pool. A `ForeignWorker` is one pipe, and callers
 * run on threads.
 */
final class Pool[E](val name: String, size: Int, open: () => E, alive: E => Boolean, close: E => Unit):
  require(size > 0, "a pool holds at least one interpreter")
  private val slots = java.util.concurrent.Semaphore(size)
  private val idle = java.util.ArrayDeque[E]()
  private val lock = Object()
  private var openedTotal = 0
  private var openNow = 0
  private var diedTotal = 0
  private var restartedTotal = 0
  Pool.born(this)

  /** how many interpreters were ever opened, restarts included */
  def opened: Int = lock.synchronized(openedTotal)
  /** how many are open right now, idle or borrowed */
  def live: Int = lock.synchronized(openNow)
  /** how many were opened to REPLACE one that died (foreign-pool-metrics) */
  def restarts: Int = lock.synchronized(restartedTotal)
  /** how many are borrowed right now — an exchange, a program in flight,
   * a stateful stage's lease */
  def borrowed: Int = size - slots.availablePermits()

  /** one more interpreter: a restart when a death is still unreplaced;
   * heard on the calling thread, the one whose request needed it */
  private def opening(): E =
    val fresh = open()
    openedTotal += 1
    openNow += 1
    val restart = diedTotal > restartedTotal
    if restart then restartedTotal += 1
    Pool.heard(Pool.Heard.Opened(name, restart))
    fresh

  /** open every interpreter now rather than on first use */
  def prime(): Unit = lock.synchronized {
    while openNow < size do idle.addLast(opening())
  }

  /** one exchange's worth of interpreter: blocks while all `size` are busy.
   * `f` answers the value and whether the interpreter is DEAD after it */
  def use[X](f: E => (X, Boolean)): X =
    val l = lease()
    var dead = true
    try
      val (x, d) = f(l.e)
      dead = d
      x
    finally l.release(dead)

  /** an interpreter kept for LONGER than an exchange — a program whose
   * stack is parked there, a stage holding a partition's state; `release`
   * gives it back, or closes it when it is dead */
  final class Lease private[Pool] (val e: E):
    private var open = true
    def release(dead: Boolean): Unit =
      if open then
        open = false
        give(e, dead)
        slots.release()

  def lease(): Lease =
    slots.acquire()
    val e =
      try borrow()
      catch case t: Throwable => { slots.release(); throw t }
    Lease(e)

  private def borrow(): E = lock.synchronized {
    val e = idle.pollFirst()
    if e != null && alive(e) then e
    else
      if e != null then { close(e); openNow -= 1; diedTotal += 1 }
      opening()
  }

  private def give(e: E, dead: Boolean): Unit = lock.synchronized {
    if dead then { try close(e) catch case _: Exception => (); openNow -= 1; diedTotal += 1 }
    else idle.addLast(e)
  }

  def closeAll(): Unit = lock.synchronized {
    idle.forEach(e => try close(e) catch case _: Exception => ())
    openNow -= idle.size
    idle.clear()
  }

/** what the pools of this JVM do, for whoever measures (foreign-pool-metrics):
 * a listener hears every open on the thread that caused it, and `held`
 * sums the interpreters every live pool holds and lends out */
object Pool:
  enum Heard:
    /** an interpreter opened; `restart` when it replaced a dead one */
    case Opened(pool: String, restart: Boolean)

  @volatile private var listeners: Vector[Heard => Unit] = Vector.empty
  private val alive = java.util.Collections.synchronizedMap(java.util.WeakHashMap[Pool[?], java.lang.Boolean]())

  /** hear every pool's events from now on */
  def listen(f: Heard => Unit): Unit = synchronized { listeners = listeners :+ f }

  private[foreign] def heard(h: Heard): Unit =
    listeners.foreach(f => try f(h) catch case _: Exception => ())

  private[foreign] def born(p: Pool[?]): Unit = alive.put(p, java.lang.Boolean.TRUE): Unit

  /** (open, borrowed) interpreters over every pool of this JVM */
  def held: (Int, Int) =
    val ps = alive.synchronized(java.util.ArrayList(alive.keySet))
    var open = 0
    var lent = 0
    ps.forEach(p => { open += p.live; lent += p.borrowed })
    (open, lent)

/** the pools of this JVM, one per (kind, interpreter, module), so every
 * caller naming the same module shares them; closed when the JVM exits */
object Pools:
  private val pools = scala.collection.mutable.LinkedHashMap.empty[String, (AnyRef, () => Unit)]

  def get[T <: AnyRef](key: String)(make: => T)(closing: T => Unit): T = synchronized {
    val (t, _) = pools.getOrElseUpdate(key, { val t = make; (t, () => closing(t)) })
    // the key names the kind, so the value under it IS that kind: the one
    // cast this registry needs, isolated here
    t.asInstanceOf[T]
  }

  def closeAll(): Unit = synchronized {
    pools.values.foreach((_, close) => try close() catch case _: Exception => ())
  }

  Runtime.getRuntime.addShutdownHook(Thread(() => closeAll(), "okay-foreign-pools"))
