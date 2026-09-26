package okay.py

import okay.Handler

/**
 * The pool (stage 1): N resident workers behind ONE handler — the
 * parallelism story for a runtime whose GIL makes threads a dead
 * end. N processes hold N sets of imports; a virtual thread parks on
 * `take` when all are busy, which is the cluster's worker model, not
 * threads-under-GIL.
 *
 * Supervision is the parallel-resilience fault model: a worker that
 * DIES mid-call still throws to the caller (the in-flight answer is
 * gone and pretending otherwise would forge it), but the pool
 * replaces the corpse with a FRESH worker before rethrowing — the
 * caller's retry lands on live imports-cold state, correctness
 * unchanged, warmth re-earned.
 */
final class PyWorkers private (n: Int, python: String, env: Map[String, String])
                               /** the wire's format and compression every worker of the pool is opened with */
                               (using WireFormat, WireCompression, WireDeadline):

  private val pool = java.util.concurrent.ArrayBlockingQueue[ForeignWorker](n)

  private[py] def prime(): Unit =
    var i = 0
    while i < n do { pool.put(ForeignWorker.start(python, env)); i += 1 }

  /**
   * A PROGRAM lives in one worker (remote-foreign), and is routed there by
   * its run. A direct function's program (foreign-one-program: a node
   * marked `once`) is its parked Python frame, so its worker is kept OUT of
   * the pool until the function answers — another call would run nested
   * inside that frame; a program as data leaves its worker in the pool,
   * since its continuations are values.
   */
  private val runs = java.util.concurrent.ConcurrentHashMap[Long, (ForeignWorker, Boolean)]()

  /**
   * A HELD object ties its calls to its worker (foreign-object-handles):
   * a call naming a ref goes to the worker holding it, whatever the pool
   * is doing. The worker STAYS in the pool — a pool of one that held an
   * object must still answer a plain call — so every exchange with a
   * worker takes that worker's lock, and the pool only chooses which
   * worker an unpinned call gets. Ref ids are renamed pool-wide.
   */
  private val refs = java.util.concurrent.ConcurrentHashMap[Long, (ForeignWorker, Long)]()
  private val nextRef = java.util.concurrent.atomic.AtomicLong()

  /** the same shape as one worker's handler — programs cannot tell */
  def handler: Handler[ForeignEval] = new:
    def handle[A](e: ForeignEval[A]): A = e match
      case ForeignEval.Continue(run, k, a) =>
        val (w, checkedOut) = Option(runs.get(run)).getOrElse(
          throw IllegalArgumentException(s"okay.py: run $run is not known to this pool (forgotten, or answered)"))
        try stepped(w, run, checkedOut)(w.handler.handle(ForeignEval.Continue(run, k, a.map(local))))
        catch case dead: IllegalStateException if dead.getMessage.contains("DEAD") =>
          retire(w)
          throw dead
      case ForeignEval.Forget(run) =>
        Option(runs.remove(run)).foreach((w, checkedOut) =>
          try w.synchronized(w.handler.handle(ForeignEval.Forget(run)))
          finally if checkedOut then pool.put(w))
      case ForeignEval.Release(r) =>
        Option(refs.remove(r.id)).foreach { (w, local) =>
          w.synchronized(w.handler.handle(ForeignEval.Release(PyRef(local, r.pyType))))
        }
      case other =>
        owner(named(other)) match
          case Some(w) => on(w, other, fromPool = false)
          case None =>
            val w = pool.take()
            // a program decides after its first node whether it keeps the worker
            var back = other match
              case ForeignEval.Program(_, _, _, _, _) => false
              case _ => true
            try on(w, other, fromPool = true)
            catch
              case dead: IllegalStateException if dead.getMessage.contains("DEAD") =>
                back = false
                retire(w)
                throw dead
            finally if back then pool.put(w)

  /** run one operation on `w` under its lock, its refs renamed to the
   * worker's own and any ref it answers registered pool-wide */
  private def on[A](w: ForeignWorker, e: ForeignEval[A], fromPool: Boolean): A = e match
    case ForeignEval.Call(fn, args, held) =>
      val answer = w.synchronized(w.handler.handle(ForeignEval.Call(localAt(fn), args.map(local), held)))
      // a held answer is a ref of THIS worker: known pool-wide from now on
      if held then answer.map {
        case PyValue.Ref(r) => PyValue.Ref(register(w, r))
        case v => v
      } else answer
    case ForeignEval.Frame(fn, f, args) => w.synchronized(w.handler.handle(ForeignEval.Frame(fn, f, args.map(local))))
    case ForeignEval.Program(run, fn, args, cbs, d) =>
      // a worker reached through a ref was never taken from the pool
      runs.put(run, (w, false)): Unit
      stepped(w, run, fromPool)(w.handler.handle(ForeignEval.Program(run, fn, args.map(local), cbs, d)))
    case ForeignEval.Release(_) | ForeignEval.Continue(_, _, _) | ForeignEval.Forget(_) =>
      throw IllegalStateException("unreachable: continue, forget and release are routed by the handler")

  /** the refs an operation names */
  private def named[A](e: ForeignEval[A]): Vector[Long] = e match
    case ForeignEval.Call(fn, args, _) => atRefs(fn) ++ args.flatMap(refsIn)
    case ForeignEval.Frame(_, _, args) => args.flatMap(refsIn)
    case ForeignEval.Program(_, _, args, _, _) => args.flatMap(refsIn)
    case ForeignEval.Release(_) | ForeignEval.Continue(_, _, _) | ForeignEval.Forget(_) => Vector.empty

  private def refsIn(v: PyValue): Vector[Long] = PyValue.refs(v)

  /** the one worker holding every ref named, or none named at all */
  private def owner(ids: Vector[Long]): Option[ForeignWorker] =
    val ws = ids.distinct.map(i => Option(refs.get(i)).map(_._1).getOrElse(
      throw IllegalArgumentException(s"okay.py: ref $i is not held by this pool (released?)")))
    ws.distinct match
      case Vector() => None
      case Vector(w) => Some(w)
      case _ => throw IllegalArgumentException(
        s"okay.py: refs ${ids.distinct.mkString(", ")} live in different workers; one call reaches one process")

  private def localRef(r: PyRef): PyRef = PyRef(refs.get(r.id)._2, r.pyType)

  /** the ref an address names (a held object's method or attribute) */
  private def atRefs(fn: Address): Vector[Long] = fn match
    case Address.Method(r, _) => Vector(r.id)
    case Address.Attr(r, _) => Vector(r.id)
    case Address.Fn(_) => Vector.empty

  private def localAt(fn: Address): Address = fn match
    case Address.Method(r, n) => Address.Method(localRef(r), n)
    case Address.Attr(r, n) => Address.Attr(localRef(r), n)
    case other => other

  private def local(v: PyValue): PyValue = PyValue.rebuild(v) {
    case PyValue.Ref(r) => PyValue.Ref(localRef(r))
    case other => other
  }

  private def register(w: ForeignWorker, r: PyRef): PyRef =
    val g = nextRef.incrementAndGet()
    refs.put(g, (w, r.id)): Unit
    PyRef(g, r.pyType)

  /**
   * One step of a program on `w`, under its lock. A node marked `once` is a
   * parked frame: the worker stays out of the pool (it came from there) or
   * pinned (it did not). The function's answer, or a failure, ends that:
   * the worker goes back to where it came from. A program as data leaves
   * the worker in the pool after every step, found again by its run.
   */
  private def stepped(w: ForeignWorker, run: Long, checkedOut: Boolean)
                     (step: => Either[Condition, PyNode]): Either[Condition, PyNode] =
    val node =
      try w.synchronized(step)
      catch case t: Throwable =>
        // the run is over either way; a death is retired by the caller,
        // anything else gives a checked-out worker back
        runs.remove(run): Unit
        val dead = t.isInstanceOf[IllegalStateException] && Option(t.getMessage).exists(_.contains("DEAD"))
        if checkedOut && !dead then pool.put(w)
        throw t
    node match
      case Right(PyNode.Perform(_, _, _, true)) =>
        runs.put(run, (w, checkedOut)): Unit
      case Right(PyNode.Perform(_, _, _, false)) =>
        runs.put(run, (w, false)): Unit
        if checkedOut then pool.put(w)
      case _ =>
        // answered, or failed: a direct run is over; a data run keeps its
        // continuations (a Choice may continue one again) until forgotten
        val direct = Option(runs.get(run)).exists(_._2) || checkedOut
        if direct then
          runs.remove(run): Unit
          if checkedOut then pool.put(w)
    node

  /** a dead worker: its refs die with it (a later use is refused by
   * name), and a fresh worker takes its place in the pool */
  private def retire(w: ForeignWorker): Unit =
    refs.entrySet.removeIf(_.getValue._1 eq w): Unit
    runs.entrySet.removeIf(_.getValue._1 eq w): Unit
    // a worker reached through a ref may still be IN the pool: take it
    // out, or the pool would hold a corpse beside its replacement
    pool.remove(w): Unit
    w.close()
    pool.put(ForeignWorker.start(python, env))   // the supervisor's move

  /** verify on ONE worker — they are started identically, and the
   * environment either is or is not the one the program was written
   * against */
  def verify(packages: Map[String, String]): Vector[String] =
    val w = pool.take()
    try w.verify(packages) finally pool.put(w)

  def close(): Unit =
    var i = 0
    while i < n do
      val w = pool.poll()
      if w != null then w.close()
      i += 1

object PyWorkers:
  def start(n: Int, python: String = "python3",
            env: Map[String, String] = Map.empty,
            /** inline modules every worker gets (foreign-inline-modules) */
            modules: Seq[PyModule] = Nil)(using WireFormat, WireCompression, WireDeadline): PyWorkers =
    require(n >= 1, "a pool needs a worker")
    val p = new PyWorkers(n, python, PyModule.env(modules, env))
    p.prime()
    p
