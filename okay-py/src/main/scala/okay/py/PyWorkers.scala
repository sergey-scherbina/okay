package okay.py

import okay.Handler

/**
 * Workers behind ONE handler: the ONE pool (`Pool`, foreign-one-pool) with
 * what routing a foreign program needs on top — the parallelism story for a
 * runtime whose GIL makes threads a dead end. N processes hold N sets of
 * imports; a caller parks while all are busy, which is the cluster's worker
 * model, not threads-under-GIL. Every language's workers pool the same way:
 * `PyWorkers.over(pool)` takes any pool of `ForeignWorker`s (R's included).
 *
 * Supervision is the parallel-resilience fault model: a worker that DIES
 * mid-call still throws to the caller (the in-flight answer is gone and
 * pretending otherwise would forge it); the pool closes the corpse, and the
 * next caller gets a FRESH worker — correctness unchanged, warmth
 * re-earned. A ref or a run that lived in the dead worker is refused by
 * name after.
 */
final class PyWorkers private (val pool: Pool[ForeignWorker]):

  private type Lease = Pool[ForeignWorker]#Lease

  /**
   * A PROGRAM lives in one worker (remote-foreign), and is routed there by
   * its run. A direct function's program (foreign-one-program: a node
   * marked `once`) is its parked frame, so its worker stays LEASED until the
   * function answers — another call would run nested inside that frame; a
   * program as data gives its worker back after every step, since its
   * continuations are values.
   */
  private val runs = java.util.concurrent.ConcurrentHashMap[Long, (ForeignWorker, Option[Lease])]()

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

  private def dead(t: Throwable): Boolean =
    t.isInstanceOf[IllegalStateException] && Option(t.getMessage).exists(_.contains("DEAD"))

  /** the same shape as one worker's handler — programs cannot tell */
  def handler: Handler[ForeignEval] = new:
    def handle[A](e: ForeignEval[A]): A = e match
      case ForeignEval.Continue(run, k, a) =>
        val (w, lease) = Option(runs.get(run)).getOrElse(
          throw IllegalArgumentException(s"okay.py: run $run is not known to this pool (forgotten, or answered)"))
        stepped(w, run, lease)(w.handler.handle(ForeignEval.Continue(run, k, a.map(local))))
      case ForeignEval.Forget(run) =>
        Option(runs.remove(run)).foreach((w, lease) =>
          try w.synchronized(w.handler.handle(ForeignEval.Forget(run)))
          finally lease.foreach(_.release(!w.alive)))
      case ForeignEval.Release(r) =>
        Option(refs.remove(r.id)).foreach { (w, local) =>
          w.synchronized(w.handler.handle(ForeignEval.Release(PyRef(local, r.pyType))))
        }
      case other =>
        owner(named(other)) match
          case Some(w) => on(w, other, None)
          case None =>
            val l = pool.lease()
            // a program decides after its first node whether it keeps the worker
            val keeps = other match
              case ForeignEval.Program(_, _, _, _, _) => true
              case _ => false
            var failed: Option[Throwable] = None
            try on(l.e, other, Option.when(keeps)(l))
            catch case t: Throwable => { failed = Some(t); throw t }
            finally if !keeps || failed.nonEmpty then
              if failed.exists(dead) then forgetWorker(l.e)
              l.release(failed.exists(dead) || !l.e.alive)

  /** run one operation on `w` under its lock, its refs renamed to the
   * worker's own and any ref it answers registered pool-wide */
  private def on[A](w: ForeignWorker, e: ForeignEval[A], lease: Option[Lease]): A = e match
    case ForeignEval.Call(fn, args, held) =>
      val answer = w.synchronized(w.handler.handle(ForeignEval.Call(localAt(fn), args.map(local), held)))
      // a held answer is a ref of THIS worker: known pool-wide from now on
      if held then answer.map {
        case PyValue.Ref(r) => PyValue.Ref(register(w, r))
        case v => v
      } else answer
    case ForeignEval.Frame(fn, f, args) => w.synchronized(w.handler.handle(ForeignEval.Frame(fn, f, args.map(local))))
    case ForeignEval.Program(run, fn, args, cbs, d) =>
      runs.put(run, (w, lease)): Unit
      stepped(w, run, lease)(w.handler.handle(ForeignEval.Program(run, fn, args.map(local), cbs, d)))
    case ForeignEval.Release(_) | ForeignEval.Continue(_, _, _) | ForeignEval.Forget(_) =>
      throw IllegalStateException("unreachable: continue, forget and release are routed by the handler")

  /**
   * One step of a program on `w`, under its lock. A node marked `once` is a
   * parked frame: the worker stays leased (if it was) until the function
   * answers or fails; a program as data gives the lease back at once and is
   * found again by its run. A death ends the run and the worker's refs.
   */
  private def stepped(w: ForeignWorker, run: Long, lease: Option[Lease])
                     (step: => Either[Condition, PyNode]): Either[Condition, PyNode] =
    val node =
      try w.synchronized(step)
      catch case t: Throwable =>
        runs.remove(run): Unit
        if dead(t) then forgetWorker(w)
        lease.foreach(_.release(dead(t) || !w.alive))
        throw t
    node match
      case Right(PyNode.Perform(_, _, _, true)) =>
        runs.put(run, (w, lease)): Unit
      case Right(PyNode.Perform(_, _, _, false)) =>
        runs.put(run, (w, None)): Unit
        lease.foreach(_.release(!w.alive))
      case _ =>
        // answered, or failed: a direct run is over; a data run keeps its
        // continuations (a Choice may continue one again) until forgotten
        if lease.nonEmpty then
          runs.remove(run): Unit
          lease.foreach(_.release(!w.alive))
    node

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

  /** a dead worker's refs and runs die with it (a later use is refused by
   * name); the pool itself closes it and opens a fresh one on demand */
  private def forgetWorker(w: ForeignWorker): Unit =
    refs.entrySet.removeIf(_.getValue._1 eq w): Unit
    runs.entrySet.removeIf(_.getValue._1 eq w): Unit

  /**
   * One exchange on a borrowed worker, for what is not an operation of the
   * effect (a Table through Arrow, the worker's `wire`, a per-worker cache):
   * a death forgets the worker's refs and runs, and still THROWS.
   */
  def use[X](f: ForeignWorker => X): X =
    pool.use { w =>
      try (w.synchronized(f(w)), !w.alive)
      catch case t: Throwable =>
        if dead(t) then forgetWorker(w)
        throw t
    }

  /** a worker kept for longer than an exchange (a stage holding a
   * partition's state), outside the routing: released by the caller */
  def lease(): Lease = pool.lease()

  /** verify on ONE worker — they are started identically, and the
   * environment either is or is not the one the program was written
   * against */
  def verify(packages: Map[String, String]): Vector[String] = use(_.verify(packages))

  def close(): Unit = pool.closeAll()

object PyWorkers:
  /** `n` Python workers, all started now */
  def start(n: Int, python: String = "python3",
            env: Map[String, String] = Map.empty,
            /** inline modules every worker gets (foreign-inline-modules) */
            modules: Seq[PyModule] = Nil)(using WireFormat, WireCompression, WireDeadline): PyWorkers =
    require(n >= 1, "a pool needs a worker")
    val envAll = PyModule.env(modules, env)
    val p = Pool[ForeignWorker](s"py:$python", n, () => ForeignWorker.start(python, envAll), _.alive, _.close())
    p.prime()
    new PyWorkers(p)

  /** the routing over any pool of workers — R's, a cluster stage's */
  def over(pool: Pool[ForeignWorker]): PyWorkers = new PyWorkers(pool)
