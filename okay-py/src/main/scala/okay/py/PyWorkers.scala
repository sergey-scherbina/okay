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
                               (using WireFormat, WireCompression):

  private val pool = java.util.concurrent.ArrayBlockingQueue[ForeignWorker](n)

  private[py] def prime(): Unit =
    var i = 0
    while i < n do { pool.put(ForeignWorker.start(python, env)); i += 1 }

  /**
   * A call with callbacks is a DIALOGUE (foreign-callbacks): `Start`,
   * then a `Resume` per ask, all with ONE worker — its Python frame is
   * the one waiting. A dialogue that took its worker from the pool keeps
   * it until the `Done`; each ask's `k` is renamed to a pool-wide one so a
   * resume finds its worker.
   */
  private val parked = java.util.concurrent.ConcurrentHashMap[Long, (ForeignWorker, Long, Boolean)]()
  private val nextK = java.util.concurrent.atomic.AtomicLong()

  /**
   * A HELD object ties its calls to its worker (foreign-object-handles):
   * a call naming a ref goes to the worker holding it, whatever the pool
   * is doing. The worker STAYS in the pool — a pool of one that held an
   * object must still answer a plain call — so every exchange with a
   * worker takes that worker's lock, and the pool only chooses which
   * worker an unpinned call gets. Ref ids are renamed pool-wide.
   */
  private val refs = java.util.concurrent.ConcurrentHashMap[Long, (ForeignWorker, Long)]()
  /** a program-as-data run's continuations live in one worker (remote-foreign) */
  private val runs = java.util.concurrent.ConcurrentHashMap[Long, ForeignWorker]()
  private val nextRef = java.util.concurrent.atomic.AtomicLong()

  /** the same shape as one worker's handler — programs cannot tell */
  def handler: Handler[ForeignEval] = new:
    def handle[A](e: ForeignEval[A]): A = e match
      case ForeignEval.Resume(k, a) =>
        val (w, local, fromPool) = Option(parked.remove(k)).getOrElse(
          throw IllegalStateException(s"okay.py: resume $k matches no waiting call (resumed twice?)"))
        dialogue(w, fromPool)(_.handle(ForeignEval.Resume(local, a)))
      case ForeignEval.Continue(run, k, a) =>
        val w = Option(runs.get(run)).getOrElse(
          throw IllegalArgumentException(s"okay.py: run $run is not known to this pool (forgotten?)"))
        w.synchronized(w.handler.handle(ForeignEval.Continue(run, k, local(a))))
      case ForeignEval.Forget(run) =>
        Option(runs.remove(run)).foreach(w => w.synchronized(w.handler.handle(ForeignEval.Forget(run))))
      case ForeignEval.Release(r) =>
        Option(refs.remove(r.id)).foreach { (w, local) =>
          w.synchronized(w.handler.handle(ForeignEval.Release(PyRef(local, r.pyType))))
        }
      case other =>
        owner(named(other)) match
          case Some(w) => on(w, other, fromPool = false)
          case None =>
            val w = pool.take()
            val dialogueKeepsIt = other match
              case ForeignEval.Start(_, _, _) => true
              case _ => false
            var back = !dialogueKeepsIt
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
    case ForeignEval.Start(fn, args, cbs) => dialogue(w, fromPool)(_.handle(ForeignEval.Start(fn, args.map(local), cbs)))
    case ForeignEval.Call(fn, args) => w.synchronized(w.handler.handle(ForeignEval.Call(fn, args.map(local))))
    case ForeignEval.Frame(fn, f, args) => w.synchronized(w.handler.handle(ForeignEval.Frame(fn, f, args.map(local))))
    case ForeignEval.Hold(fn, args) =>
      w.synchronized(w.handler.handle(ForeignEval.Hold(fn, args.map(local)))).map(register(w, _))
    case ForeignEval.Method(r, name, args, h) =>
      w.synchronized(w.handler.handle(ForeignEval.Method(localRef(r), name, args.map(local), h))).map {
        case PyValue.Ref(held) if h => PyValue.Ref(register(w, held))
        case v => v
      }
    case ForeignEval.Attr(r, name) => w.synchronized(w.handler.handle(ForeignEval.Attr(localRef(r), name)))
    case ForeignEval.Program(run, fn, args) =>
      runs.put(run, w): Unit
      w.synchronized(w.handler.handle(ForeignEval.Program(run, fn, args.map(local))))
    case ForeignEval.Resume(_, _) | ForeignEval.Release(_) | ForeignEval.Continue(_, _, _) | ForeignEval.Forget(_) =>
      throw IllegalStateException("unreachable: resume and release are routed by the handler")

  /** the refs an operation names */
  private def named[A](e: ForeignEval[A]): Vector[Long] = e match
    case ForeignEval.Call(_, args) => args.flatMap(refsIn)
    case ForeignEval.Frame(_, _, args) => args.flatMap(refsIn)
    case ForeignEval.Start(_, args, _) => args.flatMap(refsIn)
    case ForeignEval.Hold(_, args) => args.flatMap(refsIn)
    case ForeignEval.Method(r, _, args, _) => r.id +: args.flatMap(refsIn)
    case ForeignEval.Attr(r, _) => Vector(r.id)
    case ForeignEval.Program(_, _, args) => args.flatMap(refsIn)
    case ForeignEval.Resume(_, _) | ForeignEval.Release(_) | ForeignEval.Continue(_, _, _) | ForeignEval.Forget(_) => Vector.empty

  private def refsIn(v: PyValue): Vector[Long] = v match
    case PyValue.Ref(r) => Vector(r.id)
    case PyValue.Arr(xs) => xs.flatMap(refsIn)
    case PyValue.Dict(kv) => kv.flatMap(p => refsIn(p._2))
    case _ => Vector.empty

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

  private def local(v: PyValue): PyValue = v match
    case PyValue.Ref(r) => PyValue.Ref(localRef(r))
    case PyValue.Arr(xs) => PyValue.Arr(xs.map(local))
    case PyValue.Dict(kv) => PyValue.Dict(kv.map((k, x) => (k, local(x))))
    case other => other

  private def register(w: ForeignWorker, r: PyRef): PyRef =
    val g = nextRef.incrementAndGet()
    refs.put(g, (w, r.id)): Unit
    PyRef(g, r.pyType)

  /** one step of a dialogue on `w`: park it again on an ask; on the
   * answer, give it back to the pool if that is where it came from */
  private def dialogue(w: ForeignWorker, fromPool: Boolean)(step: Handler[ForeignEval] => PyStep): PyStep =
    try w.synchronized(step(w.handler)) match
      case PyStep.Ask(cb, args, local) =>
        val k = nextK.incrementAndGet()
        parked.put(k, (w, local, fromPool)): Unit
        PyStep.Ask(cb, args, k)
      case done =>
        if fromPool then pool.put(w)
        done
    catch
      case dead: IllegalStateException if dead.getMessage.contains("DEAD") =>
        retire(w)
        throw dead

  /** a dead worker: its refs die with it (a later use is refused by
   * name), and a fresh worker takes its place in the pool */
  private def retire(w: ForeignWorker): Unit =
    refs.entrySet.removeIf(_.getValue._1 eq w): Unit
    runs.entrySet.removeIf(_.getValue eq w): Unit
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
            modules: Seq[PyModule] = Nil)(using WireFormat, WireCompression): PyWorkers =
    require(n >= 1, "a pool needs a worker")
    val p = new PyWorkers(n, python, PyModule.env(modules, env))
    p.prime()
    p
