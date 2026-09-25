package okay.py

import okay.Handler
import scala.annotation.tailrec

/**
 * A foreign worker that COMES BACK (polyglot-one-wire stage 6): the same
 * handler shape as `ForeignWorker.handler`, over a worker that `open` makes
 * again after a death (an end of stream, a killed process, a dropped
 * connection) or a deadline that closed the wire.
 *
 * {{{
 * val w = ForeignWorker.supervised(ForeignWorker.connect("10.0.0.5", 7000))
 * given Handler[ForeignEval] = w.handler
 * }}}
 *
 * What survives, and why:
 *  - PROGRAMS AS DATA survive. A far-side program is a pure function of the
 *    answers it was given, so a continuation is described by its run's
 *    `(fn, args)` and the path of answers that reached it. After a restart
 *    the program is re-run on the fresh worker and the path replayed, which
 *    re-derives the continuation; a step that failed mid-flight is then
 *    redone. A replay that meets another operation than the path recorded
 *    is answered as `ReplayDrift`, never as a wrong value.
 *  - A PLAIN call (`Call`, `Frame`, a direct-style dialogue) caught in the
 *    failure answers `Left(Condition("WorkerDied" | "timeout", ...))`: the
 *    far side may have done the work before it went silent, and only the
 *    caller knows whether doing it twice is harmless — okay-platform's
 *    `retry` is the caller's move, not this class's.
 *  - A HELD object names state in one process: a ref from before a restart
 *    is refused by name (refs carry their generation), never re-pointed at
 *    whatever the fresh process numbers the same.
 */
final class SupervisedWorker private[py] (open: () => ForeignWorker):

  private var current: Option[ForeignWorker] = None
  private var generation = 0L
  private var opened = 0

  /** how many times a worker was REopened (the first open is not counted) */
  def restarts: Int = math.max(0, opened - 1)

  /** the worker to use: the current one while it lives, a fresh one after */
  private def worker(): Either[Condition, ForeignWorker] =
    current match
      case Some(w) if w.alive => Right(w)
      case _ =>
        current.foreach(_.close())
        current = None
        try
          val w = open()
          opened += 1
          generation += 1
          current = Some(w)
          Right(w)
        catch case e: Exception =>
          Left(Condition("WorkerUnavailable", s"the worker could not be (re)opened: ${e.getMessage}"))

  private def dead(e: Throwable): Boolean =
    e.isInstanceOf[ForeignWorker.TimedOut] || Option(e.getMessage).exists(_.contains("DEAD"))

  /** one operation on the live worker; a death becomes data */
  private def use[X](f: ForeignWorker => Either[Condition, X]): Either[Condition, X] =
    worker().flatMap { w =>
      try f(w)
      catch case e: IllegalStateException if dead(e) => Left(Condition("WorkerDied", e.getMessage))
    }

  // ---- refs and dialogue keys carry their generation --------------------

  private val Shift = 40
  private def expose(local: Long): Long = (generation << Shift) | local
  private def genOf(id: Long): Long = id >>> Shift
  private def localOf(id: Long): Long = id & ((1L << Shift) - 1)

  private def gone(what: String, id: Long): Condition =
    Condition("LookupError", s"$what $id belongs to a worker that is gone (restarted since): ask the fresh one again")

  /** a value going OUT: its refs must belong to the current worker */
  private def out(v: PyValue): Either[Condition, PyValue] = PyValue.rebuildE(v) {
    case PyValue.Ref(r) =>
      if genOf(r.id) == generation && current.exists(_.alive) then Right(PyValue.Ref(r.copy(id = localOf(r.id))))
      else Left(gone("the held object", r.id))
    case other => Right(other)
  }

  private def outAll(xs: Vector[PyValue]): Either[Condition, Vector[PyValue]] =
    Walk.sequence(xs.map(out))

  private def outRef(r: PyRef): Either[Condition, PyRef] =
    out(PyValue.Ref(r)).map { case PyValue.Ref(l) => l; case _ => r }

  /** a value coming IN: its refs are renamed into this generation */
  private def in(v: PyValue): PyValue = PyValue.rebuild(v) {
    case PyValue.Ref(r) => PyValue.Ref(r.copy(id = expose(r.id)))
    case other => other
  }

  // ---- programs as data: every continuation remembers how it was reached --

  /** a continuation the CALLER holds: its run, the path of answers from the
   * program's start, the operation it stands at, and where it lives now */
  private final case class Kont(run: Long, path: Vector[PyValue], op: String, args: Vector[PyValue],
                                var local: Long, var gen: Long,
                                /** a parked frame (the direct style): continued once, never replayed */
                                once: Boolean = false)

  private val runs = scala.collection.mutable.Map.empty[Long, (String, Vector[PyValue], Vector[String])]
  private val konts = scala.collection.mutable.Map.empty[Long, Kont]
  private var nextK = 0L

  /** a node from the worker, its `k` renamed to one the caller can keep */
  private def node(run: Long, path: Vector[PyValue], n: PyNode): PyNode = n match
    case PyNode.Done(v) => PyNode.Done(in(v))
    case PyNode.Perform(op, args, k, once) =>
      nextK += 1
      konts(nextK) = Kont(run, path, op, args, k, generation, once)
      PyNode.Perform(op, args.map(in), nextK, once)

  /** on the CURRENT worker: the program re-run and `path` replayed, to the
   * continuation standing at `op(args)`; its local k */
  private def replay(w: ForeignWorker, run: Long, path: Vector[PyValue], op: String,
                     args: Vector[PyValue]): Either[Condition, Long] =
    val (fn, fnArgs, cbs) = runs(run)
    // one loop over the recorded path, not a frame per answer: a
    // durable run replays as many steps as it journaled (stack-safety-py-r)
    def step(n0: Either[Condition, PyNode], rest0: Vector[PyValue]): Either[Condition, Long] =
      var n = n0
      var rest = rest0
      var result: Option[Either[Condition, Long]] = None
      while result.isEmpty do n match
        case Left(c) => result = Some(Left(c))
        case Right(PyNode.Perform(o, as, k, _)) if rest.isEmpty =>
          result = Some(if o == op && as == args then Right(k)
            else Left(Condition("ReplayDrift",
              s"replaying run $run met $o$as where the path recorded $op$args: the far-side program is not a pure function of its answers")))
        case Right(PyNode.Perform(_, _, k, _)) =>
          n = w.handler.handle(ForeignEval.Continue(run, k, Right(rest.head))); rest = rest.tail
        case Right(PyNode.Done(v)) =>
          result = Some(Left(Condition("ReplayDrift", s"replaying run $run finished ($v) before the recorded path did")))
      result.get
    val started = outAll(fnArgs).flatMap(a => w.handler.handle(ForeignEval.Program(run, fn, a, cbs)))
    step(started, path)

  /** continue `k` with `answer`, recovering a lost worker by replay; one
   * restart per step, so a far side that dies every time still answers */
  private def continue(k: Long, answer: Either[Condition, PyValue]): Either[Condition, PyNode] =
    konts.get(k) match
      case None => Left(Condition("LookupError", s"continuation $k is not held (forgotten?)"))
      case Some(c) if c.once =>
        // a parked frame: continued once, on the worker that parked it
        konts.remove(k): Unit
        if c.gen != generation || !current.exists(_.alive) then
          Left(Condition("WorkerDied",
            s"the call waiting on $k was in a worker that died: its far-side frame is gone, so the call cannot be resumed"))
        else
          val sent = answer match
            case Right(v) => out(v).map(Right(_))
            case Left(cond) => Right(Left(cond))
          sent.flatMap(a => use(w => w.handler.handle(ForeignEval.Continue(c.run, c.local, a))))
            .map(n => node(c.run, c.path, n))
      case Some(c) => answer match
        // a program as data takes answers, not failures: the API stops at a Left
        case Left(cond) => Left(cond)
        case Right(value) =>
          @tailrec def attempt(recovering: Boolean): Either[Condition, PyNode] =
            use { w =>
              val local =
                if c.gen == generation then Right(c.local)
                else replay(w, c.run, c.path, c.op, c.args).map { l => c.local = l; c.gen = generation; l }
              for
                l <- local
                a <- out(value)
                n <- w.handler.handle(ForeignEval.Continue(c.run, l, Right(a)))
              yield node(c.run, c.path :+ value, n)
            } match
              case Left(cond) if !recovering && !current.exists(_.alive) &&
                Set("WorkerDied", "timeout").contains(cond.kind) => attempt(recovering = true)
              case other => other
          attempt(recovering = false)

  // ---- a host that did not see its own past -------------------------------

  /**
   * What `Durable.over(..., replayed = supervised.witness)` tells this
   * supervisor on a resumed HOST (foreign-workflow stage 3): each operation
   * the journal answered, with that answer. The program nodes rebuild the
   * continuation table the crashed host had. Every rebuilt continuation
   * belongs to NO live worker (generation -1), so the first live `Continue`
   * re-derives it on the fresh far side by replaying its path: Durable's
   * answers from the journal, then this supervisor's replay on the far
   * side. The ids are the journal's, so the caller's `k` still names the
   * same continuation.
   */
  val witness: [X] => (ForeignEval[X], X) => Unit = [X] => (op: ForeignEval[X], answer: X) => seen(op, answer)

  /** the operation tells which record this is; the ANSWER is read by its
   * shape, because `ForeignEval` is covariant and a match on the operation
   * bounds `X` only from below — a pattern on the value, not a cast */
  private def seen[X](op: ForeignEval[X], answer: X): Unit = op match
    case ForeignEval.Program(run, fn, args, cbs, _) =>
      runs(run) = (fn, args, cbs)
      rebuilt(run, Vector.empty, answer)
    case ForeignEval.Continue(run, k, Right(a)) =>
      konts.get(k).foreach(c => rebuilt(run, c.path :+ a, answer))
    case ForeignEval.Forget(run) =>
      runs.remove(run): Unit
      konts.filterInPlace((_, c) => c.run != run)
    case _ => ()

  /** a replayed node: a Perform's continuation, kept under the journal's
   * own id, standing on no live worker */
  private def rebuilt[X](run: Long, path: Vector[PyValue], node: X): Unit = node match
    case Right(PyNode.Perform(op, args, k, once)) =>
      konts(k) = Kont(run, path, op, args, local = -1L, gen = -1L, once = once)
      nextK = math.max(nextK, k)
    case _ => ()

  // ---- the handler -------------------------------------------------------


  def handler: Handler[ForeignEval] = new:
    def handle[A](e: ForeignEval[A]): A = e match
      case ForeignEval.Call(fn, args) =>
        use(w => outAll(args).flatMap(a => w.handler.handle(ForeignEval.Call(fn, a)))).map(in)
      case ForeignEval.Frame(fn, frame, args) =>
        use(w => outAll(args).flatMap(a => w.handler.handle(ForeignEval.Frame(fn, frame, a))))
      case ForeignEval.Hold(fn, args) =>
        use(w => outAll(args).flatMap(a => w.handler.handle(ForeignEval.Hold(fn, a))))
          .map(r => r.copy(id = expose(r.id)))
      case ForeignEval.Method(r, name, args, h) =>
        use(w => for
          l <- outRef(r)
          a <- outAll(args)
          v <- w.handler.handle(ForeignEval.Method(l, name, a, h))
        yield in(v))
      case ForeignEval.Attr(r, name) =>
        use(w => outRef(r).flatMap(l => w.handler.handle(ForeignEval.Attr(l, name)))).map(in)
      case ForeignEval.Release(r) =>
        if genOf(r.id) == generation then
          current.filter(_.alive).foreach(w =>
            try w.handler.handle(ForeignEval.Release(r.copy(id = localOf(r.id))))
            catch case e: IllegalStateException if dead(e) => ())
      case ForeignEval.Program(run, fn, args, cbs, direct) =>
        runs(run) = (fn, args, cbs)
        // a start that died mid-flight is re-run on a fresh worker unless the
        // host said it is DIRECT code, which may have acted before it died
        @tailrec def attempt(recovering: Boolean): Either[Condition, PyNode] =
          use(w => outAll(args).flatMap(a => w.handler.handle(ForeignEval.Program(run, fn, a, cbs, direct)))) match
            case Left(c) if !direct && !recovering && !current.exists(_.alive) &&
              Set("WorkerDied", "timeout").contains(c.kind) => attempt(recovering = true)
            case other => other
        attempt(recovering = false).map(n => node(run, Vector.empty, n))
      case ForeignEval.Continue(_, k, answer) =>
        continue(k, answer)
      case ForeignEval.Forget(run) =>
        runs.remove(run): Unit
        konts.filterInPlace((_, c) => c.run != run)
        current.filter(_.alive).foreach(w =>
          try w.handler.handle(ForeignEval.Forget(run))
          catch case e: IllegalStateException if dead(e) => ())

  /** what the CURRENT worker's handshake settled on ("" before the first open) */
  def wire: String = current.fold("")(_.wire)

  /** the current worker's Arrow frames, (sent, answered) */
  def arrowFrames: (Long, Long) = current.fold((0L, 0L))(_.arrowFrames)

  /** `ForeignWorker.verify` on the worker, opened if it has to be */
  def verify(packages: Map[String, String]): Vector[String] =
    worker().fold(c => Vector(s"verify itself failed: ${c.kind}: ${c.message}"), _.verify(packages))

  def close(): Unit =
    current.foreach(_.close())
    current = None
