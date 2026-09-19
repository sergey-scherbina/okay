package okay

import okay.!.*
import scala.annotation.tailrec
import scala.collection.mutable

/** the transaction language: no Async, no Run — I/O inside a
 * transaction is a compile error */
enum Tx[+A] {
  case Read[A](r: TRef[A]) extends Tx[A]
  case Write[A](r: TRef[A], a: A) extends Tx[Unit]
  case Modify[A, B](r: TRef[A], f: A => (A, B)) extends Tx[B]
  /** block until something this transaction READ changes, then run again */
  case Retry() extends Tx[Nothing]
  /** run `a`; if IT retries (not on any other failure), run `b`
   * instead — `a`'s writes are discarded, never committed, as if it
   * never ran. If `b` ALSO retries, the whole thing retries, parked
   * on whatever EITHER branch read (the classic STM combinator).
   *
   * SCOPED, not algebraic: the node carries computations, so it does
   * not commute with bind (docs/theory/05, "what the middle
   * constructor decides") — `perform` descends into the branches
   * itself, which is exactly what a generic relay could not do. Safe
   * because the payload is CLOSED over this signature: `A ! Tx`
   * cannot mention an ambient row, so no foreign operation can hide
   * in a branch and escape its handler. Widening it needs
   * higher-order signatures; read the price there first. */
  case OrElse[A](a: A ! Tx, b: A ! Tx) extends Tx[A]
}

object Tx {
  def read[A](r: TRef[A]): A ! Tx = effect(Read(r))
  def write[A](r: TRef[A], a: A): Unit ! Tx = effect(Write(r, a))
  def modify[A, B](r: TRef[A])(f: A => (A, B)): B ! Tx = effect(Modify(r, f))
  def update[A](r: TRef[A])(f: A => A): Unit ! Tx = effect(Modify(r, (a: A) => (f(a), ())))
  def retry[A]: A ! Tx = effect(Retry())
  /** `retry` unless the condition holds */
  def check(cond: Boolean): Unit ! Tx = if cond then pure(()) else retry
  /** `a`, or `b` if `a` retries — specs/stm.md, stm-orelse */
  def orElse[A](a: A ! Tx, b: A ! Tx): A ! Tx = effect(OrElse(a, b))
}

/** the door: WHERE a transaction runs, and by which strategy */
trait Stm[F[+_]] {
  def atomically[A](tx: A ! Tx): A ! F
}

object Stm {
  def apply[F[+_]](using s: Stm[F]): Stm[F] = s
  def atomically[A, F[+_]](tx: A ! Tx)(using s: Stm[F]): A ! F = s.atomically(tx)

  // ---- the shared machinery: a log, the interpreter, the commit ----

  private object Abort extends RuntimeException(null, null, false, false)
  private object RetryNow extends RuntimeException(null, null, false, false)

  /** the empty read set every log starts from — shared, never written */
  private val noRefs: Array[TRef[?]] = new Array[TRef[?]](0)
  private val noStamps: Array[Long] = new Array[Long](0)

  /** what one attempt has read (cell, version seen) and written. The
   * write set is a TMap keyed by the cells: a value written to a
   * TRef[X] comes back as an X, and the heterogeneous map's one
   * justified cast lives in TMap, not here. `parent`, set only for a
   * `OrElse` branch's own nested attempt (stm-orelse): a read not
   * pending in THIS log falls through to the enclosing one, so a
   * branch sees writes the transaction already made before reaching
   * the `orElse` — but a branch's OWN writes stay local until
   * `absorb`, so a retried branch leaves nothing behind. */
  private final class Log(parent: Option[Log] = None) {
    /**
     * The read set: the cells and the versions seen, as two parallel
     * arrays grown on demand, empty until the first read. It was an
     * `ArrayBuffer[(TRef[?], Long)]`: the buffer, its sixteen-slot
     * backing array, a tuple and a boxed version per read — together
     * the largest allocation of a Read-then-Write transaction by JFR
     * sample weight (stm-log-cost, §18f).
     */
    private var readRefs: Array[TRef[?]] = noRefs
    private var readStamps: Array[Long] = noStamps
    var nReads = 0
    def addRead(r: TRef[?], v: Long): Unit =
      if nReads == readRefs.length then
        val n = if nReads == 0 then 4 else nReads * 2
        readRefs = java.util.Arrays.copyOf(readRefs, n)
        readStamps = java.util.Arrays.copyOf(readStamps, n)
      readRefs(nReads) = r
      readStamps(nReads) = v
      nReads += 1
    def readRef(i: Int): TRef[?] = readRefs(i)
    def readStamp(i: Int): Long = readStamps(i)
    def hasReads: Boolean = nReads > 0
    /** a branch's reads become this log's: read either way, and a
     * real retry parks on them too */
    def addReads(child: Log): Unit =
      var i = 0
      while i < child.nReads do
        addRead(child.readRefs(i), child.readStamps(i))
        i += 1

    private var writes = TMap.empty[TRef]

    /** the value this attempt has written to r, if any — this log's
     * own write, or (falling through) an enclosing one's */
    def pending[X](r: TRef[X]): Option[X] =
      writes.get(r).orElse(parent.flatMap(_.pending(r)))
    def write[X](r: TRef[X], v: X): Unit = writes = writes.updated(r, v)
    def hasWrites: Boolean = writes.nonEmpty

    /** every pending write with its cell, typed, in one walk of the
     * write set — what a commit iterates. It was an `Iterator` built
     * from a reversed copy and a `map`, made twice per commit (install,
     * then wake) and walked once more to look each value up again;
     * a tenth of a transaction by CPU sample (stm-log-cost (b)) */
    def eachWrite(f: [X] => (TRef[X], X) => Unit): Unit = writes.foreachUnordered(f)

    /** the direct handler's commit: install every write at its cell's
     * next version, then wake each cell's waiters */
    def installAll(): Unit =
      eachWrite([X] => (r: TRef[X], v: X) => r.ref.set(r.install(v, r.version + 1)))
      eachWrite([X] => (r: TRef[X], _: X) => r.wake())

    /** a WINNING `orElse` branch's log, folded into this one: every
     * write it made becomes this log's own (typed through TMap's
     * polymorphic `foreach`, the one justified link between a cell
     * and what was written to it) */
    def absorb(child: Log): Unit =
      child.eachWrite([X] => (r: TRef[X], v: X) => write(r, v))

    /** everything read so far still at the version it was read at,
     * and none of it owned by a commit in flight */
    def valid: Boolean =
      var i = 0
      var ok = true
      while ok && i < nReads do
        val c = readRefs(i).ref.get
        ok = !c.isInstanceOf[TRef.Owned[?]] && c.stamp == readStamps(i)
        i += 1
      ok

  }

  /** one operation against the log; a torn read aborts (Abort), a
   * retry surfaces as RetryNow — both control flow, both caught by
   * the handler that owns the attempt */
  private def perform[X](op: Tx[X], log: Log): X = op match
    case Tx.Read(r) =>
      log.pending(r) match
        case Some(a) => a
        case None =>
          val c = r.ref.get
          if c.isInstanceOf[TRef.Owned[?]] || !log.valid then throw Abort
          log.addRead(r, c.stamp)
          c.value
    case Tx.Write(r, a) => log.write(r, a)
    case Tx.Modify(r, f) =>
      val (a2, b) = f(perform(Tx.Read(r), log))
      log.write(r, a2)
      b
    case Tx.Retry() => throw RetryNow
    case Tx.OrElse(progA, progB) =>
      val branchA = new Log(parent = Some(log))
      val ra = try Some(runWithLog(progA, branchA)) catch case RetryNow => None
      log.addReads(branchA)   // read either way: a real retry blocks on it too
      ra match
        case Some(a) => log.absorb(branchA); a
        case None =>
          val branchB = new Log(parent = Some(log))
          try
            val b = runWithLog(progB, branchB)
            log.addReads(branchB)
            log.absorb(branchB)
            b
          catch case RetryNow =>
            log.addReads(branchB)
            throw RetryNow   // both branches retried: so does the whole thing

  /** run one program against a log, synchronously — the freer
   * tree's own Bind gives every step its type. Shared by the
   * top-level attempt loop AND `OrElse`'s branches (perform, above)
   * — not tail-recursive across that boundary, which is fine: a
   * transaction body is source code, not a loop counter. */
  private def runWithLog[A](tx: A ! Tx, log: Log): A =
    @tailrec def loop(p: A ! Tx): A = (p.resume: @unchecked) match
      case Pure(a) => a
      case Inject(e) => perform(e, log)
      case Bind(Inject(e), k) => loop(k(perform(e, log)))
    loop(tx)

  /** run the whole program against the log, synchronously */
  private def interpret[A](tx: A ! Tx, log: Log): A = runWithLog(tx, log)

  /** a cell a commit has taken, with the value to install into it:
   * release it, or install */
  private final class Held[X](r: TRef[X], before: TRef.Stamped[X], value: X):
    def release(): Unit = r.ref.set(before)
    def install(): Unit = r.ref.set(r.install(value, before.stamp + 1))
    def wake(): Unit = r.wake()

  /** take the cell for this commit, or null if another commit holds
   * it (a typed null, not an Option: one per write per commit) */
  private def own[X](r: TRef[X], value: X, token: AnyRef): Held[X] | Null =
    r.ref.get match
      case _: TRef.Owned[?] => null
      case s => if r.ref.compareAndSet(s, TRef.Owned(s, token)) then Held(r, s, value) else null

  /** own the write set by CAS, validate the read set, install, release —
   * or restore and answer false; nothing ever waits */
  private def commit(log: Log): Boolean =
    if !log.hasWrites then return log.valid
    val token = new AnyRef
    val owned = mutable.ArrayBuffer.empty[Held[?]]
    var ok = true
    log.eachWrite([X] => (r: TRef[X], v: X) =>
      if ok then
        val h = own(r, v, token)
        if h != null then owned += h else ok = false)
    if ok then
      var i = 0
      while ok && i < log.nReads do
        val c = log.readRef(i).ref.get
        ok = c.stamp == log.readStamp(i) && (c match
          case o: TRef.Owned[?] => o.token eq token
          case _ => true)
        i += 1
    if ok then
      owned.foreach(_.install())
      owned.foreach(_.wake())
      true
    else
      owned.foreach(_.release())
      false

  /** the structural fast paths, shared by tl2 and direct: a program
   * that IS one operation needs no log */
  private def fast[A](tx: A ! Tx): Option[A ! Async] = (tx.resume: @unchecked) match
    case Pure(a) => Some(pure(a))
    case Inject(Tx.Modify(r, f)) => Some(async(r.modify(f)))
    case Inject(Tx.Read(r)) => Some(async(r.get))
    case _ => None

  /** park the transaction on its read set; the first change re-runs
   * it through `again` — on the committing thread, as a channel
   * hands a value to a waiting receiver */
  private def park[A](log: Log, again: () => Unit, k: Either[Throwable, A] => Unit): Unit =
    if !log.hasReads then
      k(Left(IllegalStateException("retry with nothing read: nothing could ever wake it")))
    else
      val w = TRef.Waiter(again)
      var i = 0
      while i < log.nReads do
        log.readRef(i).watch(w)
        i += 1
      // a change that slipped in between our reads and the watch
      if !log.valid then w.fire()

  /**
   * TL2-shaped: versions per cell, incremental validation on every
   * read (the body always holds a consistent snapshot), a CAS-owned
   * commit that never parks, `retry` as a parked transaction. The
   * handler for the parking platforms.
   */
  val tl2: Stm[Async] = new Stm[Async]:
    def atomically[A](tx: A ! Tx): A ! Async =
      fast(tx).getOrElse(Async.await { k => attempt(tx, k); () => () })

    private def attempt[A](tx: A ! Tx, k: Either[Throwable, A] => Unit): Unit =
      var done = false
      while !done do
        val log = new Log
        try
          val a = interpret(tx, log)
          if commit(log) then { done = true; k(Right(a)) }
        catch
          case Abort => ()
          case RetryNow =>
            done = true
            park(log, () => attempt(tx, k), k)
          case e: Throwable => done = true; k(Left(e))

  /**
   * One thread, a row that cannot suspend: a transaction is atomic by
   * construction. Writes are still buffered to the end so that a
   * `retry` after a write leaves nothing behind; no versions, no
   * validation, no ownership. The JS handler.
   */
  val direct: Stm[Async] = new Stm[Async]:
    def atomically[A](tx: A ! Tx): A ! Async =
      fast(tx).getOrElse(Async.await { k => attempt(tx, k); () => () })

    private def attempt[A](tx: A ! Tx, k: Either[Throwable, A] => Unit): Unit =
      val log = new Log
      try
        val a = interpret(tx, log)
        log.installAll()
        k(Right(a))
      catch
        case Abort => attempt(tx, k)   // cannot happen on one thread; stated
        case RetryNow => park(log, () => attempt(tx, k), k)
        case e: Throwable => k(Left(e))

  /**
   * Deterministic: the same transaction code under the Sim scheduler.
   * Every operation is preceded by a scheduling point, so the seeded
   * choice interleaves transactions at every step; the commit
   * validates versions exactly as tl2 does (single-threaded, so
   * ownership always succeeds and only validation can fail); `retry`
   * sleeps one virtual millisecond and runs again — time moves when
   * nothing else can, so a waited-for writer always gets its turn.
   */
  val sim: Stm[Sim.Op] = new Stm[Sim.Op]:
    def atomically[A](tx: A ! Tx): A ! Sim.Op =
      def attempt: A ! Sim.Op =
        val log = new Log
        def step[X](e: Tx[X]): Either[Throwable, X] =
          try Right(perform(e, log))
          catch case t: Throwable => Left(t)
        def finish(a: A): A ! Sim.Op = if commit(log) then pure(a) else attempt
        def after[X](outcome: Either[Throwable, X])(next: X => A ! Sim.Op): A ! Sim.Op =
          outcome match
            case Right(x) => next(x)
            case Left(Abort) => attempt
            case Left(RetryNow) => Sim.sleep(1).flatMap(_ => attempt)
            case Left(t) => throw t
        def loop(p: A ! Tx): A ! Sim.Op = (p.resume: @unchecked) match
          case Pure(a) => finish(a)
          case Inject(e) => Sim.yieldNow.flatMap(_ => after(step(e))(finish))
          case Bind(Inject(e), k) => Sim.yieldNow.flatMap(_ => after(step(e))(x => loop(k(x))))
        loop(tx)
      attempt
}

