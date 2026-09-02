package okay

import okay.!.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec
import scala.collection.mutable

/**
 * The cell of the STM (specs/stm.md): a value with a version, in one
 * AtomicReference, plus a one-shot waiter list for `retry`. Its
 * `modify` IS the single-cell transaction — one CAS — and the path
 * every handler takes for a transaction that is one `Modify`; the
 * Channel's state lives in one of these.
 *
 * The reference holds ONE type, `Stamped[A]`: the value itself when
 * it carries its own version (a BARE cell — the Channel's State, so
 * its fast path allocates nothing beyond the state it would build
 * anyway), a `Slot` wrapping any other value (a WRAPPED cell), or an
 * `Owned` marker that a commit in flight has CAS'd over the content
 * and that mirrors the content's stamp and value. Which of bare or
 * wrapped a cell is, is decided at construction — `TRef(init)` wraps,
 * `TRef.bare(init)` needs `A <: Stamped[A]` — so nothing about a
 * value is ever guessed at runtime and nothing is cast. Owned is
 * matched only where its meaning differs: the fast path retries on
 * it, a transactional read aborts on it, another commit fails its
 * CAS on it. Nothing waits.
 */
sealed abstract class TRef[A] {
  import TRef.*

  private[okay] def ref: AtomicReference[Stamped[A]]
  private[okay] val waiters = AtomicReference[List[Waiter]](Nil)

  /** stamp and shape a value for this cell — bare or in a Slot, the
   * cell's kind */
  private[okay] def install(a: A, v: Long): Stamped[A]

  /** is the answer the content itself — "nothing changed", no CAS?
   * Only a bare cell can say yes */
  protected def unchanged(a: A, content: Stamped[A]): Boolean

  /** a plain read, outside any transaction */
  def get: A = ref.get.value

  /** the cell's version: moves by one at every install */
  def version: Long = ref.get.stamp

  /** the one-cell transaction: f is PURE and may run more than once;
   * the answer b is yours to act on after — the Channel returns its
   * callbacks this way. A bare cell whose answer IS the content skips
   * the CAS; a wrapped value always installs, an equal one included (a
   * version bump and a spurious wake-up, both harmless). Content owned
   * by a commit in progress is retried (a few instructions long, never
   * a park) */
  @tailrec final def modify[B](f: A => (A, B)): B =
    ref.get match
      case _: Owned[?] => modify(f)   // a commit is installing; spin, never park
      case s =>
        val (a2, b) = f(s.value)
        if unchanged(a2, s) then b
        else if ref.compareAndSet(s, install(a2, s.stamp + 1)) then { if waiters.get ne Nil then wake(); b }
        else modify(f)

  @tailrec private[okay] final def wake(): Unit =
    val ws = waiters.get
    if ws.nonEmpty then
      if waiters.compareAndSet(ws, Nil) then ws.reverse.foreach(_.fire())
      else wake()

  @tailrec private[okay] final def watch(w: Waiter): Unit =
    val ws = waiters.get
    if !waiters.compareAndSet(ws, w :: ws) then watch(w)
}

object TRef {
  /** a cell is its own typed token: the same cell holds the same type */
  given Same[TRef] = Same.byIdentity

  /** a cell for any value: the value travels in a Slot */
  def apply[A](init: A): TRef[A] = Wrapped(init)

  /** a cell for a value that carries its own version (`extends
   * TRef.Stamped[Self] { def value = this }`): installed bare, no
   * wrapper ever built — the Channel's kind */
  def bare[A <: Stamped[A]](init: A): TRef[A] = Bare(init)

  private final class Wrapped[A](init: A) extends TRef[A]:
    private[okay] val ref = AtomicReference[Stamped[A]](install(init, 0L))
    private[okay] def install(a: A, v: Long): Stamped[A] = { val s = Slot(a); s.stamp = v; s }
    protected def unchanged(a: A, content: Stamped[A]): Boolean = false

  private final class Bare[A <: Stamped[A]](init: A) extends TRef[A]:
    private[okay] val ref = AtomicReference[Stamped[A]](install(init, 0L))
    private[okay] def install(a: A, v: Long): Stamped[A] = { a.stamp = v; a }
    protected def unchanged(a: A, content: Stamped[A]): Boolean = a eq content

  /** what a cell holds: a value of A that carries its own version.
   * Extend it — `extends TRef.Stamped[Self] { def value = this }` —
   * and a bare cell installs your value as is; a wrapped cell puts
   * any value in a Slot, which is a Stamped too. So the cell's content
   * is always a Stamped[A], typed end to end. A bare cell STAMPS at
   * install: such a value belongs to one cell and one install — build
   * a new one for every transition (an immutable case class does,
   * through copy). A class, not a trait: the type test on the fast
   * path is a primary-supers check, not an interface scan (measured,
   * half the gap of the first cut) */
  abstract class Stamped[+A] {
    private[okay] var stamp: Long = 0L
    /** the value the cell holds: yourself, unless you are a wrapper */
    def value: A
  }

  /** the wrapper a wrapped cell puts its values in */
  final class Slot[+A](val value: A) extends Stamped[A]

  /** a commit in flight owns the content it found; it IS a Stamped[A]
   * — the content's stamp and value, seen through it — so the cell
   * has one type and only the places where ownership MEANS something
   * match on it */
  private[okay] final class Owned[+A](val inner: Stamped[A], val token: AnyRef) extends Stamped[A]:
    stamp = inner.stamp
    def value: A = inner.value

  /** fires at most once, however many cells it watches */
  final class Waiter(k: () => Unit):
    private val fired = AtomicBoolean(false)
    def fire(): Unit = if fired.compareAndSet(false, true) then k()
}

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
   * on whatever EITHER branch read (the classic STM combinator) */
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
    val reads = mutable.ArrayBuffer.empty[(TRef[?], Long)]
    private var writes = TMap.empty[TRef]

    /** the value this attempt has written to r, if any — this log's
     * own write, or (falling through) an enclosing one's */
    def pending[X](r: TRef[X]): Option[X] =
      writes.get(r).orElse(parent.flatMap(_.pending(r)))
    def write[X](r: TRef[X], v: X): Unit = writes = writes.updated(r, v)
    def hasWrites: Boolean = writes.nonEmpty
    def written: Iterator[TRef[?]] =
      def key[X](e: TMap.Entry[TRef, X]): TRef[?] = e.key
      writes.entries.map(e => key(e))

    /** a WINNING `orElse` branch's log, folded into this one: every
     * write it made becomes this log's own (typed through TMap's
     * polymorphic `foreach`, the one justified link between a cell
     * and what was written to it) */
    def absorb(child: Log): Unit =
      child.writes.foreach([X] => (r: TRef[X], v: X) => write(r, v))

    /** everything read so far still at the version it was read at,
     * and none of it owned by a commit in flight */
    def valid: Boolean =
      var i = 0
      var ok = true
      while ok && i < reads.length do
        val (r, v) = reads(i)
        val c = r.ref.get
        ok = !c.isInstanceOf[TRef.Owned[?]] && c.stamp == v
        i += 1
      ok

    /** install what this attempt wrote to r, at the next version */
    def installTo[X](r: TRef[X], v: Long): Unit =
      pending(r).foreach(x => r.ref.set(r.install(x, v)))
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
          log.reads += ((r, c.stamp))
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
      log.reads ++= branchA.reads   // read either way: a real retry blocks on it too
      ra match
        case Some(a) => log.absorb(branchA); a
        case None =>
          val branchB = new Log(parent = Some(log))
          try
            val b = runWithLog(progB, branchB)
            log.reads ++= branchB.reads
            log.absorb(branchB)
            b
          catch case RetryNow =>
            log.reads ++= branchB.reads
            throw RetryNow   // both branches retried: so does the whole thing

  /** run one program against a log, synchronously — the freer
   * tree's own Bind gives every step its type. Shared by the
   * top-level attempt loop AND `OrElse`'s branches (perform, above)
   * — not tail-recursive across that boundary, which is fine: a
   * transaction body is source code, not a loop counter. */
  private def runWithLog[A](tx: A ! Tx, log: Log): A =
    @tailrec def loop(p: A ! Tx): A = (p.resume: @unchecked) match
      case Pure(a) => a
      case Effect(e) => perform(e, log)
      case Bind(Effect(e), k) => loop(k(perform(e, log)))
    loop(tx)

  /** run the whole program against the log, synchronously */
  private def interpret[A](tx: A ! Tx, log: Log): A = runWithLog(tx, log)

  /** a cell a commit has taken: release it, or install into it */
  private final class Held[X](r: TRef[X], before: TRef.Stamped[X]):
    def release(): Unit = r.ref.set(before)
    def install(log: Log): Unit = log.installTo(r, before.stamp + 1)
    def wake(): Unit = r.wake()

  private def own[X](r: TRef[X], token: AnyRef): Option[Held[X]] =
    r.ref.get match
      case _: TRef.Owned[?] => None
      case s => if r.ref.compareAndSet(s, TRef.Owned(s, token)) then Some(Held(r, s)) else None

  /** own the write set by CAS, validate the read set, install, release —
   * or restore and answer false; nothing ever waits */
  private def commit(log: Log): Boolean =
    if !log.hasWrites then return log.valid
    val token = new AnyRef
    val owned = mutable.ArrayBuffer.empty[Held[?]]
    var ok = true
    val it = log.written
    while ok && it.hasNext do
      own(it.next(), token) match
        case Some(h) => owned += h
        case None => ok = false
    if ok then
      var i = 0
      while ok && i < log.reads.length do
        val (r, v) = log.reads(i)
        val c = r.ref.get
        ok = c.stamp == v && (c match
          case o: TRef.Owned[?] => o.token eq token
          case _ => true)
        i += 1
    if ok then
      owned.foreach(_.install(log))
      owned.foreach(_.wake())
      true
    else
      owned.foreach(_.release())
      false

  /** the structural fast paths, shared by tl2 and direct: a program
   * that IS one operation needs no log */
  private def fast[A](tx: A ! Tx): Option[A ! Async] = (tx.resume: @unchecked) match
    case Pure(a) => Some(pure(a))
    case Effect(Tx.Modify(r, f)) => Some(async(r.modify(f)))
    case Effect(Tx.Read(r)) => Some(async(r.get))
    case _ => None

  /** park the transaction on its read set; the first change re-runs
   * it through `again` — on the committing thread, as a channel
   * hands a value to a waiting receiver */
  private def park[A](log: Log, again: () => Unit, k: Either[Throwable, A] => Unit): Unit =
    if log.reads.isEmpty then
      k(Left(IllegalStateException("retry with nothing read: nothing could ever wake it")))
    else
      val w = TRef.Waiter(again)
      log.reads.foreach((r, _) => r.watch(w))
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
        log.written.foreach(r => log.installTo(r, r.version + 1))
        log.written.foreach(_.wake())
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
          case Effect(e) => Sim.yieldNow.flatMap(_ => after(step(e))(finish))
          case Bind(Effect(e), k) => Sim.yieldNow.flatMap(_ => after(step(e))(x => loop(k(x))))
        loop(tx)
      attempt
}
