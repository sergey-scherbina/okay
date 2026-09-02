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
 * What the reference holds is one of three things: a `Stamped` value
 * (the version lives IN the value, no wrapper — the Channel's State
 * is one, so its fast path allocates nothing beyond the state it
 * would build anyway), a `Slot` wrapping any other value with its
 * version, or an `Owned` marker that a commit in flight has CAS'd
 * over the content — the fast path retries on it, another commit
 * fails its CAS on it, and nothing waits.
 */
final class TRef[A](init: A) {
  import TRef.*

  private[okay] val ref = AtomicReference[AnyRef](wrap(init, 0L))
  private[okay] val waiters = AtomicReference[List[Waiter]](Nil)

  /** a plain read, outside any transaction */
  def get: A = valueOf[A](ref.get)

  /** the cell's version: moves by one at every install */
  def version: Long = versionOf(ref.get)

  /** the one-cell transaction: f is PURE and may run more than once;
   * the answer b is yours to act on after — the Channel returns its
   * callbacks this way. A result that is the same object skips the
   * CAS; content owned by a commit in progress is retried (a few
   * instructions long, never a park) */
  @tailrec def modify[B](f: A => (A, B)): B =
    val cur = ref.get
    cur match
      case s: Stamped =>
        // the bare path: one type test, no unwrapping, no wrapper built
        val (a2, b) = f(s.asInstanceOf[A])
        if a2.asInstanceOf[AnyRef] eq s then b
        else
          val next = a2 match
            case n: Stamped => n.stamp = s.stamp + 1; n
            case other => Slot(other, s.stamp + 1)
          if ref.compareAndSet(cur, next) then { if waiters.get ne Nil then wake(); b }
          else modify(f)
      case _: Owned => modify(f)
      case _ =>
        val a = valueOf[A](cur)
        val (a2, b) = f(a)
        if a2.asInstanceOf[AnyRef] eq a.asInstanceOf[AnyRef] then b
        else if ref.compareAndSet(cur, wrap(a2, versionOf(cur) + 1)) then { wake(); b }
        else modify(f)

  @tailrec private[okay] def wake(): Unit =
    val ws = waiters.get
    if ws.nonEmpty then
      if waiters.compareAndSet(ws, Nil) then ws.reverse.foreach(_.fire())
      else wake()

  @tailrec private[okay] def watch(w: Waiter): Unit =
    val ws = waiters.get
    if !waiters.compareAndSet(ws, w :: ws) then watch(w)
}

object TRef {
  /** a value that carries its own version, so a cell holding it
   * installs it bare. The cell STAMPS it at install: a Stamped value
   * belongs to one cell and one install — build a new one for every
   * transition (an immutable case class does, through copy) */
  abstract class Stamped { private[okay] var stamp: Long = 0L }   // a class, not a trait: the type test is a primary-supers check, not an interface scan

  /** any other value, with its version */
  final class Slot[+A](val value: A, val version: Long)

  /** a commit in flight owns the content it found */
  private[okay] final class Owned(val inner: AnyRef, val token: AnyRef)

  private[okay] def wrap(a: Any, v: Long): AnyRef = a match
    case s: Stamped => s.stamp = v; s
    case other => Slot(other, v)

  private[okay] def valueOf[A](c: AnyRef): A = c match
    case s: Slot[?] => s.value.asInstanceOf[A]
    case o: Owned => valueOf[A](o.inner)
    case s => s.asInstanceOf[A]

  private[okay] def versionOf(c: AnyRef): Long = c match
    case s: Slot[?] => s.version
    case s: Stamped => s.stamp
    case o: Owned => versionOf(o.inner)

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
}

object Tx {
  def read[A](r: TRef[A]): A ! Tx = effect(Read(r))
  def write[A](r: TRef[A], a: A): Unit ! Tx = effect(Write(r, a))
  def modify[A, B](r: TRef[A])(f: A => (A, B)): B ! Tx = effect(Modify(r, f))
  def update[A](r: TRef[A])(f: A => A): Unit ! Tx = effect(Modify(r, (a: A) => (f(a), ())))
  def retry[A]: A ! Tx = effect(Retry())
  /** `retry` unless the condition holds */
  def check(cond: Boolean): Unit ! Tx = if cond then pure(()) else retry
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

  private final class Log {
    val reads = mutable.ArrayBuffer.empty[(TRef[Any], Long)]
    val writes = mutable.LinkedHashMap.empty[TRef[Any], Any]   // TRef has identity equality

    /** everything read so far still at the version it was read at,
     * and none of it owned by a commit in flight */
    def valid: Boolean =
      var i = 0
      var ok = true
      while ok && i < reads.length do
        val (r, v) = reads(i)
        val c = r.ref.get
        ok = !c.isInstanceOf[TRef.Owned] && TRef.versionOf(c) == v
        i += 1
      ok
  }

  /** one operation against the log; a torn read aborts (Abort), a
   * retry surfaces as RetryNow — both control flow, both caught by
   * the handler that owns the attempt */
  private def perform(op: Tx[Any], log: Log): Any = op match
    case Tx.Read(r0) =>
      val r = r0.asInstanceOf[TRef[Any]]
      log.writes.get(r) match
        case Some(a) => a
        case None =>
          val c = r.ref.get
          if c.isInstanceOf[TRef.Owned] || !log.valid then throw Abort
          log.reads += ((r, TRef.versionOf(c)))
          TRef.valueOf[Any](c)
    case Tx.Write(r0, a) =>
      log.writes(r0.asInstanceOf[TRef[Any]]) = a
      ()
    case Tx.Modify(r0, f0) =>
      val r = r0.asInstanceOf[TRef[Any]]
      val a = perform(Tx.Read(r), log)
      val (a2, b) = f0.asInstanceOf[Any => (Any, Any)](a)
      log.writes(r) = a2
      b
    case Tx.Retry() => throw RetryNow

  /** run the whole program against the log, synchronously */
  private def interpret[A](tx: A ! Tx, log: Log): A =
    @tailrec def loop(p: Any ! Tx): Any = (p.resume: @unchecked) match
      case Pure(a) => a
      case Effect(e) => perform(e.asInstanceOf[Tx[Any]], log)
      case Bind(Effect(e), k) =>
        loop(k.asInstanceOf[Any => Any ! Tx](perform(e.asInstanceOf[Tx[Any]], log)))
    loop(tx.asInstanceOf[Any ! Tx]).asInstanceOf[A]

  /** own the write set by CAS, validate the read set, install, release —
   * or restore and answer false; nothing ever waits */
  private def commit(log: Log): Boolean =
    if log.writes.isEmpty then return log.valid
    val token = new AnyRef
    val owned = mutable.ArrayBuffer.empty[(TRef[Any], AnyRef)]
    var ok = true
    val it = log.writes.keysIterator
    while ok && it.hasNext do
      val r = it.next()
      val c = r.ref.get
      if c.isInstanceOf[TRef.Owned] || !r.ref.compareAndSet(c, TRef.Owned(c, token)) then ok = false
      else owned += ((r, c))
    if ok then
      var i = 0
      while ok && i < log.reads.length do
        val (r, v) = log.reads(i)
        val c = r.ref.get
        ok = TRef.versionOf(c) == v && (c match
          case o: TRef.Owned => o.token eq token
          case _ => true)
        i += 1
    if ok then
      owned.foreach((r, c) => r.ref.set(TRef.wrap(log.writes(r), TRef.versionOf(c) + 1)))
      owned.foreach((r, _) => r.wake())
      true
    else
      owned.foreach((r, c) => r.ref.set(c))
      false

  /** the structural fast paths, shared by tl2 and direct: a program
   * that IS one operation needs no log */
  private def fast[A](tx: A ! Tx): Option[A ! Async] = (tx.resume: @unchecked) match
    case Pure(a) => Some(pure(a))
    case Effect(Tx.Modify(r, f)) =>
      Some(async(r.asInstanceOf[TRef[Any]].modify(f.asInstanceOf[Any => (Any, A)])))
    case Effect(Tx.Read(r)) => Some(async(r.get.asInstanceOf[A]))
    case _ => None

  /** park the transaction on its read set; the first change re-runs
   * it through `again` — on the committing thread, as a channel
   * hands a value to a waiting receiver */
  private def park(log: Log, again: () => Unit, k: Either[Throwable, Any] => Unit): Unit =
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
            park(log, () => attempt(tx, k), k.asInstanceOf[Either[Throwable, Any] => Unit])
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
        log.writes.foreach { (r, v) =>
          r.ref.set(TRef.wrap(v, TRef.versionOf(r.ref.get) + 1))
        }
        log.writes.keysIterator.foreach(_.wake())
        k(Right(a))
      catch
        case Abort => attempt(tx, k)   // cannot happen on one thread; stated
        case RetryNow =>
          park(log, () => attempt(tx, k), k.asInstanceOf[Either[Throwable, Any] => Unit])
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
        def step(e: Tx[Any]): Either[Throwable, Any] =
          try Right(perform(e, log))
          catch case t: Throwable => Left(t)
        def loop(p: Any ! Tx): A ! Sim.Op = (p.resume: @unchecked) match
          case Pure(a) =>
            if commit(log) then pure(a.asInstanceOf[A]) else attempt
          case Effect(e) =>
            Sim.yieldNow.flatMap(_ => step(e.asInstanceOf[Tx[Any]]) match
              case Right(a) => if commit(log) then pure(a.asInstanceOf[A]) else attempt
              case Left(Abort) => attempt
              case Left(RetryNow) => Sim.sleep(1).flatMap(_ => attempt)
              case Left(t) => throw t)
          case Bind(Effect(e), k) =>
            Sim.yieldNow.flatMap(_ => step(e.asInstanceOf[Tx[Any]]) match
              case Right(a) => loop(k.asInstanceOf[Any => Any ! Tx](a))
              case Left(Abort) => attempt
              case Left(RetryNow) => Sim.sleep(1).flatMap(_ => attempt)
              case Left(t) => throw t)
        loop(tx.asInstanceOf[Any ! Tx])
      attempt
}
