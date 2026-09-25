package okay2

import scala.annotation.tailrec
import scala.collection.mutable
import Free.{Return, Inject, Bind}
import okay2.async._

/**
 * THE TRANSACTION LANGUAGE: read, write and modify `TRef`s, `retry`
 * until something read changes, and `orElse` — the first branch that
 * does not retry. A `Tx` program is only a description; a `Stm` runs it
 * atomically (okay-stm, ported: specs/okay2.md stage 17).
 */
sealed trait Tx extends Row { type Op[+A] = Tx.Op[A] }

object Tx {
  sealed trait Op[+A]
  final case class Read[A](r: TRef[A]) extends Op[A]
  final case class Write[A](r: TRef[A], a: A) extends Op[Unit]
  final case class Modify[A, B](r: TRef[A], f: A => (A, B)) extends Op[B]
  final case class Retry() extends Op[Nothing]
  final case class OrElse[A](a: A ! Tx, b: A ! Tx) extends Op[A]

  implicit val effect: Effect[Tx] = Effect.of[Tx]

  def read[A](r: TRef[A]): A ! Tx = Free.inject[Tx, A](Read(r))
  def write[A](r: TRef[A], a: A): Unit ! Tx = Free.inject[Tx, Unit](Write(r, a))
  def modify[A, B](r: TRef[A])(f: A => (A, B)): B ! Tx = Free.inject[Tx, B](Modify(r, f))
  def update[A](r: TRef[A])(f: A => A): Unit ! Tx = Free.inject[Tx, Unit](Modify(r, (a: A) => (f(a), ())))
  def retry[A]: A ! Tx = Free.inject[Tx, A](Retry())
  def check(cond: Boolean): Unit ! Tx = if (cond) pure[Tx, Unit](()) else retry[Unit]
  def orElse[A](a: A ! Tx, b: A ! Tx): A ! Tx = Free.inject[Tx, A](OrElse(a, b))
}

/** a way to run a transaction atomically, answering in the row F */
trait Stm[F <: Row] {
  def atomically[A](tx: A ! Tx): A ! F
}

/**
 * The runtimes. `tl2` is Transactional Locking II (Dice, Shalev & Shavit,
 * DISC 2006) over okay2's `TRef`: a transaction reads into a LOG (each
 * cell with the version it had), writes into the log, and commits by
 * claiming every written cell with an `Owned` marker, re-validating the
 * reads, installing, and waking the cells' waiters. A conflict re-runs
 * it; a `retry` PARKS it as an `Async.await` on every cell it read, so a
 * thousand parked transactions hold no thread. `direct` is the same with
 * no claim (one thread); `sim` runs it under the deterministic scheduler,
 * a scheduling point before every step.
 */
object Stm {
  def apply[F <: Row](implicit s: Stm[F]): Stm[F] = s
  def atomically[A, F <: Row](tx: A ! Tx)(implicit s: Stm[F]): A ! F = s.atomically(tx)

  private object Abort extends RuntimeException(null, null, false, false)
  private object RetryNow extends RuntimeException(null, null, false, false)

  private val noRefs: Array[TRef[_]] = new Array[TRef[_]](0)
  private val noStamps: Array[Long] = new Array[Long](0)

  /** a transaction's reads (cell, version) and writes (a typed-key map) */
  private final class Log(parent: Option[Log] = None) {
    private var readRefs: Array[TRef[_]] = noRefs
    private var readStamps: Array[Long] = noStamps
    var nReads = 0

    def addRead(r: TRef[_], v: Long): Unit = {
      if (nReads == readRefs.length) {
        val n = if (nReads == 0) 4 else nReads * 2
        val refs = new Array[TRef[_]](n)
        System.arraycopy(readRefs, 0, refs, 0, nReads)
        readRefs = refs
        readStamps = java.util.Arrays.copyOf(readStamps, n)
      }
      readRefs(nReads) = r
      readStamps(nReads) = v
      nReads += 1
    }
    def readRef(i: Int): TRef[_] = readRefs(i)
    def readStamp(i: Int): Long = readStamps(i)
    def hasReads: Boolean = nReads > 0
    def addReads(child: Log): Unit = {
      var i = 0
      while (i < child.nReads) { addRead(child.readRefs(i), child.readStamps(i)); i += 1 }
    }

    private var writes = TMap.empty[TRef]
    def pending[X](r: TRef[X]): Option[X] = writes.get(r).orElse(parent.flatMap(_.pending(r)))
    def write[X](r: TRef[X], v: X): Unit = writes = writes.updated(r, v)
    def hasWrites: Boolean = writes.nonEmpty
    def eachWrite(f: TMap.Each[TRef]): Unit = writes.foreachUnordered(f)

    def installAll(): Unit = {
      eachWrite(new TMap.Each[TRef] { def apply[X](r: TRef[X], v: X): Unit = r.ref.set(r.install(v, r.version + 1)) })
      eachWrite(new TMap.Each[TRef] { def apply[X](r: TRef[X], v: X): Unit = r.wake() })
    }
    def absorb(child: Log): Unit =
      child.eachWrite(new TMap.Each[TRef] { def apply[X](r: TRef[X], v: X): Unit = write(r, v) })

    /** every read still at the version it had, and nobody committing */
    def valid: Boolean = {
      var i = 0
      var ok = true
      while (ok && i < nReads) {
        val c = readRefs(i).ref.get
        ok = !c.isInstanceOf[TRef.Owned[_]] && c.stamp == readStamps(i)
        i += 1
      }
      ok
    }
  }

  private def readOf[X](r: TRef[X], log: Log): X = log.pending(r) match {
    case Some(a) => a
    case None =>
      val c = r.ref.get
      if (c.isInstanceOf[TRef.Owned[_]] || !log.valid) throw Abort
      log.addRead(r, c.stamp)
      c.value
  }

  /** one operation against the log, at the answer type `Any` the
   * continuation takes (stage 8) */
  private def perform(op: Tx.Op[Any], log: Log): Any = op match {
    case r: Tx.Read[x] => readOf(r.r, log)
    case w: Tx.Write[x] => log.write(w.r, w.a)
    case m: Tx.Modify[x, y] =>
      val (a2, b) = m.f(readOf(m.r, log))
      log.write(m.r, a2)
      b
    case Tx.Retry() => throw RetryNow
    case o: Tx.OrElse[x] =>
      val branchA = new Log(parent = Some(log))
      val ra = try Some(runWithLog(o.a, branchA)) catch { case RetryNow => None }
      log.addReads(branchA)   // read either way: a real retry blocks on it too
      ra match {
        case Some(a) => log.absorb(branchA); a
        case None =>
          val branchB = new Log(parent = Some(log))
          try {
            val b = runWithLog(o.b, branchB)
            log.addReads(branchB)
            log.absorb(branchB)
            b
          } catch {
            case RetryNow =>
              log.addReads(branchB)
              throw RetryNow   // both branches retried: so does the whole thing
          }
      }
  }

  private def runWithLog[A](tx: A ! Tx, log: Log): A = {
    @tailrec def loop(p: A ! Tx): A = Free.resume(p) match {
      case Return(a) => a
      case Inject(e) => loop(Bind(Inject[Tx, A](e), (v: A) => Return[Tx, A](v)))
      case Bind(Inject(e), k) => loop(k(perform(Split.only[Tx, Any](e), log)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    loop(tx)
  }

  /** a claimed cell: what it held, and what the commit installs */
  private final class Held[X](r: TRef[X], before: TRef.Stamped[X], value: X) {
    def release(): Unit = r.ref.set(before)
    def install(): Unit = r.ref.set(r.install(value, before.stamp + 1))
    def wake(): Unit = r.wake()
  }

  private def own[X](r: TRef[X], value: X, token: AnyRef): Held[X] =
    r.ref.get match {
      case _: TRef.Owned[_] => null
      case s => if (r.ref.compareAndSet(s, new TRef.Owned(s, token))) new Held(r, s, value) else null
    }

  /** claim every written cell, re-validate the reads, install, wake —
   * or release everything and answer false */
  private def commit(log: Log): Boolean = {
    if (!log.hasWrites) return log.valid
    val token = new AnyRef
    val owned = mutable.ArrayBuffer.empty[Held[_]]
    var ok = true
    log.eachWrite(new TMap.Each[TRef] {
      def apply[X](r: TRef[X], v: X): Unit =
        if (ok) {
          val h = own(r, v, token)
          if (h != null) owned += h else ok = false
        }
    })
    if (ok) {
      var i = 0
      while (ok && i < log.nReads) {
        val c = log.readRef(i).ref.get
        ok = c.stamp == log.readStamp(i) && (c match {
          case o: TRef.Owned[_] => o.token eq token
          case _ => true
        })
        i += 1
      }
    }
    if (ok) { owned.foreach(_.install()); owned.foreach(_.wake()); true }
    else { owned.foreach(_.release()); false }
  }

  /** THE ONE CLAIM of the fast path: a program that is ONE operation
   * answers that operation's answer — the tree typed it when
   * `Tx.read`/`Tx.modify` built it, and `Inject` holds the operation as
   * `Any` since stage 8 */
  private def answer[A](x: Any): A = x.asInstanceOf[A]

  /** one read or one modify is the cell's own CAS: no log, no claim */
  private def fast[A](tx: A ! Tx): Option[A ! Async] = Free.resume(tx) match {
    case Return(a) => Some(pure[Async, A](a))
    case Inject(e) => Split.only[Tx, Any](e) match {
      case m: Tx.Modify[x, y] => Some(Async(answer[A](m.r.modify(m.f))))
      case r: Tx.Read[x] => Some(Async(answer[A](r.r.get)))
      case _ => None
    }
    case _ => None
  }

  /** park on every cell read; refuse a retry that read nothing */
  private def park[A](log: Log, again: () => Unit, k: Either[Throwable, A] => Unit): Unit =
    if (!log.hasReads) k(Left(new IllegalStateException("retry with nothing read: nothing could ever wake it")))
    else {
      val w = new TRef.Waiter(again)
      var i = 0
      while (i < log.nReads) { log.readRef(i).watch(w); i += 1 }
      if (!log.valid) w.fire()
    }

  /** Transactional Locking II over Async */
  val tl2: Stm[Async] = new Stm[Async] {
    def atomically[A](tx: A ! Tx): A ! Async =
      fast(tx).getOrElse(Async.await[A] { k => attempt(tx, k); () => () })

    private def attempt[A](tx: A ! Tx, k: Either[Throwable, A] => Unit): Unit = {
      var done = false
      while (!done) {
        val log = new Log
        try {
          val a = runWithLog(tx, log)
          if (commit(log)) { done = true; k(Right(a)) }
        } catch {
          case Abort => ()
          case RetryNow =>
            done = true
            park(log, () => attempt(tx, k), k)
          case e: Throwable => done = true; k(Left(e))
        }
      }
    }
  }

  /** one thread: no claim, the writes installed directly */
  val direct: Stm[Async] = new Stm[Async] {
    def atomically[A](tx: A ! Tx): A ! Async =
      fast(tx).getOrElse(Async.await[A] { k => attempt(tx, k); () => () })

    private def attempt[A](tx: A ! Tx, k: Either[Throwable, A] => Unit): Unit = {
      val log = new Log
      try {
        val a = runWithLog(tx, log)
        log.installAll()
        k(Right(a))
      } catch {
        case Abort => attempt(tx, k)   // cannot happen on one thread; stated
        case RetryNow => park(log, () => attempt(tx, k), k)
        case e: Throwable => k(Left(e))
      }
    }
  }

  /** under the deterministic scheduler: a scheduling point before every
   * step, a conflict re-run, a retry a virtual millisecond's sleep */
  val sim: Stm[Sim.Op] = new Stm[Sim.Op] {
    def atomically[A](tx: A ! Tx): A ! Sim.Op = {
      def attempt: A ! Sim.Op = {
        val log = new Log
        def step(e: Any): Either[Throwable, Any] =
          try Right(perform(Split.only[Tx, Any](e), log))
          catch { case t: Throwable => Left(t) }
        def finish(a: A): A ! Sim.Op = if (commit(log)) pure[Sim.Op, A](a) else attempt
        def after(outcome: Either[Throwable, Any])(next: Any => A ! Sim.Op): A ! Sim.Op = outcome match {
          case Right(x) => next(x)
          case Left(Abort) => attempt
          case Left(RetryNow) => Sim.sleep(1).flatMap(_ => attempt)
          case Left(t) => throw t
        }
        // a call from inside flatMap (or a by-name `++`) cannot be a jump; `again`
        // takes it, so the walk itself stays a checked loop (specs/stack-safety.md)
        def again(p: A ! Tx): A ! Sim.Op = loop(p)
        @tailrec def loop(p: A ! Tx): A ! Sim.Op = Free.resume(p) match {
          case Return(a) => finish(a)
          case Inject(e) => loop(Bind(Inject[Tx, A](e), (v: A) => Return[Tx, A](v)))
          case Bind(Inject(e), k) => Sim.yieldNow.flatMap(_ => after(step(e))(x => again(k(x))))
          case other => throw new IllegalStateException("resume left a non-head form: " + other)
        }
        loop(tx)
      }
      attempt
    }
  }

  /** the JVM's default: TL2 */
  implicit val default: Stm[Async] = tl2
}
