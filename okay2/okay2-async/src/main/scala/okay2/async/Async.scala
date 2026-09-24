package okay2.async


import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.annotation.implicitNotFound
import scala.concurrent.{Future, Promise}
import okay2._
import okay2.Free.{Return, Inject, Bind}

/**
 * Asynchrony as an effect. `Run` is a suspended computation — blocking
 * is a PLATFORM ability, Loom-style on the JVM where parking a virtual
 * thread is free. `Await` is the callback-form suspension every
 * platform has: register a continuation and answer with the canceller
 * that unregisters it; the callback's Left is the error channel and
 * fails the whole program at this operation. The class IS the whole
 * identity here: `Async` has no parameter but its erased answer, so
 * splitting a row on it is a TOTAL test.
 */
sealed trait Async extends Row { type Op[+A] = Async.Op[A] }

/**
 * The answer to a send: was the element taken? A dedicated type
 * rather than `Boolean => Unit`, because `Function1` is not
 * specialised on Boolean and every acceptance would box one.
 */
trait Accepted { def apply(accepted: Boolean): Unit }

/**
 * Evidence that this platform can park a thread of control until a
 * callback fires. The JVM has it (`okay2.platform`); a platform without
 * it closes every blocking door at compile time.
 */
@implicitNotFound("no CanBlock capability in scope.\nBlocking parks a thread, so it must be GRANTED, not assumed: `import okay2.platform._` installs the JVM's,\nor take one as a parameter (`implicit cb: CanBlock`).")
trait CanBlock {
  /** park the current thread until the registered callback fires; if
   * the park itself fails (interruption), the registration is
   * cancelled on the way out */
  def block[A](register: (A => Unit) => (() => Unit)): A

  /** the same wait, for the one answer that is a primitive */
  def blockAccepted(register: Accepted => (() => Unit)): Boolean

  /** a handoff this platform can park on */
  def handoff[A](): Handoff[A]

  /** park until the handoff is filled; returns at once if it already is */
  def await(h: Handoff[_]): Unit
}

/** the platform timer: run a callback after the duration; the answer cancels */
trait Timer { def after(millis: Long)(k: () => Unit): () => Unit }

/**
 * WHAT A PLATFORM PROVIDES, as one value: the three capabilities every
 * blocking or forking door asks for. The companions of `CanBlock`,
 * `Timer` and `Scheduler` derive their implicit from it, so a platform
 * installs all three by putting ONE implicit in scope
 * (`import okay2.platform._`), and — the reason for the indirection —
 * they arrive through IMPLICIT SCOPE, which a lexical `implicit val
 * S: Scheduler = Schedulers.forkJoin()` beats outright. Three plain
 * implicit vals in the platform's package object were tried first:
 * Scala 2 reads a local implicit of another NAME as a second
 * candidate, not a shadow, and every override was "ambiguous implicit
 * values".
 */
trait PlatformDefaults {
  def canBlock: CanBlock
  def timer: Timer
  def scheduler: Scheduler
}

object CanBlock {
  implicit def fromPlatform(implicit p: PlatformDefaults): CanBlock = p.canBlock
}

object Timer {
  implicit def fromPlatform(implicit p: PlatformDefaults): Timer = p.timer
}

object Scheduler {
  implicit def fromPlatform(implicit p: PlatformDefaults): Scheduler = p.scheduler
}

/**
 * The one-shot handoff a blocking receive waits on — and the callback
 * that fills it, in ONE object: the callback IS the slot, so a receive
 * that waits allocates this and nothing else. Platform-specific only
 * in how a waiter is parked and signalled.
 */
abstract class Handoff[A] extends (Either[Throwable, Option[A]] => Unit) {
  private var value: A = null.asInstanceOf[A]
  private var ended = false
  private var failure: Throwable = null
  /** the release fence: a reader that sees it true sees everything written before it */
  @volatile var filled: Boolean = false

  /** the fast path: an element that was ready — only the write and the fence */
  final def got(a: A): Unit = { value = a; filled = true }

  /** the callback path: fills, fences, and signals a parked waiter if any */
  final def apply(e: Either[Throwable, Option[A]]): Unit = {
    e match {
      case Right(Some(a)) => value = a
      case Right(None) => ended = true
      case Left(t) => failure = t
    }
    filled = true
    signal()
  }

  /** the element, `None` at the end, or the producer's failure thrown */
  final def answer: Option[A] = {
    val f = failure
    if (f != null) throw f
    else if (ended) None
    else Some(value)
  }

  /** wake whoever parked on this handoff, if anyone */
  protected def signal(): Unit
}

/**
 * A fiber: a computation already running on its own thread of
 * control. The cross-platform surface is completion and cancellation;
 * the blocking join exists exactly where parking does.
 */
trait Fiber[A] {
  /** call k when finished — the universal observation */
  def onComplete(k: Either[Throwable, A] => Unit): Unit

  /** request cancellation (best effort — the computation must be
   * interruptible, or between operations, to notice) */
  def cancel(): Unit

  /** join as an operation: awaits the fiber, fails if it failed */
  def joinAsync: A ! Async = Async.await[A](k => { onComplete(k); () => () })

  /** park until finished, then the answer */
  def join()(implicit cb: CanBlock): A = joinEither().fold(e => throw e, identity)

  /** park until finished; a failure as a value */
  def joinEither()(implicit cb: CanBlock): Either[Throwable, A] =
    cb.block[Either[Throwable, A]](k => { onComplete(k); () => () })
}

/** the scheduler: how a program gets its own thread of control. It
 * takes the PROGRAM, not a computed answer */
trait Scheduler { def fork[A](prog: () => A ! Async): Fiber[A] }

object Async {
  sealed trait Op[+A]
  /** a suspended (possibly blocking) computation */
  final case class Run[A](run: () => A) extends Op[A]
  /** the universal, callback-form suspension: register a continuation
   * and answer with the canceller that unregisters it */
  final case class Await[A](register: (Either[Throwable, A] => Unit) => (() => Unit)) extends Op[A]

  implicit val effect: Effect[Async] = Effect.of[Async]

  /** suspend a (possibly blocking) computation as an operation — the
   * Scala 3 core's `async(a)`, spelled as the signature's constructor */
  def apply[A](a: => A): A ! Async = Free.inject[Async, A](Run(() => a))

  /** execute each operation on the current (ideally virtual) thread;
   * an Await parks it until the callback fires */
  implicit def handler(implicit cb: CanBlock): Handler[Async] = new Handler.Of[Async] {
    def handle[A](e: Op[A]): A = e match {
      case Run(f) => f()
      case Await(reg) => cb.block[Either[Throwable, A]](reg).fold(e => throw e, identity)
    }
  }

  /** the full callback form: an error channel in, a canceller out */
  def await[A](register: (Either[Throwable, A] => Unit) => (() => Unit)): A ! Async =
    Free.inject[Async, A](Await(register))

  /** handle by executing each operation in place, forwarding the rest
   * of the row; an Await parks (hence the evidence) */
  def run[A, R <: Row](prog: Free[Async with R, A])(implicit cb: CanBlock): A ! R =
    runAt[A, R](prog)

  /** `run` at the handler's own shape */
  def runAt[A, F <: Row](prog: Free[Async with F, A])(implicit cb: CanBlock): A ! F =
    Effects.relay[A, A, Async, F](prog)(a => pure(a))(new Relay[Async] {
      def apply[X, Y](e: Op[X]): X /> Y = e match {
        case Run(f) => Cont.Pure(f())
        case Await(reg) => Cont.Pure(cb.block[Either[Throwable, X]](reg).fold(e => throw e, identity))
      }
    })

  /** the universal terminal: drive the tree through callbacks — Run
   * operations execute in place, an Await parks nothing, the
   * registered callback re-enters the drive */
  def runAsync[A](prog: Free[Async, A]): Future[A] = {
    val p = Promise[A]()
    new PromiseDrive(p).apply(prog)
    p.future
  }

  /** the callback may fire during registration, on this thread or
   * another: whoever loses the atomic exchange continues the drive */
  private final class Got[X](val x: Either[Throwable, X])
  private object Moved

  /**
   * One driving of one tree: a while-loop while answers arrive
   * synchronously, a re-entry from the callback when they do not.
   * cancel() stops the drive at its next operation AND unregisters a
   * parked Await.
   */
  private[okay2] trait Drive[A] {
    protected def succeed(a: A): Unit
    protected def fail(e: Throwable): Unit
    @volatile private var stopped = false
    @volatile private var unregister: () => Unit = () => ()

    def cancel(): Unit = {
      stopped = true
      unregister()
    }

    /** a direct loop over the tree's cases, one turn per OPERATION */
    def apply(prog: Free[Async, A]): Unit = {
      var cur: A ! Async = prog
      var looping = !stopped
      try {
        while (looping) {
          looping = false
          Free.resume(cur) match {
            case Return(a) => succeed(a)
            case Bind(Inject(e), f) =>
              val next = op(Split.only[Async, Any](e), f)
              if (next != null) { cur = next; looping = !stopped }
            case Inject(e) =>
              val next = op(Split.only[Async, A](e), (x: A) => Return[Async, A](x))
              if (next != null) { cur = next; looping = !stopped }
            case other => throw new IllegalStateException("resume left a non-head form: " + other)
          }
        }
      } catch { case e: Throwable => fail(e) }
    }

    /** one operation: the continuation to drive next when the answer
     * came synchronously, null when the drive parked on a callback
     * (which re-enters `apply`) or the program finished here */
    private def op[X](e: Op[X], k: X => A ! Async): A ! Async = e match {
      case Run(f) => k(f())
      case Await(reg) =>
        val cell = new AtomicReference[AnyRef](null)
        val cancelReg = reg { r =>
          if (!cell.compareAndSet(null, new Got(r))) {
            if (!stopped) r match {
              case Right(x) => apply(k(x))
              case Left(e) => fail(e)
            }
          }
        }
        cell.getAndSet(Moved) match {
          case g: Got[X @unchecked] => g.x match {
            case Right(x) => k(x)
            case Left(err) => fail(err); null
          }
          case _ =>
            unregister = cancelReg
            if (stopped) cancelReg()
            null
        }
    }
  }

  /** the Drive that answers into a Promise */
  private[okay2] final class PromiseDrive[A](p: Promise[A]) extends Drive[A] {
    protected def succeed(a: A): Unit = { val _ = p.trySuccess(a) }
    protected def fail(e: Throwable): Unit = { val _ = p.tryFailure(e) }
  }

  /** run the program on its own fiber */
  def spawn[A](prog: => Free[Async, A])(implicit S: Scheduler): Fiber[A] = S.fork(() => prog)

  /**
   * AN OPEN SUPERVISED SCOPE: fork as many children as you like, and
   * the scope owns them — it does not finish while a child runs; the
   * FIRST failure, a child's or the body's, cancels every other child
   * and leaves the scope with that error. Callbacks, not parking.
   */
  final class Nursery private[async] (S: Scheduler) {
    private val live = new AtomicInteger(0)
    private val kids = new AtomicReference(List.empty[Fiber[_]])
    private val failed = new AtomicBoolean(false)
    private val idle = new AtomicReference[() => Unit](null)
    private[async] var onFirstFailure: Throwable => Unit = _ => ()

    /** fork a child into this scope */
    def fork[B](p: => Free[Async, B]): Fiber[B] = {
      val _ = live.incrementAndGet()
      val f = Async.spawn(p)(S)
      val _ = kids.updateAndGet(f :: _)
      f.onComplete {
        case Left(e) =>
          if (!failed.getAndSet(true)) onFirstFailure(e)
          settle()
        case Right(_) => settle()
      }
      // a child forked after the scope failed is cancelled as it joins
      if (failed.get()) f.cancel()
      f
    }

    private def settle(): Unit = if (live.decrementAndGet() == 0) fireIdle()

    private def fireIdle(): Unit = {
      val cb = idle.getAndSet(null)
      if (cb != null) cb()
    }

    private[async] def whenIdle(cb: () => Unit): Unit =
      if (live.get() == 0) cb()
      else {
        idle.set(cb)
        if (live.get() == 0) fireIdle()
      }

    private[async] def cancelAll(): Unit = {
      failed.set(true)
      kids.get().foreach(_.cancel())
    }
  }

  /** the body takes the nursery as an ordinary parameter, where the
   * Scala 3 core gives it as a context function */
  def supervised[A](body: Nursery => A ! Async)(implicit S: Scheduler): A ! Async =
    await[A] { k =>
      val n = new Nursery(S)
      val settled = new AtomicBoolean(false)
      def done(r: Either[Throwable, A]): Unit = if (!settled.getAndSet(true)) k(r)

      n.onFirstFailure = e => { n.cancelAll(); done(Left(e)) }

      val main = spawn(body(n))
      main.onComplete {
        case Left(e) => n.cancelAll(); done(Left(e))
        case Right(a) => n.whenIdle(() => done(Right(a)))
      }
      () => { n.cancelAll(); main.cancel() }
    }

  /** both, each on its own fiber, by completion callbacks; EITHER
   * side's failure fails the pair at once and cancels the sibling —
   * the failure watch is registered on both sides up front */
  def par[A, B](a: => Free[Async, A], b: => Free[Async, B])(implicit S: Scheduler): (A, B) ! Async =
    await[(A, B)] { k =>
      val fa = spawn(a)
      val fb = spawn(b)
      val done = new AtomicBoolean(false)
      def fail(other: Fiber[_])(e: Throwable): Unit =
        if (!done.getAndSet(true)) { other.cancel(); k(Left(e)) }
      fb.onComplete {
        case Left(e) => fail(fa)(e)
        case Right(_) => ()
      }
      fa.onComplete {
        case Right(x) => fb.onComplete {
          case Right(y) => if (!done.getAndSet(true)) k(Right((x, y)))
          case Left(e) => fail(fa)(e)
        }
        case Left(e) => fail(fb)(e)
      }
      () => { fa.cancel(); fb.cancel() }
    }

  /** the program's failure as DATA: it runs on its own fiber, and
   * whatever it threw arrives as a Left */
  def attempt[A](prog: => Free[Async, A])(implicit S: Scheduler): Either[Throwable, A] ! Async =
    await[Either[Throwable, A]] { k =>
      val f = spawn(prog)
      f.onComplete(r => k(Right(r)))
      () => f.cancel()
    }

  /** park for the duration — an Await on the platform timer */
  def sleep(millis: Long)(implicit T: Timer): Unit ! Async =
    await[Unit](k => T.after(millis)(() => k(Right(()))))

  /** the answer within the duration, or None; the loser is cancelled.
   * The FIRST outcome of the program, of either kind, settles the
   * timeout, and only the timer answers None */
  def timeout[A](millis: Long)(prog: => Free[Async, A])(implicit S: Scheduler, T: Timer): Option[A] ! Async =
    await[Option[A]] { k =>
      val f = spawn(prog)
      val done = new AtomicBoolean(false)
      val timer = T.after(millis) { () =>
        if (!done.getAndSet(true)) { f.cancel(); k(Right(None)) }
      }
      f.onComplete { r =>
        if (!done.getAndSet(true)) { timer(); k(r.map(Some(_))) }
      }
      () => { if (!done.getAndSet(true)) { timer(); f.cancel() } }
    }

  /** the first of the two to SUCCEED; both losers are cancelled. If
   * both fail, the race fails with the later error */
  def race[A](a: => Free[Async, A], b: => Free[Async, A])(implicit S: Scheduler): A ! Async =
    await[A] { k =>
      val fa = spawn(a)
      val fb = spawn(b)
      val won = new AtomicBoolean(false)
      val alive = new AtomicInteger(2)
      def finish(r: Either[Throwable, A]): Unit = r match {
        case Right(v) => if (!won.getAndSet(true)) { fa.cancel(); fb.cancel(); k(Right(v)) }
        case Left(e) => if (alive.decrementAndGet() == 0 && !won.getAndSet(true)) k(Left(e))
      }
      fa.onComplete(finish)
      fb.onComplete(finish)
      () => { fa.cancel(); fb.cancel() }
    }
}

/**
 * Retry policies ARE streams of delays (milliseconds): the stream
 * algebra is the policy algebra — take limits the attempts, map
 * scales, ++ chains phases.
 */
object Retry {
  /** the same delay every time */
  def constant(ms: Long): LazyList[Long] = LazyList.continually(ms)

  /** n immediate retries */
  def immediate(n: Int): LazyList[Long] = constant(0).take(n)

  /** exponential backoff, capped */
  def exponential(base: Long, factor: Double = 2.0, cap: Long = Long.MaxValue): LazyList[Long] =
    LazyList.iterate(base.toDouble)(_ * factor).map(d => math.min(d.toLong, cap))

  /** multiply each delay by a deterministic factor in [0.5, 1.5) */
  def jittered(policy: LazyList[Long], seed: Long = 42): LazyList[Long] = {
    val rs = LazyList.iterate(seed)(x => x * 6364136223846793005L + 1442695040888963407L).tail
    policy.lazyZip(rs).map((d, r) => (d * (0.5 + math.floorMod(r, 1000) / 1000.0)).toLong)
  }

  /** `retry` as an Async PROGRAM: the attempt's failure arrives as
   * data, the delay is an `Async.sleep` — nothing parks a thread. The
   * program reruns FROM ITS BEGINNING on any exception; a policy
   * exhausted fails with the LAST error */
  def async[A](policy: LazyList[Long])(prog: => Free[Async, A])(implicit S: Scheduler, T: Timer): A ! Async = {
    def go(delays: LazyList[Long]): A ! Async =
      Async.attempt(prog).flatMap {
        case Right(a) => pure(a)
        case Left(e) => delays match {
          case d #:: rest => if (d > 0) Async.sleep(d).flatMap(_ => go(rest)) else go(rest)
          case _ => Async[A](throw e)
        }
      }
    go(policy)
  }
}

/**
 * THE PARALLEL SPINE: an applicative program is a pure term over
 * effectful leaves, so the leaves may run at once. `map2` joins two
 * leaves by `Async.par`, which forks both, fails the pair on either
 * failure and cancels the healthy sibling; `traverse`/`sequence` are
 * the same over a sequence, results in the argument's order. (The
 * Scala 3 core's `Applicative[Par]` instance waits for the
 * `Applicative` typeclass, a later stage.)
 */
object Par {
  /** two leaves at once, joined by a plain function */
  def map2[A, B, C](a: Free[Async, A], b: Free[Async, B])(f: (A, B) => C)(implicit S: Scheduler): C ! Async =
    Async.par(a, b).map { case (x, y) => f(x, y) }

  /** every element at once, results in the argument's order */
  def traverse[A, B](xs: Seq[A])(f: A => B ! Async)(implicit S: Scheduler): Seq[B] ! Async =
    xs.foldRight(pure[Async, List[B]](Nil))((a, acc) => map2(f(a), acc)(_ :: _)).map(_.toSeq)

  /** every program at once, results in the argument's order */
  def sequence[A](xs: Seq[A ! Async])(implicit S: Scheduler): Seq[A] ! Async = traverse(xs)(identity)
}
