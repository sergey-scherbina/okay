package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The rows this project never measured because it expected to lose
 * them (adversarial-lanes, 2026-09-06).
 *
 * Every other table here is a shape this library chose. These three
 * are the shapes the cats-effect and ZIO runtimes are BUILT for and
 * their per-bind tax buys: many fibers under real contention,
 * cancellation, and fork/join as throughput rather than as the fixed
 * cost §4 measures at K = 100. Predictions are in the claim; if okay
 * wins all three, the lanes were chosen badly and the doc says so.
 *
 * Competitor spellings are their own idioms, checked in their
 * sources: `ZIO.foreachPar` (unbounded parallelism by default),
 * `parTraverse`, `Queue.bounded` with a `None` sentinel per consumer,
 * `fiber.interrupt` / `fiber.cancel`, both of which await the fiber.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class AdversarialBenchmark {

  // ── 1. fork/join as THROUGHPUT: 10 000 fibers, each a small step ──

  final val K = 10000

  /** the work per fiber, integer ops. 100 is scheduling overhead
   * with a token of work (~30 ns): the shape where NOT spreading the
   * work wins and kyo's one-worker answer is the right one. 10 000
   * (~3 us) is where spreading over the cores pays and a scheduler
   * that keeps the work at home loses. Both, so the row says which
   * it is measuring. */
  @Param(Array("100", "10000"))
  var work: Int = 100

  private def step(i: Int): Int =
    var s = 0; var j = 0
    val n = work
    while j < n do { s += (i ^ j); j += 1 }
    s

  @Benchmark
  def forkJoin10k_rawLoom(): Long =
    import java.util.concurrent.CompletableFuture
    val fs = (0 until K).map: i =>
      val f = CompletableFuture[Int]()
      Thread.startVirtualThread(() => f.complete(step(i)): Unit)
      f
    fs.foldLeft(0L)((acc, f) => acc + f.join())

  @Benchmark
  def forkJoin10k_okay(): Long =
    (0 until K).map(i => Async.spawn(async(step(i)))).foldLeft(0L)((acc, f) => acc + f.join())

  /** the same on okay's POOL scheduler: a fiber as a pool task rather
   * than a virtual thread. `Schedulers.forkJoin` exists for exactly
   * this shape -- short, CPU-bound, nothing parks -- and a parked
   * fiber there would hold a pool thread, which is why Loom is the
   * default and this is not */
  @Benchmark
  def forkJoin10k_okayPool(): Long =
    given Scheduler = Schedulers.forkJoin()
    (0 until K).map(i => Async.spawn(async(step(i)))).foldLeft(0L)((acc, f) => acc + f.join())

  /** the owned-worker schedulers are created for the trial and closed
   * after it: each owns `cores` threads, and three of them alive at
   * once in one JVM is a benchmark measuring its own thread count. */
  private var own: Schedulers.Running = null
  private var ownShort: Schedulers.Running = null
  private var ownLong: Schedulers.Running = null

  @Setup(Level.Trial)
  def startSchedulers(): Unit =
    own = Schedulers.own.build
    ownShort = Schedulers.own.forShortTasks.build
    ownLong = Schedulers.own.forLongTasks.build

  @TearDown(Level.Trial)
  def stopSchedulers(): Unit =
    own.close(); ownShort.close(); ownLong.close()

  /** a scheduler that owns its threads (`Schedulers.own`, kyo's
   * shape), one instance for the lanes: the workers outlive the
   * call. `okayOwn` forks from the JMH thread (an external submitter,
   * as every okay lane here does); `okayOwnInside` forks from INSIDE
   * a fiber on the scheduler and joins by `joinAsync`, which is what
   * kyo's `runAndBlock(parallelUnbounded(..))` does — the forking
   * program runs on a worker, its children land on that worker's
   * own queue. Both are okay's idiom; the second is the one kyo's
   * lane is shaped like. */

  @Benchmark
  def forkJoin10k_okayOwn(): Long =
    given Scheduler = own
    (0 until K).map(i => Async.spawn(async(step(i)))).foldLeft(0L)((acc, f) => acc + f.join())

  /** the same scheduler with its one decision pinned by the builder:
   * `forShortTasks` never spreads (kyo's policy), `forLongTasks`
   * spreads at once (the pool's). The default lane above decides for
   * itself; these two say what each end of that decision is worth on
   * each shape of work. */

  @Benchmark
  def forkJoin10k_okayOwnShortInside(): Long =
    given Scheduler = ownShort
    Async.spawn {
      val fs = (0 until K).map(i => Async.spawn(async(step(i))))
      fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
    }.join()

  @Benchmark
  def forkJoin10k_okayOwnLongInside(): Long =
    given Scheduler = ownLong
    Async.spawn {
      val fs = (0 until K).map(i => Async.spawn(async(step(i))))
      fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
    }.join()

  @Benchmark
  def forkJoin10k_okayOwnInside(): Long =
    given Scheduler = own
    Async.spawn {
      val fs = (0 until K).map(i => Async.spawn(async(step(i))))
      fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
    }.join()

  /** the same shape on the drive scheduler, for the pair */
  @Benchmark
  def forkJoin10k_okayDriveInside(): Long =
    given Scheduler = Schedulers.drive()
    Async.spawn {
      val fs = (0 until K).map(i => Async.spawn(async(step(i))))
      fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
    }.join()

  /** okay's drive scheduler: the JS shape on the JVM, fiber = task =
   * promise (`DriveTask`); 25 % over the raw pool, §4b */
  @Benchmark
  def forkJoin10k_okayDrive(): Long =
    given Scheduler = Schedulers.drive()
    (0 until K).map(i => Async.spawn(async(step(i)))).foldLeft(0L)((acc, f) => acc + f.join())

  @Benchmark
  def forkJoin10k_zio(): Long =
    import _root_.zio.*
    val z = ZIO.foreachPar(0 until K)(i => ZIO.succeed(step(i))).map(_.foldLeft(0L)(_ + _))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def forkJoin10k_cats(): Long =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    import cats.syntax.all.*
    (0 until K).toList.parTraverse(i => IO(step(i))).map(_.foldLeft(0L)(_ + _)).unsafeRunSync()

  /** kyo as it must be spelled for this lane: the step DEFERRED into
   * the fiber with `IO`. The form this table carried until 2026-09-07
   * computed `step(i)` on the caller, sequentially, BEFORE
   * `parallelUnbounded` saw it (`(step(i): Int < Any)` is a value),
   * so kyo forked 10 000 finished values; it is kept below as the
   * diagnostic that says how much of "kyo's scheduler" was that. */
  @Benchmark
  def forkJoin10k_kyo(): Long =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    val seq: Seq[Int < (Abort[Nothing] & Async)] = (0 until K).map(i => IO(step(i)))
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(
      Async.parallelUnbounded(seq).flatMap((c: Seq[Int]) => (c.foldLeft(0L)(_ + _): Long < Any)))
      .getOrThrow

  /** kyo forked from OUTSIDE a worker: each fiber is started from the
   * benchmark thread, so `Worker.current` is null and kyo's scheduler
   * takes its OTHER path — the least loaded of a random sample of
   * workers. Same kyo, same fibers, the one difference being where
   * `Async.run` is called from. This is the lane that says whether
   * kyo's one-core answer at heavy work is a limit or a policy. */
  @Benchmark
  def forkJoin10k_kyoOutside(): Long =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    val fs = (0 until K).map(i => IO.Unsafe.evalOrThrow(Async.run(IO(step(i)))))
    fs.foldLeft(0L)((acc, f) => acc + IO.Unsafe.evalOrThrow(f.block(Duration.Infinity)).getOrThrow)

  @Benchmark
  def forkJoin10k_kyoEager(): Long =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    val seq: Seq[Int < (Abort[Nothing] & Async)] = (0 until K).map(i => (step(i): Int < Any))
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(
      Async.parallelUnbounded(seq).flatMap((c: Seq[Int]) => (c.foldLeft(0L)(_ + _): Long < Any)))
      .getOrThrow

  // ── 2. many-to-many CONTENTION over one bounded queue ─────────────

  /** producers x consumers; ManyProducers is P x 1 and never had a
   * second consumer, which is where a channel transaction and a
   * queue's take differ most */
  @Param(Array("4x4", "16x16"))
  var shape: String = "4x4"

  final val Total = 16000
  final val Cap = 1024

  private def P = shape.split("x")(0).toInt
  private def C = shape.split("x")(1).toInt

  @Benchmark
  def manyToMany_okay(): Long =
    val c = Channel[Long](Cap)
    val p = P; val n = C; val per = Total / p
    val ps = (0 until p).map(w => Thread.ofVirtual().start { () =>
      var i = 0L
      while i < per do { val _ = c.sendBlocking(w.toLong * per + i); i += 1 }
    })
    val sums = new Array[Long](n)
    val cs = (0 until n).map(j => Thread.ofVirtual().start { () =>
      var s = 0L; var go = true
      while go do
        c.receiveBlocking() match
          case Some(v) => s += v
          case None => go = false
      sums(j) = s
    })
    ps.foreach(_.join()); c.close(); cs.foreach(_.join())
    sums.sum

  /** the partitioned buffer, P parts: producers spread over parts,
   * which is what relaxed-queues built for the P x 1 shape */
  @Benchmark
  def manyToMany_okayAdaptive(): Long =
    // CAPACITY-MATCHED (lane-fairness, 2026-09-08): this was
    // `each(Cap)` — 16 384 slots against `manyToMany_okay`'s 1024 and
    // against zio's and cats' `Queue.bounded(Cap)`. §4b quoted it as
    // "the answer this library ships, 3.6x ahead"; that comparison
    // gave us 16x the buffer.
    val c = Queues.strong[Long].adaptive.parts(16).each(math.max(Cap / 16, 2)).build
    val p = P; val n = C; val per = Total / p
    val ps = (0 until p).map(w => Thread.ofVirtual().start { () =>
      var i = 0L
      while i < per do { val _ = c.sendBlocking(w.toLong * per + i); i += 1 }
    })
    val sums = new Array[Long](n)
    val cs = (0 until n).map(j => Thread.ofVirtual().start { () =>
      var s = 0L; var go = true
      while go do
        c.receiveBlocking() match
          case Some(v) => s += v
          case None => go = false
      sums(j) = s
    })
    ps.foreach(_.join()); c.close(); cs.foreach(_.join())
    sums.sum

  @Benchmark
  def manyToMany_zio(): Long =
    import _root_.zio.*
    val p = P; val n = C; val per = Total / p
    def consume(q: Queue[Option[Long]]): UIO[Long] =
      def go(acc: Long): UIO[Long] = q.take.flatMap {
        case Some(x) => go(acc + x)
        case None => ZIO.succeed(acc)
      }
      go(0L)
    val z = for
      q <- Queue.bounded[Option[Long]](Cap)
      prod <- ZIO.foreachPar(0 until p)(w =>
                ZIO.foreachDiscard(0L until per.toLong)(i => q.offer(Some(w.toLong * per + i)))).fork
      cons <- ZIO.foreachPar(0 until n)(_ => consume(q)).fork
      _ <- prod.join
      _ <- ZIO.foreachDiscard(0 until n)(_ => q.offer(None))
      sums <- cons.join
    yield sums.sum
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def manyToMany_cats(): Long =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    import cats.syntax.all.*
    val p = P; val n = C; val per = Total / p
    def consume(q: cats.effect.std.Queue[IO, Option[Long]]): IO[Long] =
      def go(acc: Long): IO[Long] = q.take.flatMap {
        case Some(x) => go(acc + x)
        case None => IO.pure(acc)
      }
      go(0L)
    def produce(q: cats.effect.std.Queue[IO, Option[Long]], w: Int): IO[Unit] =
      def go(i: Long): IO[Unit] = if i < per then q.offer(Some(w.toLong * per + i)) >> go(i + 1) else IO.unit
      go(0L)
    (for
      q <- cats.effect.std.Queue.bounded[IO, Option[Long]](Cap)
      prod <- (0 until p).toList.parTraverse_(w => produce(q, w)).start
      cons <- (0 until n).toList.parTraverse(_ => consume(q)).start
      _ <- prod.join
      _ <- (0 until n).toList.traverse_(_ => q.offer(None))
      sums <- cons.joinWithNever
    yield sums.sum).unsafeRunSync()

  // ── 3. CANCELLATION: fibers parked for ever, all cancelled, joined ─

  final val K2 = 1000

  /** okay's cancel is a thread interrupt reaching a parked CanBlock;
   * theirs is a fiber flag checked between operations */
  @Benchmark
  def cancel1k_okay(): Int =
    val fs = (0 until K2).map(_ => Async.spawn(Async.await[Unit](_ => () => ())))
    fs.foreach(_.cancel())
    fs.count(_.joinEither().isLeft)

  /** the same 1 000 parked fibers on the owned-worker scheduler,
   * where a cancel is a CAS on the fiber's own cell rather than an
   * interrupt of a thread — §4b blamed the interrupt, and this is
   * the lane that says whether it was right */
  @Benchmark
  def cancel1k_okayOwn(): Int =
    given Scheduler = own
    val fs = (0 until K2).map(_ => Async.spawn(Async.await[Unit](_ => () => ())))
    fs.foreach(_.cancel())
    fs.count(_.joinEither().isLeft)

  @Benchmark
  def cancel1k_okayDrive(): Int =
    given Scheduler = Schedulers.drive()
    val fs = (0 until K2).map(_ => Async.spawn(Async.await[Unit](_ => () => ())))
    fs.foreach(_.cancel())
    fs.count(_.joinEither().isLeft)

  @Benchmark
  def cancel1k_zio(): Int =
    import _root_.zio.*
    val z = for
      fs <- ZIO.foreach(0 until K2)(_ => ZIO.never.fork)
      exits <- ZIO.foreach(fs)(_.interrupt)
    yield exits.count(_.isInterrupted)
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def cancel1k_cats(): Int =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    import cats.syntax.all.*
    (for
      fs <- (0 until K2).toList.traverse(_ => IO.never[Unit].start)
      _ <- fs.traverse_(_.cancel)
      outs <- fs.traverse(_.join)
    yield outs.count(_.isCanceled)).unsafeRunSync()
}
