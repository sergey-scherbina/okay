package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * own-managed-blocking (specs/schedulers.md, "Managed blocking"): the
 * blocking door on `own`/`adaptive`, and what it must NOT cost the path
 * that never blocks — `own` and `adaptive` side by side in one fork, so
 * the stuck-check's per-task counter shows as the gap between them.
 *
 *   forkJoin10kInside  10 000 ~30 ns fibers forked inside a fiber,
 *                      joined by joinAsync (AdversarialBenchmark's
 *                      forkJoin10k_okayOwnInside, work = 100)
 *   spawnJoinSeq       1 000 x (fork one tiny child, join it) from inside
 *                      a fiber (OwnMonitorBenchmark's)
 *   blockingBurst      64 fibers forked inside a fiber, each blocking 4 x
 *                      1 ms through `CanBlock.block` on a timer — the
 *                      library's own door, what `join()` and a blocking
 *                      receive go through
 *   outsideForkWhileBlocked  one fiber blocked in the door, the other
 *                      workers parked; a tiny fiber forked from OUTSIDE
 *                      and joined. Before managed blocking the blocked
 *                      worker counted as awake, the submission woke
 *                      nobody, and the fiber waited for the stuck-check
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class OwnBlockingBenchmark {

  @Param(Array("own", "adaptive"))
  var sched: String = "own"

  private var s: Schedulers.Running = null

  @Setup(Level.Trial)
  def start(): Unit =
    s = (sched match
      case "own" => Schedulers.own
      case "adaptive" => Schedulers.adaptive
      case other => throw new IllegalArgumentException(other)).build

  @TearDown(Level.Trial)
  def stop(): Unit = s.close()

  private def step(i: Int): Int =
    var r = 0; var j = 0
    while j < 100 do { r += (i ^ j); j += 1 }
    r

  @Benchmark
  def forkJoin10kInside(): Long =
    given Scheduler = s
    Async.spawn {
      val fs = (0 until 10000).map(i => Async.spawn(async(step(i))))
      fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
    }.join()

  @Benchmark
  def spawnJoinSeq(): Long =
    given Scheduler = s
    def loop(i: Int, sum: Long): Long ! Async =
      if i == 1000 then pure[Async, Long](sum)
      else async(Async.spawn(async(i))).flatMap(_.joinAsync).flatMap(v => loop(i + 1, sum + v))
    Async.spawn(loop(0, 0L)).join()

  /** one 1 ms wait through the door: the timer answers, the fiber parks */
  private def blockOneMilli(): Unit =
    summon[CanBlock].block[Unit](k => summon[Timer].after(1L)(() => k(())))

  @Benchmark
  def blockingBurst(): Long =
    given Scheduler = s
    Async.spawn {
      async((0 until 64).map(_ => Async.spawn(async { var c = 0; while c < 4 do { blockOneMilli(); c += 1 }; 1L }))).flatMap { fs =>
        fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
      }
    }.join()

  @Benchmark
  def outsideForkWhileBlocked(): Long =
    given Scheduler = s
    val k = java.util.concurrent.atomic.AtomicReference[(Unit => Unit) | Null](null)
    val blocked = Async.spawn(async(summon[CanBlock].block[Unit] { cb => k.set(cb); () => () }))
    while k.get == null do Thread.onSpinWait()
    Thread.sleep(1) // the other workers spin out and park
    val r = Async.spawn(async(1L)).join()
    k.get.nn(())
    blocked.join()
    r
}
