package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * SPIKE: Ox (com.softwaremill.ox, direct-style structured concurrency
 * on Loom) in the fork/join lane, at AsyncBenchmark's exact shape --
 * K trivial tasks forked, joined, summed -- so the numbers pair.
 *
 * Ox enters its scope ONCE and forks K inside, which is the shape
 * every competitor lane in AsyncBenchmark uses and which
 * `okaySpawnInside` exists to match.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class OxBenchmark {

  final val K = 100

  /** the floor, repeated here so a round of this file is self-contained */
  @Benchmark
  def rawLoom(): Int =
    import java.util.concurrent.CompletableFuture
    val fs = (1 to K).map: _ =>
      val f = CompletableFuture[Int]()
      Thread.startVirtualThread(() => f.complete(1): Unit)
      f
    fs.map(_.join()).sum

  @Benchmark
  def okaySpawnInside(): Int =
    Async.spawn {
      val fs = (1 to K).map(_ => Async.spawn(async(1)))
      fs.foldLeft(pure[Async, Int](0))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
    }.join()

  /** Ox WITH supervision: a failing fork cancels its siblings and the
   *  scope rethrows. That is a guarantee okay's lane does not provide,
   *  so this row is not the pair -- it is the fuller feature. */
  @Benchmark
  def oxForkJoin(): Int =
    import ox.*
    supervised {
      val fs = (1 to K).map(_ => fork(1))
      fs.map(_.join()).sum
    }

  /** THE OTHER PAIR, now that okay has the feature: supervision on
   *  BOTH sides. `Async.supervised` owns its children, cancels the
   *  siblings of a failure and does not finish while one runs --
   *  the same contract `ox.supervised` sells. */
  @Benchmark
  def okaySupervised(): Int =
    // NB: NO scheduler declared here. The first cut of this lane said
    // `given Scheduler = Schedulers.forkJoin()`, which swapped the
    // scheduler out from under the comparison -- every other lane
    // takes the default (Schedulers.loom, a virtual thread per fork),
    // and Platform.scala prices that choice at up to 3.6x on its own.
    // It read as "supervision is FASTER than no supervision", which is
    // the tell.
    !.run(Async.run[Int, Pure](
      Async.supervised: n ?=>
        val fs = (1 to K).map(_ => n.fork(async(1)))
        fs.foldLeft(pure[Async, Int](0))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))))

  /** THE PAIR: no supervision, which is the budget okaySpawnInside
   *  actually spends (rule 4 -- both sides get the same budget). */
  @Benchmark
  def oxUnsupervised(): Int =
    import ox.*
    unsupervised {
      val fs = (1 to K).map(_ => forkUnsupervised(1))
      fs.map(_.join()).sum
    }
}
