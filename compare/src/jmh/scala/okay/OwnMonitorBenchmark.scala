package okay


import okay.freer.*


import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * own-scheduler-monitor: what the monitor costs, and what it buys, in
 * ONE fork — the same builder with the monitor off, at 100 us and at
 * 1 ms, so a rebuild or a republish between arms cannot move the number.
 *
 *   spawnJoinSeq   1 000 x (fork one tiny child, join it) from inside a
 *                  fiber: the five-way lane `own` wins; must not pay
 *   longBurst      8 fibers x ~60 us of CPU forked from inside a fiber:
 *                  the lane the monitor exists for
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class OwnMonitorBenchmark {

  @Param(Array("off", "100us", "1ms"))
  var monitor: String = "off"

  private var s: Schedulers.Running = null

  @Setup(Level.Trial)
  def start(): Unit =
    val b = Schedulers.own
    s = (monitor match
      case "off" => b.unmonitored
      case "100us" => b.monitorEvery(100000L)
      case "1ms" => b.monitorEvery(1000000L)
      case other => throw new IllegalArgumentException(other)).build

  @TearDown(Level.Trial)
  def stop(): Unit = s.close()

  @Benchmark
  def spawnJoinSeq(): Long =
    given Scheduler = s
    def loop(i: Int, sum: Long): Long ! Async =
      if i == 1000 then pure[Async, Long](sum)
      else async(Async.spawn(async(i))).flatMap(_.joinAsync).flatMap(v => loop(i + 1, sum + v))
    Async.spawn(loop(0, 0L)).join()

  private def spin(i: Int): Int =
    var r = i; var j = 0
    while j < 60000 do { r = Integer.rotateLeft(r * 1664525 + 1013904223, 7); j += 1 }
    r

  @Benchmark
  def longBurst(): Long =
    given Scheduler = s
    Async.spawn {
      async((0 until 8).map(i => Async.spawn(async(spin(i))))).flatMap { fs =>
        fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
      }
    }.join()
}
