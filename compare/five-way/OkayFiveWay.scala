package bench.okay

// okay as a sixth runtime in github.com/stasimus/scala-effect-bench
// (82ac6f1). Copied into that repository's io-bench by apply.py; not
// compiled by okay's own build. Each workload mirrors the CE one in
// bench.matched (Workers.ce, PrimitivesBench.ceSpawnJoin,
// IoBench.ceBatch) operation for operation: a suspended step per
// `IO(...)`, a fiber per `.start`, a join per `joinWithNever`.

import java.util.concurrent.atomic.AtomicInteger
import okay.{!, Async, Scheduler, Schedulers, async, pure}
import okay.given

object OkayFiveWay:

  /** "okay" is what a user gets with no configuration — one Loom
   * virtual thread per fiber (`Schedulers.loom`, the default given on
   * JDK 21+); "okayOwn" is okay's own work-stealing scheduler over
   * platform threads. */
  private lazy val own: Scheduler = Schedulers.own.build
  def scheduler(runtime: String): Scheduler = runtime match
    case "okay"    => summon[Scheduler]
    case "okayOwn" => own
    case other     => throw new IllegalArgumentException(other)

  def isOkay(runtime: String): Boolean = runtime == "okay" || runtime == "okayOwn"

  /** the runtime entry: the program on one fiber, the caller joins it —
   * as the Loom backend submits to its executor and gets */
  def run[A](runtime: String)(prog: Scheduler ?=> A ! Async): A =
    given S: Scheduler = scheduler(runtime)
    Async.spawn(prog).join()

  /** Workers.ce: min(size, parallelism) workers started one after
   * another, each pulling the next index, joined one after another */
  def workers(values: Vector[Int], parallelism: Int)(f: Int => Int ! Async)(using Scheduler): Vector[Int] ! Async =
    async {
      require(parallelism > 0)
      (new AtomicInteger(0), new Array[Int](values.size))
    }.flatMap { (index, output) =>
      def worker: Unit ! Async =
        async(index.getAndIncrement()).flatMap { i =>
          if i >= values.size then pure[Async, Unit](())
          else f(values(i)).flatMap(v => async(output(i) = v)).flatMap(_ => worker)
        }
      async(Vector.fill(math.min(values.size, parallelism))(Async.spawn(worker))).flatMap { fibers =>
        joinAll(fibers.map(_.joinAsync)).flatMap(_ => async(output.toVector))
      }
    }

  /** PrimitivesBench.ceSpawnJoin: spawn a child, join it, repeat */
  def spawnJoin(ops: Int)(using Scheduler): Long ! Async =
    def loop(i: Int, sum: Long): Long ! Async =
      if i == ops then pure[Async, Long](sum)
      else async(Async.spawn(async(i))).flatMap(_.joinAsync).flatMap(v => loop(i + 1, sum + v))
    loop(0, 0L)

  /** IoBench.ceBatch: one worker per lane, lane + k * parallelism
   * indexes, the blocking call in place (a virtual thread parks on the
   * default scheduler) or the callback registered through Async.await */
  def batch(io: bench.io.Exchange, size: Int, parallelism: Int, blocking: Boolean)(using Scheduler): Vector[Int] ! Async =
    async(new Array[Int](size)).flatMap { output =>
      def call(lane: Int, index: Int): Int ! Async =
        if blocking then async(io.blocking(lane, index))
        else Async.await[Int](complete => io.async(lane, index)(complete))
      def worker(lane: Int, index: Int): Unit ! Async =
        if index >= size then pure[Async, Unit](())
        else call(lane, index).flatMap(v => async(output(index) = v)).flatMap(_ => worker(lane, index + parallelism))
      // each worker attempted, as ceBatch supervises `.attempt`: every
      // worker is joined before the first failure is raised
      async(Vector.range(0, math.min(size, parallelism)).map(lane => Async.spawn(Async.attempt(worker(lane, lane))))).flatMap { fibers =>
        fibers.foldLeft(pure[Async, Vector[Either[Throwable, Unit]]](Vector.empty))((acc, f) =>
          acc.flatMap(rs => f.joinAsync.map(rs :+ _))
        ).flatMap(results => async {
          results.foreach(_.fold(e => throw e, identity))
          output.toVector
        })
      }
    }

  /** join in order, as `traverse_(_.joinWithNever)` does */
  private def joinAll(joins: Vector[Unit ! Async]): Unit ! Async =
    joins.foldLeft(pure[Async, Unit](()))((acc, j) => acc.flatMap(_ => j))
