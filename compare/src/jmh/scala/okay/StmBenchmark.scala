package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * stm-js-direct-bench: the JVM reference for `BenchStmCross` — the
 * same chain of N transactions under `Stm.tl2` and `Stm.direct`,
 * single fibre, no contention, through `runWith`, with the `async(i)`
 * chain as the control (the bind cost alone; PerElementStepBenchmark
 * `bind_runWith` is the same shape). Lane minus control is the
 * transaction. `-prof gc` says what a transaction allocates.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StmBenchmark {

  final val N = 4000

  private def chain(s: Stm[Async], tx: TRef[Long] => Long ! Tx): Long =
    val r = TRef(0L)
    def go(i: Long, acc: Long): Long ! Async =
      if i >= N then okay.pure(acc)
      else s.atomically(tx(r)).flatMap(x => go(i + 1, acc + x))
    go(0L, 0L).runWith

  private def modify(r: TRef[Long]): Long ! Tx = Tx.modify(r)(x => (x + 1, x))
  private def readWrite(r: TRef[Long]): Long ! Tx =
    Tx.read(r).flatMap(x => Tx.write(r, x + 1).map(_ => x))

  @Benchmark
  def control(): Long =
    def go(i: Long, acc: Long): Long ! Async =
      if i >= N then okay.pure(acc)
      else async(i).flatMap(x => go(i + 1, acc + x))
    go(0L, 0L).runWith

  @Benchmark def tl2Modify(): Long = chain(Stm.tl2, modify)
  @Benchmark def directModify(): Long = chain(Stm.direct, modify)
  @Benchmark def tl2ReadWrite(): Long = chain(Stm.tl2, readWrite)
  @Benchmark def directReadWrite(): Long = chain(Stm.direct, readWrite)
}
