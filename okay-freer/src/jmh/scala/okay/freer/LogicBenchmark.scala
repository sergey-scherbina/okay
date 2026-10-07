package okay.freer

import okay.{guard}
import okay.given

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.freer.Row.at

/**
 * logic-cut-releases: `observe` (msplit, a split per answer) over a binary choice tree of depth 10 — no scope, so
 * the price of carrying branch points nobody shares; and `runChoice` over a scope holding one acquisition with a
 * choice of 100 under it — the shared resource's holders and branch point.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class LogicBenchmark {

  type R = Choose + Pure
  given Failing[R] = new Failing[R]:
    def guard[X](e: R[X], onFailure: () => Unit): R[X] = e

  def tree(d: Int): Int ! R =
    if d == 0 then pure(1) else effect[R, Int](Choose(Seq(0, 1))).flatMap(x => tree(d - 1).map(_ + x))

  val alternatives: Seq[Int] = 0 until 100

  def scope: Int ! R =
    Resource.run[Int, R](Resource.acquire(1)(_ => ()).at[Resource + R].flatMap(_ =>
      effect[Choose, Int](Choose(alternatives)).at[Resource + R]))

  @Benchmark def observe_tree(): Int = !.run(Logic.observe[Int, Pure](1024)(tree(10))).sum
  @Benchmark def shared_scope(): Int = !.run(runChoice[Int, Pure](scope)).sum
}
