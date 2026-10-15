package okay.std.bench

import okay.{!, effect, pure}
import okay.std.*

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * logic-cut-releases: `observe` (msplit, a split per answer) over a binary choice tree of depth 10 — no scope, so
 * the price of carrying branch points nobody shares; and `runChoice` over a scope holding one acquisition with a
 * choice of 100 under it — the shared resource's holders and branch point.
 *
 * On the new road the row is `Choose` (the classic's `Choose + Pure`), and the classic's `Failing[R]` given is not
 * needed: Resource is a kernel `Deep` handler with a guard. Known change of behaviour (specs/freer-on-core.md): a
 * multi-shot `Choose` outside the scope releases at the end of the FIRST branch, where the classic shared the
 * resource and released after the last — the same 100 alternatives run either way.
 *
 * MEASURED ONCE BY HAND (2026-10-15, one call, not JMH): `observe_tree` on the first conversion grew about 8x a
 * level (depth 10 out of a 1 GB heap); `msplit` stepped (a rest the search's worklist) it is linear in the leaves,
 * depth 10 in ~0.5 ms (`TestLogicScaling`).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class LogicBenchmark {

  type R = Choose

  def tree(d: Int): Int ! R =
    if d == 0 then pure(1) else effect[Choose, Int](Choose(Seq(0, 1))).flatMap(x => tree(d - 1).map(_ + x))

  val alternatives: Seq[Int] = 0 until 100

  def scope: Int ! R =
    Resource.run[Int, okay.+[Resource, Choose]](Resource.acquire(1)(_ => ()).flatMap(_ =>
      effect[Choose, Int](Choose(alternatives))))

  @Benchmark def observe_tree(): Int = Logic.observe[Int, R](1024)(tree(10)).run.sum
  @Benchmark def shared_scope(): Int = runChoice[Int, R](scope).run.sum
}
