package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * "countdown", the canonical shape of the Kammar-Lindley-Oury /
 * effect-handlers-bench suite (ICFP 2013's "Handlers in action";
 * the descendant suite Koka, Effekt and OCaml 5's own papers report
 * on): N handled state operations in a tight recursive loop, TAIL-
 * resumptive throughout — every effect runtime's cheapest possible
 * shape for a handler, which is exactly why it is the suite's first
 * lane. `handlers-bench-suite` (backlog) took this ONE shape rather
 * than the whole suite for a first landing; `ChoiceBenchmark` already
 * covers the suite's search/multi-shot family (nqueens/triples'
 * shape) and `GeneratorBenchmark` its generator family (fibonacci's);
 * the rest — handler_sieve (a deep handler STACK), resume_nontail (a
 * handler resuming outside tail position), tree_explore,
 * parsing_dollars — are named, not built, in specs/handlers-bench-
 * suite.md's Out of scope, each with the reason it did not make this
 * pass.
 *
 * `stdLoop` is the hand ceiling (lane rule 1: a competitor's number
 * beside the shape that pays no effect machinery at all, never
 * alone). Published figures for Koka/OCaml5/Effekt exist in their own
 * papers, on THEIR OWN machines — quoted in the spec with a citation,
 * never in this table, because a cross-machine wall-clock number is
 * not a comparison this repo's own lane rules would accept from
 * anyone else (rule 4: a lane whose number depends on the machine is
 * not comparable across machines).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class CountdownBenchmark {

  final val N = 100000

  /** the hand ceiling: no effect machinery, no allocation past the loop itself */
  @Benchmark
  def stdLoop(): Long =
    var acc = 0L
    var i = N
    while i > 0 do { acc += 1; i -= 1 }
    acc

  @Benchmark
  def okayCountdown(): Long =
    def go(n: Int): Long ! State % Long =
      if n == 0 then State.get[Long]
      else State.modify[Long](_ + 1).flatMap(_ => go(n - 1))
    State.run(0L)(go(N))._2

  @Benchmark
  def kyoCountdown(): Long =
    import _root_.kyo.*
    def go(n: Int): Long < Var[Long] =
      if n == 0 then Var.get[Long]
      else Var.update[Long](_ + 1).flatMap((_: Long) => go(n - 1))
    Var.runTuple(0L)(go(N)).eval._2

  @Benchmark
  def catsState(): Long =
    import cats.data.State
    def go(n: Int): State[Long, Long] =
      if n == 0 then State.get
      else State.modify[Long](_ + 1).flatMap(_ => go(n - 1))
    go(N).run(0L).value._1

  @Benchmark
  def zioRef(): Long =
    import _root_.zio.*
    def go(n: Int, ref: Ref[Long]): UIO[Long] =
      if n == 0 then ref.get
      else ref.update(_ + 1) *> go(n - 1, ref)
    Unsafe.unsafe(implicit u =>
      Runtime.default.unsafe.run(Ref.make(0L).flatMap(go(N, _))).getOrThrowFiberFailure())
}
