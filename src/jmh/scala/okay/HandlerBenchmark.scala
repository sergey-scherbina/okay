package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*
import scala.annotation.nowarn

/** an operation carrying its own answer, for handler benchmarks */
// `derives Effect`: since f9417643 (2026-09-08) every signature declares
// its own runtime test — the erasure fallback is gone — and this lane's
// relay/handle split needs one. Found by handler-fusion-gate: the
// landing that removed the fallback did not reach the Jmh configuration.
case class Ask[+A](a: A) derives okay.Effect

/**
 * The previously unbenchmarked paths: tail-resumptive relay vs the
 * general Effects.handle on the same forwarding-heavy program; bulk
 * vs one-by-one stepping (the reflection-without-remorse probe that
 * makes the type-aligned-queue idea falsifiable); and the State
 * effect handler vs the parameterised PState on the same workload.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class HandlerBenchmark {

  final val N = 10000

  /** 10k ops, every 100th handled (Ask), the rest forwarded (Produce) */
  def prog: Int ! (Ask + Produce) =
    (1 to N).foldLeft(effect[Ask + Produce, Int](Ask(0))): (m, i) =>
      m.flatMap(x => effect[Ask + Produce, Int](if i % 100 == 0 then Ask(x + 1) else x + 1))

  // relay/handle inline a type test on the operation type, which
  // erasure cannot verify for Ask[Nothing] — the trusted kernel's
  // warning (Effects.scala), not a cast this file adds
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def relayForward(): Int =
    relay[Int, Int, Ask, Produce](prog)(pure(_))([X, Y] => a => Cont.Pure(a.a)).runWith

  // relay/handle inline a type test on the operation type, which
  // erasure cannot verify for Ask[Nothing] — the trusted kernel's
  // warning (Effects.scala), not a cast this file adds
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def handleForward(): Int =
    Effects[Free].handle[Ask, Produce](prog)(pure(_))([X] => a => Cont.Pure(a.a)).runWith

  @Benchmark
  def stepBulk(): Any =
    fibs[Int, Producer].next(N).?

  @Benchmark
  def stepOneByOne(): Any =
    var p = fibs[Int, Producer]
    var i = 0
    while i < N do
      p = p.next(1)
      i += 1
    p.?

  final val M = 1000

  @Benchmark
  def stateEffect(): (Long, Long) =
    State.run(0L):
      (1 to M).foldLeft(0L.state[Long]): (m, _) =>
        m.flatMap(_ => State.get[Long].flatMap(s => State.set[Long](s + 1)))

  @Benchmark
  def statePara(): (Long, Long) =
    PState.run(0L):
      (1 to M).foldLeft(PState.get[Long, (Long, Long)]): (m, _) =>
        m.flatMap(_ => PState.get.flatMap(s => PState.set(s + 1)))
}
