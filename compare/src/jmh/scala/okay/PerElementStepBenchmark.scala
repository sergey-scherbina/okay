package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * free-cont-stack / channel-per-element-effect-cost: what the
 * per-element cost is actually MADE OF, once the re-association
 * hypothesis is dead.
 *
 * Counted with a probe in `runFree`, elementwise lane, N=4000:
 *
 *   rotate=64  bindPure=4001  bindInject=8020  pure=2
 *
 * The left-nested rotation — the case that rebuilds a node and a
 * closure, and the one an explicit continuation stack would remove —
 * fires 64 times in 12085 steps. Half a percent. The other 12021 are
 * `Bind(Pure, f)` and `Bind(Inject, f)`, which allocate nothing today.
 * So the interpreter's cost is not the SHAPE of a step, it is the
 * NUMBER of them: three interpreter steps and two effect injections
 * per element.
 *
 * Two of the three injections are structural and one is the CALLBACK.
 * `runForeach` takes `A => Unit ! Async`, so a caller with a plain
 * side effect lifts it — `Async.Run(() => sum += x)` — and pays an
 * Inject plus the Bind that sequences it, per element. This measures
 * what that lifting costs by walking the same source with a plain
 * function instead.
 *
 * It is a MEASUREMENT, not a proposed API: `Source.runForeach`'s own
 * documentation states the lifting rule deliberately, and the plain
 * door below would reverse it. The number decides whether that is a
 * conversation worth having.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class PerElementStepBenchmark {

  final val N = 4000
  private val list: List[Long] = (0L until N.toLong).toList

  /** today's surface: the callback is a program, lifted per element */
  @Benchmark
  def elem_effectCallback(): Long =
    var sum = 0L
    Channel.buffer(1024)(list).drained.runForeach(x =>
      okay.effect[Async, Unit](Async.Run(() => sum += x))).runWith
    sum

  /** the same walk, with the callback as a plain function: one Inject
   * and one Bind fewer per element, nothing else changed */
  @Benchmark
  def elem_plainCallback(): Long =
    var sum = 0L
    def go(rest: Source[Long]): Unit ! Async =
      Writer.uncons[Long, Unit, Async](rest).flatMap:
        case Right((a, more)) => sum += a; go(more)
        case Left(_) => okay.pure(())
    go(Channel.buffer(1024)(list).drained).runWith
    sum
}
