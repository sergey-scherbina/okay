package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * What the free selective costs to RUN (specs/applicative-static.md,
 * stage 2). A thousand leaves.
 *
 * THE MATCHED PAIR IS THE PREBUILT ONE. `staticToFree` converts a
 * spine built once at construction and runs it; `monadicPrebuilt`
 * runs an ordinary program built once at construction. Same work at
 * measurement time, so the difference is the detour through `Static`
 * and nothing else. The first cut of this file paired a PREBUILT
 * spine against a monadic lane that built itself every invocation —
 * a mismatched pair that flattered the spine, and the house rule
 * against exactly that is why it was caught before the number was
 * written down (benchmark-pairing-rule).
 *
 * `monadicBuildAndRun` keeps the building lane for context: it says
 * what constructing a thousand-node tree costs, which is most of why
 * the two prebuilt lanes are cheaper than it.
 *
 * `staticLeaves` reads the spine WITHOUT running it — the thing the
 * type exists for. It has no counterpart on the monadic side at any
 * price; it is here so the cost of ASKING is on the record.
 *
 * The handler answers in place, so what is measured is the tree and
 * the walk, not an effect's own work.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StaticBenchmark {

  enum Fetch[+A]:
    case Get(k: Int) extends Fetch[Int]

  import Fetch.*

  given Handler[Fetch] with
    def handle[A](e: Fetch[A]): A = e match
      case Get(k) => k

  private val N = 1000
  private val spine: Static[Fetch, Seq[Int]] = traverse(1 to N)(i => Static.op(Get(i)))

  private val prog: Seq[Int] ! Fetch = traverse(1 to N)(i => effect[Fetch, Int](Get(i)))

  @Benchmark
  def staticToFree(): Int = spine.toFree.runWith.sum

  @Benchmark
  def monadicPrebuilt(): Int = prog.runWith.sum

  @Benchmark
  def monadicBuildAndRun(): Int = traverse(1 to N)(i => effect[Fetch, Int](Get(i))).runWith.sum

  @Benchmark
  def staticLeaves(): Int = spine.leaves.length
}
