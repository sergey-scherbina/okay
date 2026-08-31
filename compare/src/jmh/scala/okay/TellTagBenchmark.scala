package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import !.*

/**
 * What a runtime tag on a tell would cost.
 *
 * The library's writer operation IS its element — `opaque type
 * Writer[W, +A] = W`, no node, no allocation — and the price is that a
 * `Bind`'s answer type cannot be recovered by matching, so resuming
 * the continuation asserts it. The one encoding that would make it
 * provable is a GADT whose constructor carries the value:
 *
 *     enum Tell[W, +A]:
 *       case Say(w: W) extends Tell[W, Unit]
 *
 * Matching `Say(w)` refines `A = Unit`, and every cast in Writer and
 * Pipe goes away. The cost is one wrapper per tell, plus a pattern
 * match instead of a class test.
 *
 * This measures exactly that difference and nothing else: the same
 * 10k-tell program, built and folded, over an identity operation and
 * over a wrapped one. It is the number the decision should turn on.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class TellTagBenchmark {

  val n = 10000

  /** the GADT the alternative would use */
  enum Tag[W, +A]:
    case Say(w: W) extends Tag[W, Unit]

  /** the shipped path: the operation is the value.
   *
   * Folded by hand with the SAME loop shape as the tagged lane below —
   * an earlier version of this benchmark ran `Writer.run` here, which
   * also builds a 10k-element Seq, and reported the tag as 2x faster.
   * It was measuring the Seq. */
  @Benchmark
  def identityTell: Int =
    def go(i: Int): Unit ! Writer % Int =
      if i >= n then okay.pure(())
      else Writer.tell(i).flatMap(_ => go(i + 1))

    var count = 0
    def fold(x: Unit ! Writer % Int): Unit = (x.resume: @unchecked) match
      case Pure(_) => ()
      case Effect(_) => count += 1
      // the assertion this whole comparison is about
      case Bind(Effect(_), k) => count += 1; fold(k(okay.answer))
    fold(go(0))
    count

  /** the tagged path: one wrapper per tell, and the match that pays
   * for itself by refining the answer type */
  @Benchmark
  def taggedTell: Int =
    def go(i: Int): Unit ! ([X] =>> Tag[Int, X]) =
      if i >= n then okay.pure(())
      else okay.effect[[X] =>> Tag[Int, X], Unit](Tag.Say(i)).flatMap(_ => go(i + 1))

    var count = 0
    def fold(x: Unit ! ([X] =>> Tag[Int, X])): Unit = (x.resume: @unchecked) match
      case Pure(_) => ()
      case Effect(e) => e match
        // GADT refinement: A is Unit here, nothing is asserted
        case Tag.Say(_) => count += 1
      case Bind(Effect(e), k) => e match
        case Tag.Say(_) => count += 1; fold(k(()))
    fold(go(0))
    count
}
