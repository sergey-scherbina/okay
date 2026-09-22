package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * What the parallel applicative's SPINE costs (specs/applicative-static.md,
 * stage 1). Eight trivial leaves, and TWO PAIRS — because one table
 * of four lanes measuring two different questions is how a mismatched
 * pair gets read as a verdict (benchmark-pairing-rule).
 *
 * THE WRAPPER, matched exactly: `bracketPar8` writes the idiom
 * bracket by hand at this carrier — `fmap(l1, f).app(l2)...` — which
 * is SEVEN `app` calls and therefore seven `Async.par` joins, against
 * `handNested8`, seven `Async.par` calls written out. Same shape,
 * same leaves, same answer; the difference is the carrier's two
 * closures per join. `fmap` is used for the head precisely so the
 * count matches: `pure(f).app(l1)` would have made it eight.
 *
 * THE DOOR, a different question: `parApplicative8` is
 * `Par.sequence`, the generic `traverse` at this carrier, against
 * `parAllFlat8`, the older `parAll` — one fiber per leaf, no nesting,
 * joined in order. These two answer "which door should I reach for",
 * and they are NOT a wrapper measurement: `traverse` also builds its
 * Vector element by element, which `parAll` does not.
 *
 * `sequential8` is the floor: `traverse` at the program's own
 * instance, no fibers at all.
 *
 * The leaves are trivial on purpose: with no work in them the numbers
 * are the spine's own cost, which is what is being decided.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ParBenchmark {

  private val leaves: Seq[Int ! Async] = (1 to 8).map(i => async(i))

  @Benchmark
  def parApplicative8(): Int = Par.sequence(leaves).runWith.sum

  @Benchmark
  def handNested8(): Int =
    // the left-nested shape `traverse`'s foldLeft builds, written out
    val Seq(a, b, c, d, e, f, g, h) = leaves: @unchecked
    Async.par(Async.par(Async.par(Async.par(Async.par(Async.par(Async.par(a, b), c), d), e), f), g), h)
      .map { case (((((((x1, x2), x3), x4), x5), x6), x7), x8) =>
        x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 }
      .runWith

  @Benchmark
  def bracketPar8(): Int =
    val A = summon[Applicative[Par]]
    val Seq(a, b, c, d, e, f, g, h) = leaves.map(Par(_)): @unchecked
    A.fmap(a, (x1: Int) => (x2: Int) => (x3: Int) => (x4: Int) =>
              (x5: Int) => (x6: Int) => (x7: Int) => (x8: Int) =>
                x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8)
      .app(b).app(c).app(d).app(e).app(f).app(g).app(h).seq.runWith

  @Benchmark
  def parAllFlat8(): Int = parAll(leaves).runWith.sum

  @Benchmark
  def sequential8(): Int = sequence(leaves).runWith.sum
}
