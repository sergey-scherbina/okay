package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * What a `Gen` pipeline costs against the hand-written road it is
 * made of (sprint generators-jmh; specs/generators.md, Results).
 *
 * `Gen[W]` is a value class over `Unit ! Writer % W + Stop`: `map`
 * IS `Writer.map`, the readers ARE `FoldUntil` walks, the chain is
 * fused into the reader (gen-chain-fusion, gen-flatmap-fusion). So
 * parity with a Writer program read by `Writer.foldUntil` is the
 * expectation, and the lanes are paired to find where it does not
 * hold: the same 10 000 Longs, unfolded, on both roads.
 *
 *   genUnfoldToList     Gen.unfold(...).toList
 *   writerUnfoldCollect the same program at Writer % Long, Writer.run
 *   writerUnfoldFoldUntil the same program, Writer.foldUntil(collecting) (the Stop-free floor)
 *   genOfToList         Gen.of(prog).toList — the SAME prog, widened by Stop ONLY
 *                        (gen-read-stop-residual: isolates the row-width cost — none)
 *   writerUnfoldShapedFoldUntil the SAME shape unfold uses (S => Option[(W,S)]), Writer-only, no Stop
 *                        (isolates unfold's Option/Tuple tax — the real residual)
 *   genPipelineToList   .map(_ * 2).filter(_ % 3 == 0).toList
 *   writerPipelineCollect Writer.map + a filtering Writer.fold step     (the hand road for the pipeline)
 *   genTakeToList       an INFINITE unfold, .take(n).toList
 *   genIteratorSum      .iterator, summed — the Stepper
 *   sourceRunCollect    Source.range(0, n).runCollect (Async row, Vector) — the entry's third comparator
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class GenBenchmark {

  val n = 10000L

  def gen: Gen[Long] = Gen.unfold(0L)(i => if i < n then Some((i, i + 1)) else None)
  def genInfinite: Gen[Long] = Gen.unfold(0L)(i => Some((i, i + 1)))

  /** the same unfold as a plain Writer program: what Gen.unfold builds, minus the Stop row */
  def prog: Unit ! Writer % Long =
    def go(i: Long): Unit ! Writer % Long =
      Free.delay(() => if i < n then Writer.tell(i).flatMap(_ => go(i + 1)) else pure(()))
    go(0L)

  /** Gen's own collecting reader, spelled here (it is private there) */
  val collecting: FoldUntil[Long, List[Long], List[Long]] = new:
    def init: List[Long] = Nil
    def add(s: List[Long], a: Long): List[Long] = a :: s
    def done(s: List[Long]): Boolean = false
    def end(s: List[Long]): List[Long] = s.reverse

  // ---- unfold, read whole

  @Benchmark
  def genUnfoldToList: List[Long] = gen.toList

  // `Writer.run`/`fold`/`foldUntil`, not the inline `loopWith` directly:
  // at `F = Nothing` the inline `split` expands to `Nothing[A]`, which
  // does not compile; the plain defs take `Nothing` at their boundary
  @Benchmark
  def writerUnfoldCollect: List[Long] =
    !.run(Writer.run[Long, Unit, Nothing](prog))._1.toList

  @Benchmark
  def writerUnfoldFoldUntil: List[Long] =
    !.run(Writer.foldUntil[Long, List[Long], Unit, List[Long], Nothing](prog)(using summon, collecting))

  /** gen-read-stop-residual: the SAME `prog`, widened ONLY by Stop
   * (`.plus[Stop]`, what `Gen.of` does) and read through `Gen.foldUntil`
   * — isolates the row-width cost (the Stop arm in every split) from
   * whatever `unfold`'s own shape (Free.delay per step, Chain/Xf) adds */
  @Benchmark
  def genOfToList: List[Long] = Gen.of(prog).toList

  /** the second isolation: `unfold`'s OWN generative shape — a step
   * function `S => Option[(W, S)]`, which boxes an `Option`, a
   * `Tuple2` and (for a `Long` state/element) two `Long`s per step —
   * written directly for `Writer % Long`, NO Stop anywhere. If this
   * reads close to `genUnfoldToList` and NOT to `prog`, the tax is
   * `unfold`'s signature, not Gen's row */
  def progUnfoldShaped: Unit ! Writer % Long =
    def go(s: Long): Unit ! Writer % Long = Free.delay(() =>
      (if s < n then Some((s, s + 1)) else None) match
        case Some((w, s2)) => Writer.tell(w).flatMap(_ => go(s2))
        case None => pure(()))
    go(0L)

  @Benchmark
  def writerUnfoldShapedFoldUntil: List[Long] =
    !.run(Writer.foldUntil[Long, List[Long], Unit, List[Long], Nothing](progUnfoldShaped)(using summon, collecting))

  // ---- the pipeline

  @Benchmark
  def genPipelineToList: List[Long] = gen.map(_ * 2).filter(_ % 3 == 0).toList

  val filtering: Fold[Long, List[Long]] = Fold(Nil)((l, a) => if a % 3 == 0 then a :: l else l)

  @Benchmark
  def writerPipelineCollect: List[Long] =
    val mapped = Writer.map[Long, Long, Unit, Nothing](prog)(_ * 2)
    !.run(Writer.fold[Long, List[Long], Unit, Nothing](mapped)(using summon, filtering))._1.reverse

  // ---- the barriers, fused (gen-flatmap-fusion): flatMap, ++, zipWithIndex

  @Benchmark
  def genFlatMapToList: List[Long] = gen.flatMap(i => Gen.emit(i).map(_ + 1)).toList

  /** the hand road for flatMap's shape: a Writer program telling once per element */
  @Benchmark
  def writerFlatMapCollect: List[Long] =
    val mapped = Writer.map[Long, Long, Unit, Nothing](prog)(_ + 1)
    !.run(Writer.run[Long, Unit, Nothing](mapped))._1.toList

  @Benchmark
  def genConcatToList: List[Long] = (gen ++ gen).toList

  @Benchmark
  def genZipWithIndexSum: Long =
    var s = 0L
    gen.zipWithIndex.foreach((x, i) => s += x + i)
    s

  // ---- take, iterator, and the async comparator

  @Benchmark
  def genTakeToList: List[Long] = genInfinite.take(n.toInt).toList

  @Benchmark
  def genIteratorSum: Long =
    val it = gen.iterator
    var s = 0L
    while it.hasNext do s += it.next()
    s

  @Benchmark
  def sourceRunCollect: Vector[Long] = Source.range(0L, n).runCollect.runWith
}
