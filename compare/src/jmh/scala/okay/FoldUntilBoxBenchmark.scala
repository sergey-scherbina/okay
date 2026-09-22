package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * What `FoldUntil` costs over primitives, and whether an unboxed
 * variant would buy what `Fold.OfLong` bought (specs/fold-until.md,
 * "Out of scope" — measure before adding).
 *
 * The same shape as `FoldBoxBenchmark`: 10k Longs in chunks of 64,
 * summing into a Long, the fold arriving as DATA (nothing inlines).
 * The stop never fires, so every lane reads the whole input and the
 * only differences are the accumulator's declared type and the one
 * `done` branch per element.
 *
 *   foldBoxed          Chunks.fold, a generic Fold           (the boxed baseline)
 *   foldUntilBoxed     Chunks.foldUntil, a generic FoldUntil (+ one branch per element)
 *   foldOfLong         Chunks.fold, Fold.sumLong             (what specialisation bought Fold)
 *   loopUntilBoxed     the generic FoldUntil over materialized chunks, hand loop
 *   loopUntilOfLong    a PROTOTYPE OfLong FoldUntil over the same loop — the ceiling
 *                      a specialised `FoldUntil.OfLong` + dispatch could reach
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FoldUntilBoxBenchmark {

  val n = 10000
  val size = 64

  def longs: Chunks[Long] = Chunks.range(0L, n.toLong, size)

  val materialized: Vector[Chunk[Long]] =
    def go(p: Chunks[Long], acc: Vector[Chunk[Long]]): Vector[Chunk[Long]] =
      Chunks.pull(p) match
        case None => acc
        case Some((c, r)) => go(r, acc :+ c)
    go(longs, Vector.empty)

  // the folds as DATA, built once: a generic step behind an interface
  val boxedFold: Fold[Long, Long] = Fold(0L)(_ + _)
  val boxedUntil: FoldUntil[Long, Long, Long] = FoldUntil[Long, Long, Long](0L)(_ + _)(_ => false)(identity)

  /** the prototype: the accumulator declared where it is primitive,
   * `done` on a `long` — what `FoldUntil.OfLong` would be */
  trait OfLongUntil[-A]:
    def initLong: Long
    def addLong(s: Long, a: A): Long
    def doneLong(s: Long): Boolean
    def endLong(s: Long): Long

  val protoUntil: OfLongUntil[Long] = new:
    def initLong: Long = 0L
    def addLong(s: Long, a: Long): Long = s + a
    def doneLong(s: Long): Boolean = false
    def endLong(s: Long): Long = s

  // ---- the shipped paths, over the producing chunks

  @Benchmark
  def foldBoxed: Long = Chunks.fold(longs)(using boxedFold)

  @Benchmark
  def foldUntilBoxed: Long = Chunks.foldUntil(longs)(using boxedUntil)

  @Benchmark
  def foldOfLong: Long = Chunks.fold(longs)(using Fold.sumLong)

  /** the shipped specialisation, AFTER: `FoldUntil.long` through
   * `Chunks.foldUntil`'s dispatch */
  val untilOfLong: FoldUntil.OfLong[Long, Long] = FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)

  @Benchmark
  def foldUntilOfLong: Long = Chunks.foldUntil(longs)(using untilOfLong)

  // ---- the two variants over the SAME materialized loop: only the
  // accumulator's declared type differs

  @Benchmark
  def loopUntilBoxed: Long =
    val fo = boxedUntil
    var s = fo.init
    var j = 0
    while !fo.done(s) && j < materialized.length do
      val c = materialized(j)
      var i = 0
      while i < c.length && !fo.done(s) do
        s = fo.add(s, c(i))
        i += 1
      j += 1
    fo.end(s)

  @Benchmark
  def loopUntilOfLong: Long =
    val fo = protoUntil
    var s = fo.initLong
    var j = 0
    while !fo.doneLong(s) && j < materialized.length do
      val c = materialized(j)
      var i = 0
      while i < c.length && !fo.doneLong(s) do
        s = fo.addLong(s, c(i))
        i += 1
      j += 1
    fo.endLong(s)
}
