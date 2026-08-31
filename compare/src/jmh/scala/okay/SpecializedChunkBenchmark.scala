package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq
import scala.compiletime.summonFrom
import scala.reflect.ClassTag

/**
 * What an UNBOXED chunk would be worth, before deciding whether to
 * build the machinery that produces one.
 *
 * `Chunk[A] = ArraySeq[A]`, and an `ArraySeq[Long]` may be backed by
 * `long[]` (`ofLong`) or by `Object[]` of boxed Longs (`ofRef`) —
 * both are honest `ArraySeq[Long]`, which is why choosing between
 * them at construction is SOUND, where a match type on the buffer
 * type was not. `inline` + `summonFrom[ClassTag[A]]` picks the
 * specialized branch exactly when the element type is static at the
 * call site, and falls back when it is not (verified).
 *
 * So the machinery exists. The question this measures is whether it
 * pays: the same read-heavy work over both backings, at the chunk
 * size the streaming layer actually uses.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class SpecializedChunkBenchmark {

  val size = 64

  /** the specialized backing: long[] */
  val unboxed: ArraySeq[Long] =
    ArraySeq.unsafeWrapArray(Array.tabulate(size)(_.toLong))

  /** what the generic producers build today: Object[] of boxed Longs */
  val boxed: ArraySeq[Long] =
    val arr = new Array[AnyRef](size)
    var i = 0
    while i < size do { arr(i) = java.lang.Long.valueOf(i.toLong); i += 1 }
    ArraySeq.unsafeWrapArray(arr).asInstanceOf[ArraySeq[Long]]

  // ---- reading: what every downstream operation does

  @Benchmark
  def sumUnboxed: Long =
    var acc = 0L; var i = 0
    while i < size do { acc += unboxed(i); i += 1 }
    acc

  @Benchmark
  def sumBoxed: Long =
    var acc = 0L; var i = 0
    while i < size do { acc += boxed(i); i += 1 }
    acc

  // ---- mapping: read, compute, write a new chunk

  @Benchmark
  def mapUnboxed: ArraySeq[Long] =
    val out = new Array[Long](size)
    var i = 0
    while i < size do { out(i) = unboxed(i) * 2; i += 1 }
    ArraySeq.unsafeWrapArray(out)

  @Benchmark
  def mapBoxed: ArraySeq[Long] =
    val out = new Array[AnyRef](size)
    var i = 0
    while i < size do { out(i) = java.lang.Long.valueOf(boxed(i) * 2); i += 1 }
    ArraySeq.unsafeWrapArray(out).asInstanceOf[ArraySeq[Long]]

  // ---- and the construction itself, both ways, through the seam
  //      that would choose between them

  inline def build[A](n: Int)(inline f: Int => A): ArraySeq[A] =
    summonFrom {
      case ct: ClassTag[A] =>
        val arr = ct.newArray(n)
        var i = 0
        while i < n do { arr(i) = f(i); i += 1 }
        ArraySeq.unsafeWrapArray(arr)
      case _ =>
        val arr = new Array[AnyRef](n)
        var i = 0
        while i < n do { arr(i) = f(i).asInstanceOf[AnyRef]; i += 1 }
        ArraySeq.unsafeWrapArray(arr).asInstanceOf[ArraySeq[A]]
    }

  /** A is static here: the specialized branch is taken */
  @Benchmark
  def buildStatic: ArraySeq[Long] = build[Long](size)(_.toLong)

  /** A is abstract at the allocation, as it is inside `Chunks.map` */
  def genericBuild[A](n: Int)(f: Int => A): ArraySeq[A] = build(n)(f)

  @Benchmark
  def buildGeneric: ArraySeq[Long] = genericBuild[Long](size)(_.toLong)
}
