package okay.spark

import okay.codec.Columns
import okay.codec.Columns.ColType
import org.apache.spark.sql.types.*

/**
 * spark-deep-values: `dataType` and `value` walk a `ColType`, `value` in
 * step with the value, TRAMPOLINED — no depth is refused by SparkSchema
 * (it used to refuse past 64, Arrow's limit, which Spark does not have).
 * The proof is a type thousands of levels deep, converted on a thread
 * with a small stack, where a JVM-stack walk would overflow.
 */
class TestSparkDepth extends munit.FunSuite:

  def arrays(depth: Int): ColType =
    var t: ColType = ColType.Int64
    var i = 1
    while i < depth do
      t = if i % 2 == 0 then ColType.Arr(t, true) else ColType.Struct(Vector(Columns.Field("f", t, false)))
      i += 1
    t

  def value(depth: Int): Any =
    var v: Any = 7L
    var i = 1
    while i < depth do
      v = if i % 2 == 0 then Vector(v) else Columns.Row(Vector(v))
      i += 1
    v

  /** run `a` on a thread with a 256 KB stack */
  def small[A](a: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("did not run"))
    val t = Thread(null, () => out = try Right(a) catch case e: Throwable => Left(e), "small-stack", 256 * 1024)
    t.start(); t.join()
    out.fold(throw _, identity)

  test("a type 5000 levels deep converts on a small stack, and so does a value of it") {
    val depth = 5000
    val d = small(SparkSchema.dataType(arrays(depth)))
    var cur: DataType = d
    var levels = 1
    var more = true
    while more do cur match
      case ArrayType(e, _) => cur = e; levels += 1
      case StructType(Array(f)) => cur = f.dataType; levels += 1
      case _ => more = false
    assertEquals(levels, depth)
    assertEquals(cur, LongType)
    val v = small(SparkSchema.value(arrays(depth), value(depth)))
    var x: Any = v
    var n = 1
    while x match { case _: Long => false; case _ => true } do
      x = x match
        case s: Seq[?] => s.head
        case r: org.apache.spark.sql.Row => r.get(0)
      n += 1
    assertEquals((n, x), (depth, 7L))
  }
