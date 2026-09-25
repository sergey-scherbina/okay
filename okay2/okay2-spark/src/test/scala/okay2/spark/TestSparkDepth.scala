package okay2.spark

import okay2.codec.Columns
import okay2.codec.Columns.ColType
import org.apache.spark.sql.types._

/**
 * stack-safety-catch-up-okay2: `dataType` and `value` walk a `ColType`,
 * `value` in step with the value. A type is refused past
 * `SparkSchema.MaxNesting` (64, Arrow's own limit) before any walk
 * starts, so the walks recurse at most that deep.
 */
class TestSparkDepth extends munit.FunSuite {

  def arrays(depth: Int): ColType = {
    var t: ColType = ColType.Int64
    var i = 1
    while (i < depth) {
      t = if (i % 2 == 0) ColType.Arr(t, true) else ColType.Struct(Vector(Columns.Field("f", t, false)))
      i += 1
    }
    t
  }

  test("a type 64 levels deep converts, and so does a value of it") {
    var d: DataType = SparkSchema.dataType(arrays(SparkSchema.MaxNesting))
    var levels = 1
    var more = true
    while (more) d match {
      case ArrayType(e, _) => d = e; levels += 1
      case s: StructType if s.fields.length == 1 => d = s.fields(0).dataType; levels += 1
      case _ => more = false
    }
    assertEquals(levels, SparkSchema.MaxNesting)
    assertEquals(d, LongType: DataType)
    assertEquals(SparkSchema.value(ColType.Arr(ColType.Int64, true), Vector(1L, 2L)), Vector(1L, 2L): Any)
  }

  test("a type past 64 levels is refused by name, however deep") {
    for (depth <- Vector(SparkSchema.MaxNesting + 1, 200000)) {
      val t = arrays(depth)
      val e = intercept[IllegalArgumentException](SparkSchema.dataType(t))
      assert(e.getMessage.contains("nested deeper than 64"), e.getMessage)
      val e2 = intercept[IllegalArgumentException](SparkSchema.value(t, null))
      assert(e2.getMessage.contains("nested deeper than 64"), e2.getMessage)
    }
  }
}
