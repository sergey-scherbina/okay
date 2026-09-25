package okay.arrow

import org.apache.arrow.vector.VectorSchemaRoot
import org.apache.arrow.vector.types.pojo.{ArrowType, Field, FieldType, Schema}

/**
 * stack-safety-arrow, on the JVM: a schema nested far past the limit is
 * what an adversarial file looks like, and the reader walked it with one
 * frame per level. Built on a big stack (the flatbuffer writer recurses
 * too), read on a small one.
 */
class TestArrowDepth extends munit.FunSuite:

  private def onStack[A](bytes: Long)(body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "stack", bytes)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  test("a schema nested 100 000 deep is refused by name, not a stack overflow") {
    val bytes = onStack(1L << 30)(TestArrowNesting.schemaOnly(100000))
    val e = intercept[IllegalStateException](onStack(256L * 1024)(OkayArrow.read(bytes)))
    assert(e.getMessage.contains("64"), e.getMessage)
  }

  test("Arrow Java vectors nested past the limit are refused by name") {
    var f = Field("item", FieldType.nullable(ArrowType.Int(64, true)), null)
    var i = 0
    while i < Column.MaxNesting + 1 do
      f = Field("item", FieldType.nullable(ArrowType.List.INSTANCE), java.util.List.of(f)); i += 1
    val alloc = org.apache.arrow.memory.RootAllocator()
    try
      val root = VectorSchemaRoot.create(Schema(java.util.List.of(f)), alloc)
      try
        val e = intercept[IllegalStateException](ApacheArrow.fromRoot(root))
        assert(e.getMessage.contains("64"), e.getMessage)
      finally root.close()
    finally alloc.close()
  }

  for levels <- Seq(0, 1, 2, Column.MaxNesting) do
    test(s"an EMPTY Arrow Java root, $levels list level(s) around an int64, converts") {
      // Arrow Java allocates a list's offsets with its first value, so an
      // empty one has none, and reading offset 0 threw before
      var f = Field("item", FieldType.nullable(ArrowType.Int(64, true)), null)
      var i = 0
      while i < levels do { f = Field("item", FieldType.nullable(ArrowType.List.INSTANCE), java.util.List.of(f)); i += 1 }
      val alloc = org.apache.arrow.memory.RootAllocator()
      try
        val root = VectorSchemaRoot.create(Schema(java.util.List.of(f)), alloc)
        try
          val t = ApacheArrow.fromRoot(root)
          assertEquals((t.rows, Column.nesting(t.cols.head._2)), (0, levels))
        finally root.close()
      finally alloc.close()
    }
