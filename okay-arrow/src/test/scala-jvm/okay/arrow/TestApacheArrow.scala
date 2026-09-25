package okay.arrow

/** stage 2: the facade over Arrow Java, and the two implementations
 * reading each other's streams */
class TestApacheArrow extends munit.FunSuite:

  private val t = Table(Vector(
    "id" -> Column.Int64(Array(1L, -2L, Long.MaxValue, 0L), Array(true, true, true, false)),
    "temp" -> Column.Float64(Array(0.5, Double.NaN, -0.0, 1e300), Array(true, true, false, true)),
    "site" -> Column.Utf8(Array("kyiv", "", "чай ☕", "x"), Array(true, true, true, false)),
    "ok" -> Column.Bool(Array(true, false, true, true), Array(true, true, false, true)),
    "nothing" -> Column.Nulls(4)),
    Vector("okay" -> """{"id":7}"""))

  private def cells(c: Column): Vector[Option[Any]] = Tables.cells(c)

  private def same(a: Table, b: Table): Unit =
    assertEquals(b.metadata, a.metadata)
    assertEquals(b.cols.map(_._1), a.cols.map(_._1))
    a.cols.zip(b.cols).foreach { case ((n, x), (_, y)) =>
      assertEquals(y.getClass, x.getClass, n)
      assertEquals(cells(y), cells(x), n)
    }

  test("the default codec is ours; importing ApacheArrow.given picks Arrow Java") {
    assertEquals(summon[ArrowCodec].name, "okay")
    import ApacheArrow.given
    assertEquals(summon[ArrowCodec].name, "apache")
  }

  test("ApacheArrow round-trips the model: every column kind, nulls, NaN, text, metadata") {
    same(t, ApacheArrow.read(ApacheArrow.write(t)))
  }

  test("each implementation reads the other's stream, value for value") {
    same(t, ApacheArrow.read(OkayArrow.write(t)))
    same(t, OkayArrow.read(ApacheArrow.write(t)))
  }

  test("toRoot and fromRoot move a table into Arrow Java's columns and back") {
    val alloc = org.apache.arrow.memory.RootAllocator()
    try
      val root = ApacheArrow.toRoot(t, alloc)
      try
        assertEquals(root.getRowCount, 4)
        same(t, ApacheArrow.fromRoot(root))
      finally root.close()
    finally alloc.close()
  }

  test("without Arrow Java on the classpath, the refusal says what to add") {
    val why = ApacheArrow.missing("org.apache.arrow.NotThere")
    assert(why.exists(w => w.contains("optional dependency of okay-arrow") && w.contains("arrow-vector")
      && w.contains("okay.arrow.OkayArrow")), why.toString)
    assertEquals(ApacheArrow.missing(), None)
  }

  test("every kind of column: each implementation reads the other's stream") {
    val t = Tables.everything
    assertEquals(Tables.same(t, ApacheArrow.read(ApacheArrow.write(t))), None)
    assertEquals(Tables.same(t, ApacheArrow.read(OkayArrow.write(t))), None)
    assertEquals(Tables.same(t, OkayArrow.read(ApacheArrow.write(t))), None)
  }

  test("a dictionary-encoded column (written by Arrow Java) reads as its values") {
    import org.apache.arrow.vector.{IntVector, VarCharVector, VectorSchemaRoot}
    import org.apache.arrow.vector.dictionary.{Dictionary, DictionaryEncoder, DictionaryProvider}
    import org.apache.arrow.vector.types.pojo.{ArrowType, DictionaryEncoding}
    val alloc = org.apache.arrow.memory.RootAllocator()
    try
      val values = VarCharVector("dict", alloc); values.allocateNew()
      Vector("kyiv", "lviv").zipWithIndex.foreach((s, i) => values.setSafe(i, s.getBytes("UTF-8"))); values.setValueCount(2)
      val dict = Dictionary(values, DictionaryEncoding(1L, false, ArrowType.Int(32, true)))
      val raw = VarCharVector("city", alloc); raw.allocateNew()
      Vector("lviv", "kyiv", "lviv").zipWithIndex.foreach((s, i) => raw.setSafe(i, s.getBytes("UTF-8"))); raw.setNull(3); raw.setValueCount(4)
      val root = DictionaryEncoder.encode(raw, dict) match
        case encoded: IntVector => VectorSchemaRoot.of(encoded)
        case other => fail(s"int32 indices, not ${other.getClass}")
      val provider = DictionaryProvider.MapDictionaryProvider(dict)
      val out = java.io.ByteArrayOutputStream()
      val w = org.apache.arrow.vector.ipc.ArrowStreamWriter(root, provider, java.nio.channels.Channels.newChannel(out))
      w.start(); w.writeBatch(); w.end(); w.close()
      val expected = Vector(Some("lviv"), Some("kyiv"), Some("lviv"), None)
      for codec <- Vector(OkayArrow, ApacheArrow) do
        val t = codec.read(out.toByteArray)
        assertEquals(t.cols.map(c => Tables.cells(c._2)), Vector(expected), codec.name)
      root.close(); raw.close(); values.close()
    finally alloc.close()
  }

