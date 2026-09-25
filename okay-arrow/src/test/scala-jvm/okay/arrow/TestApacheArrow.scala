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

  /** a column as comparable cells: None for a null */
  private def cells(c: Column): Vector[Option[Any]] = c match
    case Column.Int64(v, ok) => v.indices.map(i => Option.when(ok(i))(v(i))).toVector
    case Column.Float64(v, ok) => v.indices.map(i => Option.when(ok(i))(java.lang.Double.doubleToRawLongBits(v(i)))).toVector
    case Column.Utf8(v, ok) => v.indices.map(i => Option.when(ok(i))(v(i))).toVector
    case Column.Bool(v, ok) => v.indices.map(i => Option.when(ok(i))(v(i))).toVector
    case Column.Nulls(n) => Vector.fill(n)(None)

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
