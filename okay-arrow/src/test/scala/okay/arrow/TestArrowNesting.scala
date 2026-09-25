package okay.arrow

/**
 * stack-safety-arrow: a column's TYPE nests (a list of lists of structs …),
 * and every walk here recurses once per level of it. The levels are capped
 * where Arrow's own reference implementation caps them — C++'s
 * `IpcReadOptions::max_recursion_depth`, `kMaxNestingDepth` = 64 — at every
 * door a type comes in by: a stream's schema, a `Table`, Arrow Java's
 * vectors. Past it is a refusal by name, not a stack overflow.
 */
class TestArrowNesting extends munit.FunSuite:

  /** `levels` lists around one int64 row */
  def nested(levels: Int): Column =
    var c: Column = Column.Int64(Array(7L), Array(true))
    var i = 0
    while i < levels do { c = Column.ListOf(Array(0, 1), c, Array(true)); i += 1 }
    c

  test("a column nested exactly the limit writes and reads back") {
    val t = Table(Vector("deep" -> nested(Column.MaxNesting)), Vector.empty)
    val back = OkayArrow.read(OkayArrow.write(t))
    assertEquals(Tables.same(t, back), None)
  }

  test("a Table refuses a column nested past the limit, by name") {
    val e = intercept[IllegalArgumentException](Table(Vector("deep" -> nested(Column.MaxNesting + 1)), Vector.empty))
    assert(e.getMessage.contains("'deep'") && e.getMessage.contains("64"), e.getMessage)
  }

  test("a stream whose schema nests past the limit is refused, and exactly the limit is read") {
    assertEquals(OkayArrow.read(TestArrowNesting.schemaOnly(Column.MaxNesting)).cols.map(_._1), Vector("item"))
    val e = intercept[IllegalStateException](OkayArrow.read(TestArrowNesting.schemaOnly(Column.MaxNesting + 1)))
    assert(e.getMessage.contains("64"), e.getMessage)
  }

object TestArrowNesting:
  /** an Arrow stream of one schema message and the end marker: one field,
   * `levels` lists around an int64, and no batch — built from the
   * flatbuffer tables directly, so it can say what no writer here will */
  def schemaOnly(levels: Int): Array[Byte] =
    def fieldOf(typeId: Int, tpe: Fb.Table, children: Vector[Fb.Table]) =
      Fb.Table(Vector(Some(Fb.Str("item")), Some(Fb.Bool(true)), Some(Fb.U8(typeId)), Some(tpe), None, Some(Fb.Tables(children))))
    var f = fieldOf(2, Fb.Table(Vector(Some(Fb.I32(64)), Some(Fb.Bool(true)))), Vector.empty)   // Int(64, signed)
    var i = 0
    while i < levels do { f = fieldOf(12, Fb.Table(Vector()), Vector(f)); i += 1 }               // List
    val schema = Fb.Table(Vector(Some(Fb.I16(0)), Some(Fb.Tables(Vector(f))), None))
    // version V5 (4), header Schema (1), no body
    val fb = Fb.finish(Fb.Table(Vector(Some(Fb.I16(4)), Some(Fb.U8(1)), Some(schema), Some(Fb.I64(0L)))))
    val padded = (fb.length + 7) / 8 * 8
    val out = java.nio.ByteBuffer.allocate(8 + padded + 8).order(java.nio.ByteOrder.LITTLE_ENDIAN)
    out.putInt(-1).putInt(padded).put(fb)
    out.position(8 + padded)
    out.putInt(-1).putInt(0)
    out.array()
