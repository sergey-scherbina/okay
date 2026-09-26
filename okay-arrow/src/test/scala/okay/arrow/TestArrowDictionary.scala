package okay.arrow

/** okay-arrow stage 8: dictionary-encoded columns, kept and written */
class TestArrowDictionary extends munit.FunSuite:
  private def levels(xs: String*) = Column.Utf8(xs.toArray, Array.fill(xs.length)(true))
  // an R factor as it crosses: the levels in their order, one never used ("self-employed")
  private val employment: Column.Dictionary = Column.Dictionary(Array(0, 2, 0, 1, 0), levels("employed", "unemployed", "self-employed"),
    ordered = false, valid = Array(true, true, false, true, true))
  private val table = Table(Vector("id" -> Column.Int64(Array(1L, 2, 3, 4, 5), Array.fill(5)(true)),
    "employment" -> employment,
    "grade" -> Column.Dictionary(Array(1, 0, 2, 2, 1), levels("low", "mid", "high"), ordered = true, valid = Array.fill(5)(true))),
    Vector.empty)

  private def assertKept(got: Column, want: Column.Dictionary): Unit = got match
    case Column.Dictionary(idx, Column.Utf8(vs, _), ord, ok) =>
      val Column.Utf8(wantVs, _) = want.dictionary: @unchecked
      assertEquals(vs.toVector, wantVs.toVector, "the values, their order, the unused one")
      assertEquals(ord, want.ordered)
      assertEquals(ok.toVector, want.valid.toVector)
      assertEquals(idx.indices.filter(ok).map(idx).toVector, want.indices.indices.filter(want.valid).map(want.indices).toVector)
    case other => fail(s"not kept: ${Column.describe(other)}")

  test("a dictionary column round-trips through the stream and the file, kept: indices, values in order, ordered, nulls") {
    for bytes <- Vector(OkayArrow.write(table)) do
      val back = OkayArrow.readKeeping(bytes)
      assertKept(back.cols(1)._2, employment)
      assertKept(back.cols(2)._2, table.cols(2)._2.asInstanceOf[Column.Dictionary])
      assertEquals(back.cols(0)._2.asInstanceOf[Column.Int64].values.toVector, Vector(1L, 2, 3, 4, 5))
    // the FILE lists the dictionary block in its footer: read (decoded), every row its value
    val file = OkayArrow.readFile(OkayArrow.writeFile(table))
    (file.cols(1)._2, employment.decoded) match
      case (Column.Utf8(got, gok), Column.Utf8(want, wok)) =>
        assertEquals(got.toVector.zip(gok).map((v, o) => Option.when(o)(v)), want.toVector.zip(wok).map((v, o) => Option.when(o)(v)))
      case (g, _) => fail(Column.describe(g))
  }

  test("read answers the decoded column, as before stage 8") {
    OkayArrow.read(OkayArrow.write(table)).cols(1)._2 match
      case Column.Utf8(vs, ok) =>
        assertEquals(vs.toVector.zip(ok).map((v, o) => Option.when(o)(v)),
          Vector(Some("employed"), Some("self-employed"), None, Some("unemployed"), Some("employed")))
      case other => fail(Column.describe(other))
    assertEquals(employment.decoded.length, 5)
  }

  test("two parts with one dictionary concatenate their indices; a replacement one shifts the later indices") {
    val a = Column.Dictionary(Array(0, 1), levels("x", "y"), false, Array(true, true))
    val same = Column.Dictionary(Array(1), levels("x", "y"), false, Array(true))
    Column.concat(Vector(a, same)) match
      case Column.Dictionary(idx, d, _, _) => assertEquals((idx.toVector, d.length), (Vector(0, 1, 1), 2))
      case other => fail(Column.describe(other))
    val other = Column.Dictionary(Array(0), levels("z"), false, Array(true))
    val joined = Column.concat(Vector(a, other))
    joined.decoded match
      case Column.Utf8(vs, _) => assertEquals(vs.toVector, Vector("x", "y", "z"), "every row reads its own value")
      case c => fail(Column.describe(c))
  }

  test("a dictionary nested in a struct is refused on write, by name; an index outside its dictionary too") {
    val nested = Table(Vector("s" -> Column.Struct(Vector("f" -> employment), Array.fill(5)(true))), Vector.empty)
    val e = intercept[IllegalArgumentException](OkayArrow.write(nested))
    assert(e.getMessage.contains("nested"), e.getMessage)
    val bad = Table(Vector("d" -> Column.Dictionary(Array(3), levels("a"), false, Array(true))), Vector.empty)
    assert(intercept[IllegalArgumentException](OkayArrow.write(bad)).getMessage.contains("outside a dictionary"))
  }
