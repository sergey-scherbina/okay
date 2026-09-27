package okay.foreign

import PyValue.*
import okay.codec.{Json, Schema}

final case class Row(key: Int, v: Long) derives Schema

/** pyvalue-table: a frame as a VALUE — where it crosses, what refuses it
 * (default gate, no interpreter) */
class TestPyValueTable extends munit.FunSuite:

  private val frame = PyFrame(Vector(
    "key" -> Vector(I64(1), I64(2), I64(3)),
    "v" -> Vector(I64(10), F64(2.5), I64(30)),
    "s" -> Vector(Str("a"), Str("чай"), Str("c"))))

  test("a frame inside a dict crosses the wire and comes back a Table, cells intact") {
    val v = Dict(Vector("rows" -> Table(frame), "state" -> Dict(Vector("n" -> I64(3)))))
    val back = Wire.dec(Json.parse(Json.print(Wire.enc(v))))
    assertEquals(back, v)
  }

  test("a null cell in a typed column comes back as a typed absence, as a frame op's answer always did") {
    val withNull = PyFrame(Vector("key" -> Vector(I64(1), PyNone)))
    Wire.dec(Json.parse(Json.print(Wire.enc(Dict(Vector("rows" -> Table(withNull))))))) match
      case Dict(Vector(("rows", Table(f)))) => f.cols.head._2 match
        case Vector(I64(1), NA(_)) => ()
        case other => fail(s"a null read back as $other")
      case other => fail(s"not the dict: $other")
  }

  test("a Table among a list's elements, and alone, round-trips") {
    assertEquals(Wire.dec(Json.parse(Json.print(Wire.enc(Arr(Vector(Table(frame), I64(1))))))), Arr(Vector(Table(frame), I64(1))))
    assertEquals(Wire.dec(Json.parse(Json.print(Wire.enc(Table(frame))))), Table(frame))
  }

  test("a frame's column may not hold a frame: refused by name on the way out and on the way in") {
    val nested = PyFrame(Vector("x" -> Vector(Table(frame))))
    val e = intercept[IllegalArgumentException](Wire.enc(Table(nested)))
    assert(e.getMessage.contains("may not hold a frame"), e.getMessage)
    val text = """{"t":"frame","cols":[["x",[{"t":"frame","cols":[["y",[1]]]}]]]}"""
    val d = intercept[IllegalStateException](Wire.dec(Json.parse(text)))
    assert(d.getMessage.contains("may not hold a frame"), d.getMessage)
  }

  test("...at any depth: a frame inside a dict inside a column is refused too, before it is decoded — that is what bounds the stack") {
    val deep = PyFrame(Vector("x" -> Vector(Dict(Vector("inner" -> Arr(Vector(Table(frame))))))))
    val e = intercept[IllegalArgumentException](Wire.enc(Table(deep)))
    assert(e.getMessage.contains("at any depth"), e.getMessage)
    val text = """{"t":"frame","cols":[["x",[{"inner":[{"t":"frame","cols":[["y",[1]]]}]}]]]}"""
    val d = intercept[IllegalStateException](Wire.dec(Json.parse(text)))
    assert(d.getMessage.contains("at any depth"), d.getMessage)
    // and the scans themselves are not the recursion they prevent: 100 000 levels of nesting, no overflow
    val nested = (1 to 100000).foldLeft(Table(frame): PyValue)((v, _) => Arr(Vector(v)))
    assert(Wire.holdsFrame(nested))
    assert(!Wire.holdsFrame(Arr(Vector(I64(1), Dict(Vector("a" -> Str("b")))))))
  }

  test("rows expected, a frame answered: PyCodec reads the rows by the row's Schema") {
    val rows = Vector(Row(1, 10), Row(2, 20))
    val t = Table(PyFrame(Vector("key" -> Vector(I64(1), I64(2)), "v" -> Vector(I64(10), I64(20)))))
    assertEquals(PyCodec.decode[Vector[Row]](t), Right(rows))
    assertEquals(PyCodec.decode[List[Row]](t), Right(rows.toList))
    final case class Stepped(rows: Vector[Row], state: Long) derives Schema
    assertEquals(PyCodec.decode[Stepped](Dict(Vector("rows" -> t, "state" -> I64(7)))), Right(Stepped(rows, 7L)))
  }

  test("on the JVM a frame is a map of columns, each a list") {
    val j = Jvm.jvm(Table(PyFrame(Vector("a" -> Vector(I64(1), I64(2))))))
    val m = j.asInstanceOf[java.util.Map[String, java.util.List[Any]]]
    assertEquals(m.get("a").size, 2)
    assertEquals(m.get("a").get(1), java.lang.Long.valueOf(2))
  }
