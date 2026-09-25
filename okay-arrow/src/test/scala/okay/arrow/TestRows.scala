package okay.arrow

import okay.codec.Schema

object RowsModel:
  enum Colour derives Schema:
    case Red, Green

  enum Shape derives Schema:
    case Circle(r: Double)
    case Square(side: Double, label: Option[String])
    case Empty

  final case class Line(product: String, n: Int) derives Schema

  final case class Order(id: Long, sku: String, qty: Int, price: Double, paid: Boolean,
                         note: Option[String], tags: List[String], lines: Vector[Line],
                         colour: Colour, shape: Shape, blob: Array[Byte], total: BigInt,
                         initial: Char) derives Schema

  final case class Tree(label: String, kids: Vector[Tree])
  given Schema[Tree] = Schema.derived
  final case class Forest(name: String, tree: Tree) derives Schema

  val orders: Vector[Order] = Vector(
    Order(1L, "tea", 3, 2.5, true, Some("gift"), List("hot", "green"), Vector(Line("cup", 2)),
      Colour.Red, Shape.Circle(1.5), Array[Byte](1, 2), BigInt("123456789012345678901234567890"), 'k'),
    Order(Long.MaxValue, "чай ☕", -1, -0.0, false, None, Nil, Vector.empty,
      Colour.Green, Shape.Square(2.0, None), Array.emptyByteArray, BigInt(-7), 'ü'),
    Order(0L, "", 0, 1e300, true, Some(""), List(""), Vector(Line("", 0), Line("x", Int.MinValue)),
      Colour.Red, Shape.Empty, Array[Byte](-1), BigInt(0), ' '))

/** stage 5: typed rows through okay-codec's Schema, on every platform */
class TestRows extends munit.FunSuite:
  import RowsModel.*

  /** Order holds an Array[Byte], which `==` compares by reference */
  private def same(a: Vector[Order], b: Vector[Order]): Unit =
    assertEquals(b.map(_.copy(blob = Array.emptyByteArray)), a.map(_.copy(blob = Array.emptyByteArray)))
    assertEquals(b.map(_.blob.toVector), a.map(_.blob.toVector))

  test("rows of a case class round-trip: nested products, lists, options, enums, sums, bytes, big ints, chars") {
    val back = OkayArrow.decode[Order](OkayArrow.encode(orders))
    same(orders, back.fold(e => fail(e), identity))
  }

  test("a product's fields are the table's columns, with Columns' types") {
    val t = Rows.table(orders)
    assertEquals(t.cols.map((n, c) => n -> Column.describe(c)), Vector(
      "id" -> "Int64", "sku" -> "Utf8", "qty" -> "int32", "price" -> "Float64", "paid" -> "Bool",
      "note" -> "Utf8", "tags" -> "list<Utf8>", "lines" -> "list<struct<product: Utf8, n: int32>>",
      "colour" -> "Utf8",
      "shape" -> "struct<kind: Utf8, Circle: struct<r: Float64>, Square: struct<side: Float64, label: Utf8>>",
      "blob" -> "Binary", "total" -> "decimal(38, 0)", "initial" -> "Utf8"))
  }

  test("a recursive type crosses as its CBOR, beside its JSON") {
    val forest = Vector(Forest("oak", Tree("root", Vector(Tree("a", Vector.empty), Tree("b", Vector(Tree("c", Vector.empty)))))))
    assertEquals(OkayArrow.decode[Forest](OkayArrow.encode(forest)), Right(forest))
  }

  test("a value that is not a product is one 'value' column") {
    assertEquals(OkayArrow.decode[Colour](OkayArrow.encode(Vector(Colour.Green, Colour.Red))), Right(Vector(Colour.Green, Colour.Red)))
    assertEquals(OkayArrow.decode[Int](OkayArrow.encode(Vector(1, 2, 3))), Right(Vector(1, 2, 3)))
  }

  test("a table that does not fit the schema is a Left naming the row and the column") {
    val t = Rows.table(orders)
    val nulled = t.cols.map {
      case ("sku", Column.Utf8(v, ok)) => "sku" -> Column.Utf8(v, ok.updated(1, false))
      case other => other
    }
    assertEquals(Rows.rows[Order](Table(nulled, Vector.empty)),
      Left("row 1: column 'sku': null where the schema has no Option around a String"))
    val renamed = t.cols.map {
      case ("colour", Column.Utf8(v, ok)) => "colour" -> Column.Utf8(v.updated(0, "Blue"), ok)
      case other => other
    }
    assert(Rows.rows[Order](Table(renamed, Vector.empty)).left.exists(_.contains("'Blue' is not a case of Colour")))
    val wide = t.cols.map {
      case ("qty", _) => "qty" -> Column.Int64(Array(1L, Long.MaxValue, 0L), Array(true, true, true))
      case other => other
    }
    assert(Rows.rows[Order](Table(wide, Vector.empty)).left.exists(_.contains("9223372036854775807 is past an Int")))
  }

  test("an empty sequence of rows is a table with its columns and no rows") {
    val t = OkayArrow.read(OkayArrow.encode(Vector.empty[Order]))
    assertEquals((t.rows, t.cols.length), (0, 13))
    assertEquals(OkayArrow.decode[Order](OkayArrow.encode(Vector.empty[Order])), Right(Vector.empty))
  }

  test("a table of no rows reads as no rows, even when its columns lost their kind (Nulls(0))") {
    // the JSON frame road cannot type an empty column and answers Nulls(0)
    val empty = Table(Vector("key" -> Column.Nulls(0), "v" -> Column.Nulls(0)), Vector.empty)
    assertEquals(Rows.rows[Line](empty), Right(Vector.empty))
  }
