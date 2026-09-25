package okay2.codec

// the sum's children are declared before any derivation reads them
// (a sealed type's children are known once typed: SI-7046)
sealed trait Shape
object Shape {
  final case class Circle(r: Double) extends Shape
  final case class Rect(w: Double, h: Double) extends Shape
  case object Dot extends Shape
}

final case class Person(name: String, age: Int, tags: List[String], boss: Option[Person])
object Person {
  // a recursive type is declared by name, so the field's lookup finds it
  implicit lazy val schema: Schema[Person] = Schema.derived
}

/** One Schema, folded by two algebras: encode and total decode
 * (okay-codec's TestCodec, its JSON half). */
class TestCodec extends munit.FunSuite {

  test("round-trip: a nested, recursive product") {
    val p = Person("ann", 41, List("a", "b"), Some(Person("bo\"ss", 60, Nil, None)))
    val text = Json.write(p)
    assertEquals(Json.read[Person](text), Right(p))
  }

  test("round-trip: sums by case name, a case object included, derived without a declaration") {
    val shapes: List[Shape] = List(Shape.Circle(1.5), Shape.Rect(2, 3), Shape.Dot)
    for (s <- shapes) assertEquals(Json.read[Shape](Json.write(s)), Right(s))
    assertEquals(Json.write[Shape](Shape.Circle(1.5)), """{"Circle":{"r":1.5}}""")
    assertEquals(Json.write[Shape](Shape.Dot), """{"Dot":{}}""")
  }

  test("decode errors are values: missing field, wrong shape, damage") {
    assertEquals(Json.read[Person]("""{"name":"x","age":1,"tags":[]}"""), Right(Person("x", 1, Nil, None)))
    assert(Json.read[Person]("""{"name":"x","tags":[]}""").left.exists(_.contains("age")))
    // truncated input still PARSES (totality), and the fields that are
    // there project
    assertEquals(Json.read[Person]("""{"name":"x","age":1,"tags":[],"boss":null"""), Right(Person("x", 1, Nil, None)))
    assert(Json.read[Int]("true").left.exists(_.startsWith("expected")))
    assertEquals(Json.read[Shape]("""{"Hexagon":{}}"""), Left("unknown case 'Hexagon' of Shape"))
  }

  test("whitespace, escapes and structure survive the pipeline") {
    val text = "{\n  \"name\": \"a\\nb\",\n  \"age\": 7,\n  \"tags\": [\"x\"],\n  \"boss\": null\n}"
    assertEquals(Json.read[Person](text), Right(Person("a\nb", 7, List("x"), None)))
  }

  test("the projection keeps damage as data") {
    Json.parse("""{"a": @@, "b": 2}""") match {
      case Json.JObj(fs) => assert(fs.exists(_._2.isInstanceOf[Json.JErr]) || fs.exists(_._1.startsWith("<")))
      case other => fail(s"expected an object, got $other")
    }
  }

  test("lossless round-trip: parse then render reproduces the input") {
    val inputs = List(
      "{ \"b\" : 2 ,\n\t\"a\": [ 1,2 , 3 ] }",
      "{\"k\":1,\"k\":2}",
      "[true , null,\n\n 1e-3 ]",
      "{\"a\": @@, \"b\": 2}",
      "\"just a string\"   ")
    for (s <- inputs) assertEquals(Json.render(Json.cst(s)), s)
  }

  test("base64 round-trips every length, and rejects damage as a value") {
    for (n <- 0 to 32) {
      val bs = Array.tabulate(n)(i => (i * 37 - 128).toByte)
      assertEquals(Base64.decode(Base64.encode(bs)).map(_.toList), Right(bs.toList), s"length $n")
    }
    assertEquals(Base64.encode("M".getBytes("UTF-8")), "TQ==")
    assertEquals(Base64.encode("Ma".getBytes("UTF-8")), "TWE=")
    assertEquals(Base64.encode("Man".getBytes("UTF-8")), "TWFu")
    assert(Base64.decode("TW!u").isLeft)
    assert(Base64.decode("TWF").isLeft, "a length not a multiple of four")
  }

  test("bytes travel as base64: one token, bit for bit") {
    final case class Blob(name: String, data: Array[Byte])
    implicit val blob: Schema[Blob] = Schema.derived
    val packed = Array.tabulate(1024)(i => (i * 31).toByte)
    val json = Json.write(Blob("v", packed))
    assert(json.length < 1400, s"JSON spent ${json.length} bytes")
    assertEquals(Json.read[Blob](json).map(_.data.toList), Right(packed.toList))
  }

  test("Option, List and Vector are the containers, never derived sums") {
    assert(implicitly[Schema[Option[Int]]].isInstanceOf[Schema.SOption[_]])
    assert(implicitly[Schema[List[Int]]].isInstanceOf[Schema.SList[_]])
    assert(implicitly[Schema[Vector[Int]]].isInstanceOf[Schema.SVector[_]])
  }
}
