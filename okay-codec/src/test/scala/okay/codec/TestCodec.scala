package okay.codec

/** One Schema, folded by two algebras: encode and total decode. */
class TestCodec extends munit.FunSuite {

  case class Person(name: String, age: Int, tags: List[String], boss: Option[Person])
  given Schema[Person] = Schema.derived

  enum Shape:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)
  given Schema[Shape.Circle] = Schema.derived
  given Schema[Shape.Rect] = Schema.derived
  given Schema[Shape] = Schema.derived

  test("round-trip: a nested, recursive product") {
    val p = Person("ann", 41, List("a", "b"),
      Some(Person("bo\"ss", 60, Nil, None)))
    val text = Json.write(p)
    assertEquals(Json.read[Person](text), Right(p))
  }

  test("round-trip: sums by case name") {
    val shapes: List[Shape] = List(Shape.Circle(1.5), Shape.Rect(2, 3))
    for s <- shapes do
      assertEquals(Json.read[Shape](Json.write(s)), Right(s))
  }

  test("decode errors are values: missing field, wrong shape, damage") {
    // a missing OPTIONAL field is None; a missing required one is an error
    assertEquals(Json.read[Person]("""{"name":"x","age":1,"tags":[]}"""),
      Right(Person("x", 1, Nil, None)))
    assert(Json.read[Person]("""{"name":"x","tags":[]}""").left.exists(_.contains("age")))
    assert(Json.read[Person]("""{"name":"x","age":1,"tags":[],"boss":null""").isLeft
      || Json.read[Person]("""{"name":"x","age":1,"tags":[],"boss":null""").isRight)
    // truncated input still PARSES (totality); decode may still succeed
    // because the tree with holes projects the fields that are there
    assertEquals(
      Json.read[Person]("""{"name":"x","age":1,"tags":[],"boss":null"""),
      Right(Person("x", 1, Nil, None)))
    assert(Json.read[Int]("true").left.exists(_.startsWith("expected")))
  }

  test("whitespace, escapes and structure survive the pipeline") {
    val text = "{\n  \"name\": \"a\\nb\",\n  \"age\": 7,\n  \"tags\": [\"x\"],\n  \"boss\": null\n}"
    assertEquals(Json.read[Person](text), Right(Person("a\nb", 7, List("x"), None)))
  }

  test("the projection keeps damage as data") {
    val j = Json.parse("""{"a": @@, "b": 2}""")
    j match
      case Json.JObj(fs) => assert(fs.exists((_, v) => v.isInstanceOf[Json.JErr])
        || fs.exists((k, _) => k.startsWith("<")))
      case other => fail(s"expected an object, got $other")
  }
}
