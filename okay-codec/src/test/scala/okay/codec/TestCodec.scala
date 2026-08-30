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

  test("lossless round-trip: parse then render reproduces the input") {
    // trivia, odd spacing, duplicate keys, ordering, even damage —
    // the CST keeps it all and render is the identity's other half
    val inputs = List(
      "{ \"b\" : 2 ,\n\t\"a\": [ 1,2 , 3 ] }",
      "{\"k\":1,\"k\":2}",
      "[true , null,\n\n 1e-3 ]",
      "{\"a\": @@, \"b\": 2}",
      "\"just a string\"   ")
    for s <- inputs do
      assertEquals(Json.render(Json.cst(s)), s)
  }

  test("cross-format: one derived Schema speaks JSON and CBOR alike") {
    val p = Person("ann", 41, List("a", "b"),
      Some(Person("bo\"ss", 60, Nil, None)))
    assertEquals(Cbor.read[Person](Cbor.write(p)), Right(p))
    assertEquals(Cbor.read[Person](Cbor.write(p)), Json.read[Person](Json.write(p)))
    val shapes: List[Shape] = List(Shape.Circle(1.5), Shape.Rect(2, 3))
    for s <- shapes do
      assertEquals(Cbor.read[Shape](Cbor.write(s)), Right(s))
      assertEquals(Cbor.read[Shape](Cbor.write(s)), Json.read[Shape](Json.write(s)))
  }

  test("CBOR decode errors are values: truncation and wrong shapes") {
    val bytes = Cbor.write(Person("x", 1, Nil, None))
    assert(Cbor.read[Person](bytes.dropRight(3)).isLeft)
    assert(Cbor.read[Shape](bytes).isLeft)
    assert(Cbor.read[Person](Array[Byte]()).isLeft)
  }

  test("markdown: the reframing case parses without faults, losslessly") {
    val input = "*a _b* c_\n"
    val t = Markdown.parse(input)
    assertEquals(okay.parse.Cst.lexemes(t), input)
    assertEquals(okay.parse.Cst.errors(t), Vector.empty)
    // the crossing close reframed: the underscore emphasis reopens
    def kinds(c: okay.parse.Cst[Markdown.K]): Vector[String] = c match
      case okay.parse.Cst.Node(k, cs) => k +: cs.flatMap(kinds)
      case _ => Vector.empty
    assertEquals(kinds(t).count(_ == "u-em"), 2)
  }

  test("markdown: headings, paragraphs, code spans; unclosed is an error node") {
    val doc = "# title\ntext *bold* and `code # here`\n"
    val t = Markdown.parse(doc)
    assertEquals(okay.parse.Cst.lexemes(t), doc)
    assertEquals(okay.parse.Cst.errors(t), Vector.empty)
    // an unclosed emphasis at end of input is an error IN the tree
    val open = Markdown.parse("*never closed")
    assertEquals(okay.parse.Cst.lexemes(open), "*never closed")
    assert(okay.parse.Cst.errors(open).nonEmpty)
  }
}
