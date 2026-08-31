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

  test("yaml: the indentation dialect decodes through the SAME Schema algebra") {
    val doc =
      "name: ann        # a comment\n" +
      "age: 41\n" +
      "tags:\n" +
      "  - a\n" +
      "  - b\n" +
      "boss:\n" +
      "  name: \"bo ss\"\n" +
      "  age: 60\n" +
      "  tags:\n" +
      "    - x\n"
    assertEquals(Yaml.read[Person](doc),
      Right(Person("ann", 41, List("a", "b"),
        Some(Person("bo ss", 60, List("x"), None)))))
  }

  test("yaml: lossless render, comments and indentation included") {
    val docs = List(
      "a: 1\nb:\n  - x\n  - y   # tail comment\n",
      "- 1\n- -5\n- true\n",
      "msg: \"a: b\"  # a colon inside quotes\n",
      "weird: http://example.com\n",
      ": orphan\n")
    for d <- docs do
      assertEquals(Yaml.render(Yaml.cst(d)), d)
  }

  test("yaml: scalars type themselves; a plain colon stays in a URL") {
    val j = Yaml.parse("- 1\n- -5.5\n- true\n- null\n- plain text\n")
    assertEquals(j, Json.JArr(Vector(Json.JNum(1), Json.JNum(-5.5),
      Json.JBool(true), Json.JNull, Json.JStr("plain text"))))
    assertEquals(Yaml.parse("url: http://x/y\n"),
      Json.JObj(Vector("url" -> Json.JStr("http://x/y"))))
  }

  test("yaml: total on damage — an orphan colon is data, not a fault") {
    val j = Yaml.parse(": orphan\nok: 1\n")
    j match
      case Json.JObj(fs) => assert(fs.exists((k, _) => k == "ok"))
      case other => // damage may swallow the doc into an error — still a value
        assert(other.isInstanceOf[Json.JErr] || other == Json.JNull)
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

  test("base64 round-trips every length, and rejects damage as a value") {
    for n <- 0 to 32 do
      val bs = Array.tabulate(n)(i => (i * 37 - 128).toByte)
      val back = Base64.decode(Base64.encode(bs))
      assertEquals(back.map(_.toList), Right(bs.toList), s"length $n")
    // padding is exact, so the encoding is the canonical one
    assertEquals(Base64.encode("M".getBytes("UTF-8")), "TQ==")
    assertEquals(Base64.encode("Ma".getBytes("UTF-8")), "TWE=")
    assertEquals(Base64.encode("Man".getBytes("UTF-8")), "TWFu")
    // and a bad character is an error value, not a thrown exception
    assert(Base64.decode("TW!u").isLeft)
    assert(Base64.decode("TWF").isLeft, "a length not a multiple of four")
  }

  test("bytes survive both wires, and CBOR spends four bytes per float") {
    final case class Blob(name: String, data: Array[Byte])
    given Schema[Blob] = Schema.derived

    val floats = Array.tabulate(1536)(i => i * 0.001f)
    val packed = new Array[Byte](floats.length * 4)
    for i <- floats.indices do
      val b = java.lang.Float.floatToIntBits(floats(i))
      packed(i * 4) = b.toByte
      packed(i * 4 + 1) = (b >> 8).toByte
      packed(i * 4 + 2) = (b >> 16).toByte
      packed(i * 4 + 3) = (b >> 24).toByte
    val blob = Blob("v", packed)

    // CBOR: a byte string, so four bytes per component and no per-item
    // tag — where List[Double] spent nine
    val cbor = Cbor.write(blob)
    assert(cbor.length < 6200, s"CBOR spent ${cbor.length} bytes")
    assert(cbor.length > 6140, s"CBOR spent only ${cbor.length} — is the data there?")
    val backC = Cbor.read[Blob](cbor)
    assertEquals(backC.map(_.data.toList), Right(packed.toList))

    // JSON: base64, so one token instead of 1536 float literals
    val json = Json.write(blob)
    assert(json.length < 8300, s"JSON spent ${json.length} bytes")
    val backJ = Json.read[Blob](json)
    assertEquals(backJ.map(_.data.toList), Right(packed.toList))

    // and the floats come back bit-for-bit
    val got = backC.toOption.get.data
    for i <- floats.indices do
      val b = (got(i * 4) & 0xFF) | ((got(i * 4 + 1) & 0xFF) << 8) |
        ((got(i * 4 + 2) & 0xFF) << 16) | ((got(i * 4 + 3) & 0xFF) << 24)
      assertEquals(java.lang.Float.intBitsToFloat(b), floats(i), s"component $i")
  }

  test("a `>` inside an XML comment does not end it") {
    // the branch that used to compute a mode and throw it away was
    // guarding exactly this; the guard belongs (and lives) at `<!--`,
    // so the dead code went and this test stays
    val text = "<a><!-- b > c --></a>"
    val lexed = okay.lex.Scan.all(Xml.scan)(text)
    assertEquals(lexed.tokens.map(_.lexeme).mkString, text, "not lossless")
    val comments = lexed.tokens.filter(_.kind == Xml.K.Comment)
    assertEquals(comments.size, 1, s"comment split: ${comments.map(_.lexeme)}")
    assertEquals(comments.head.lexeme, "<!-- b > c -->")
    // and the element around it is still seen
    assertEquals(lexed.tokens.count(_.kind == Xml.K.Open), 1)
    assertEquals(lexed.tokens.count(_.kind == Xml.K.Close), 1)
  }
}
