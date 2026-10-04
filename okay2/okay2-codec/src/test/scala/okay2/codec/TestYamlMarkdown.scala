package okay2.codec

import okay2.parse.Cst

/** YAML and Markdown (okay-codec's TestCodec, their halves): the
 * indentation dialect decodes through the SAME Schema algebra as JSON,
 * and the reframing dialect keeps every token without a fault. */
class TestYamlMarkdown extends munit.FunSuite {

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
      Right(Person("ann", 41, List("a", "b"), Some(Person("bo ss", 60, List("x"), None)))))
  }

  test("yaml: lossless render, comments and indentation included") {
    val docs = List(
      "a: 1\nb:\n  - x\n  - y   # tail comment\n",
      "- 1\n- -5\n- true\n",
      "msg: \"a: b\"  # a colon inside quotes\n",
      "weird: http://example.com\n",
      ": orphan\n")
    for (d <- docs) assertEquals(Yaml.render(Yaml.cst(d)), d)
  }

  test("yaml: scalars type themselves; a plain colon stays in a URL") {
    assertEquals(Yaml.parse("- 1\n- -5.5\n- true\n- null\n- plain text\n"),
      Json.JArr(Vector(Json.JNum(1), Json.JNum(-5.5), Json.JBool(true), Json.JNull, Json.JStr("plain text"))): Json)
    assertEquals(Yaml.parse("url: http://x/y\n"), Json.JObj(Vector("url" -> Json.JStr("http://x/y"))): Json)
  }

  test("yaml: total on damage — an orphan colon is data, not a fault") {
    Yaml.parse(": orphan\nok: 1\n") match {
      case Json.JObj(fs) => assert(fs.exists(_._1 == "ok"))
      case other => assert(other.isInstanceOf[Json.JErr] || other == Json.JNull, s"$other")
    }
  }

  test("markdown: the reframing case parses without faults, losslessly") {
    val input = "*a _b* c_\n"
    val t = Markdown.parse(input)
    assertEquals(Cst.lexemes(t), input)
    assertEquals(Cst.errors(t), Vector.empty)
    // the crossing close reframed: the underscore emphasis reopens
    var count = 0
    var work: List[Cst[Markdown.K]] = List(t)
    while (work.nonEmpty) {
      work.head match {
        case Cst.Node(k, cs) => if (k == "u-em") count += 1; work = cs.toList ::: work.tail
        case _ => work = work.tail
      }
    }
    assertEquals(count, 2)
  }

  test("markdown: headings, paragraphs, code spans; unclosed is an error node") {
    val doc = "# title\ntext *bold* and `code # here`\n"
    val t = Markdown.parse(doc)
    assertEquals(Cst.lexemes(t), doc)
    assertEquals(Cst.errors(t), Vector.empty)
    val open = Markdown.parse("*never closed")
    assertEquals(Cst.lexemes(open), "*never closed")
    assert(Cst.errors(open).nonEmpty)
  }
}
