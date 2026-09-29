package okay.codec

import okay.parse.Cst

/** The nesting prover: named tags, where a close can be wrong. */
class TestXml extends munit.FunSuite {

  def scanned(s: String): Vector[Xml.T] = okay.lex.Scan.all(Xml.scan)(s).tokens
  def streamed(s: String): Vector[Xml.T] =
    val out = Vector.newBuilder[Xml.T]
    Xml.tokens(java.io.StringReader(s))(out += _)
    out.result()

  val doc =
    """<html>
      |  <!-- a note -->
      |  <body class="main">
      |    <p>Hello <b>world</b></p>
      |    <br>
      |    <img src="x.png"/>
      |  </body>
      |</html>
      |""".stripMargin

  test("lossless: the tree reproduces the document exactly") {
    assertEquals(Xml.render(Xml.cst(doc)), doc)
  }

  test("nesting by name: elements come out as nodes") {
    val tree = Xml.cst(doc)
    assertEquals(Xml.elements(tree, "html").length, 1)
    assertEquals(Xml.elements(tree, "p").length, 1)
    assertEquals(Xml.elements(tree, "b").length, 1)
    assertEquals(Xml.text(Xml.elements(tree, "p").head).trim, "Hello world")
  }

  test("void elements never open a frame") {
    val tree = Xml.cst(doc)
    // br and img are nodes, but they contain nothing and did not
    // swallow their siblings
    assertEquals(Xml.elements(tree, "br").length, 1)
    assertEquals(Xml.text(Xml.elements(tree, "br").head), "")
    assertEquals(Xml.elements(tree, "img").length, 1)
    assertEquals(Xml.elements(tree, "body").length, 1)
  }

  test("a mismatched close closes the unclosed ones, and says so") {
    val bad = "<a><b>text</a>"
    val tree = Xml.cst(bad)
    assertEquals(Xml.render(tree), bad)
    val errs = Cst.errors(tree).map(_._2)
    assert(errs.exists(_.contains("<b> was never closed")), errs.toString)
    // and </a> still closed a, so the text is inside it
    assertEquals(Xml.text(Xml.elements(tree, "a").head), "text")
  }

  test("a close with nothing open is an error leaf, not a fault") {
    val tree = Xml.cst("</p>hello")
    assertEquals(Xml.render(tree), "</p>hello")
    assert(Cst.errors(tree).exists(_._2.contains("closes nothing")))
  }

  test("an unterminated tag at end of input is still a token") {
    for s <- Seq("<a", "<a href=\"x", "<!-- unterminated", "<![CDATA[ x") do
      assertEquals(Xml.render(Xml.cst(s)), s, s)
  }

  test("comments and CDATA are kept and do not nest as elements") {
    val s = "<a><!-- <b> --><![CDATA[ </c> ]]></a>"
    val tree = Xml.cst(s)
    assertEquals(Xml.render(tree), s)
    assertEquals(Xml.elements(tree, "b").length, 0, "a tag inside a comment opened")
    assertEquals(Xml.elements(tree, "c").length, 0, "a tag inside CDATA closed")
  }

  test("the XML declaration and a processing instruction are one token each, never a frame") {
    // before xml-processing-instruction (2026-09-29) `<?xml …?>` was an
    // Open nobody closed, so every real document ended in `unclosed`
    val s = """<?xml version="1.0" encoding="UTF-8"?><a><?php if (1 > 0) echo "x"; ?><b/></a>"""
    val tree = Xml.cst(s)
    assertEquals(Xml.render(tree), s)
    assertEquals(Cst.errors(tree), Vector.empty)
    val kinds = scanned(s).map(_.kind)
    assertEquals(kinds.count(_ == Xml.K.Pi), 2)
    assertEquals(Xml.elements(tree, "a").length, 1)
    assertEquals(Xml.elements(tree, "b").length, 1)
    // the streaming tokenizer agrees with the scanner, token for token
    assertEquals(scanned(s), streamed(s))
  }

  test("a DOCTYPE is one token, no frame") {
    val s = "<!DOCTYPE html><html><body/></html>"
    val tree = Xml.cst(s)
    assertEquals(Xml.render(tree), s)
    assertEquals(Cst.errors(tree), Vector.empty)
    assertEquals(scanned(s).map(_.kind).head, Xml.K.Decl)
    assertEquals(scanned(s), streamed(s))
  }

  test("an unterminated processing instruction at end of input is still a token") {
    for s <- Seq("<?xml version=\"1.0\"", "<?xml ?", "<!DOCTYPE html") do
      assertEquals(Xml.render(Xml.cst(s)), s, s)
  }

  test("value: elements as objects, attributes as @name, repeats as arrays, text as strings, case kept") {
    val s = """<?xml version="1.0"?><Doc v="5-10"><!-- c --><a>1</a><a>2</a><b k='x'>t</b><c><d/>tail</c><e/></Doc>"""
    assertEquals(Json.print(Xml.value(Xml.cst(s))),
      """{"Doc":{"@v":"5-10","a":["1","2"],"b":{"@k":"x","#text":"t"},"c":{"d":"","#text":"tail"},"e":""}}""")
    assertEquals(Xml.attributes("""<x a="1" b='2' c d = "e f"/>"""), Vector("a" -> "1", "b" -> "2", "c" -> "", "d" -> "e f"))
    assertEquals(Xml.value(Xml.cst("just text")), Json.JStr("just text"))
  }

  test("an incremental reparse of markup equals a full one") {
    val session = Xml.parse(doc, 16)
    val at = doc.indexOf("world")
    val edited = doc.patch(at, "WORLD", 5)
    val re = Xml.reparse(session, doc, edited, at, at + 5, at + 5, 16)
    assertEquals(Xml.render(re.tree), edited)
    assertEquals(re.tree, Xml.parse(edited, 16).tree)
  }

  // xml-projection-stack-safe: the parse built this depth without
  // trouble; the projections recursed per level and overflowed
  test("depth: the projections walk a 20 000-deep document the parser builds") {
    val n = 20000
    val s = ("<a>" * n) + "x" + ("</a>" * n)
    val tree = Xml.cst(s)
    assertEquals(Xml.render(tree), s)
    assertEquals(Xml.text(tree), "x")
    assertEquals(Xml.elements(tree, "a").length, n)
  }
}
