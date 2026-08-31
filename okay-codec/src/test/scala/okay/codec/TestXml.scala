package okay.codec

import okay.parse.Cst

/** The nesting prover: named tags, where a close can be wrong. */
class TestXml extends munit.FunSuite {

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

  test("an incremental reparse of markup equals a full one") {
    val session = Xml.parse(doc, 16)
    val at = doc.indexOf("world")
    val edited = doc.patch(at, "WORLD", 5)
    val re = Xml.reparse(session, doc, edited, at, at + 5, at + 5, 16)
    assertEquals(Xml.render(re.tree), edited)
    assertEquals(re.tree, Xml.parse(edited, 16).tree)
  }
}
