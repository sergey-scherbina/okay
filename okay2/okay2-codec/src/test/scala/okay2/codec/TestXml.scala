package okay2.codec

import okay2.parse.Cst

/** The nesting prover: named tags, where a close can be wrong
 * (okay-codec's TestXml, and the depth its projections owe) */
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
    assertEquals(Xml.text(Xml.elements(tree, "a").head), "text")
  }

  test("a close with nothing open is an error leaf, not a fault") {
    val tree = Xml.cst("</p>hello")
    assertEquals(Xml.render(tree), "</p>hello")
    assert(Cst.errors(tree).exists(_._2.contains("closes nothing")))
  }

  test("an unterminated tag at end of input is still a token") {
    for (s <- Seq("<a", "<a href=\"x", "<!-- unterminated", "<![CDATA[ x"))
      assertEquals(Xml.render(Xml.cst(s)), s, s)
  }

  test("comments and CDATA are kept and do not nest as elements") {
    val s = "<a><!-- <b> --><![CDATA[ </c> ]]></a>"
    val tree = Xml.cst(s)
    assertEquals(Xml.render(tree), s)
    assertEquals(Xml.elements(tree, "b").length, 0, "a tag inside a comment opened")
    assertEquals(Xml.elements(tree, "c").length, 0, "a tag inside CDATA closed")
  }

  test("the XML declaration, a processing instruction and a DOCTYPE are one token each, never a frame") {
    val s = """<?xml version="1.0"?><!DOCTYPE a><a><?php if (1 > 0) echo "x"; ?><b/></a>"""
    val tree = Xml.cst(s)
    assertEquals(Xml.render(tree), s)
    assertEquals(Cst.errors(tree), Vector.empty)
    assertEquals(Xml.elements(tree, "a").length, 1)
    assertEquals(Xml.elements(tree, "b").length, 1)
    assertEquals(Xml.render(Xml.cst("<?xml version=\"1.0\"")), "<?xml version=\"1.0\"")
  }

  test("value: elements as objects, attributes as @name, repeats as arrays, text as strings, case kept") {
    val s = """<?xml version="1.0"?><Doc v="5-10"><!-- c --><a>1</a><a>2</a><b k='x'>t</b><c><d/>tail</c><e/></Doc>"""
    assertEquals(Json.print(Xml.value(Xml.cst(s))),
      """{"Doc":{"@v":"5-10","a":["1","2"],"b":{"@k":"x","#text":"t"},"c":{"d":"","#text":"tail"},"e":""}}""")
    assertEquals(Xml.attributes("""<x a="1" b='2' c d = "e f"/>"""), Vector("a" -> "1", "b" -> "2", "c" -> "", "d" -> "e f"))
    assertEquals(Xml.value(Xml.cst("just text")), Json.JStr("just text"))
    val e = """<a x="&lt;&amp;&gt;">S&amp;P &#169; &#x41; &unknown; &</a>"""
    assertEquals(Json.print(Xml.value(Xml.cst(e))), """{"a":{"@x":"<&>","#text":"S&P © A &unknown; &"}}""")
    assertEquals(Xml.render(Xml.cst(e)), e)
  }

  test("fromValue: the inverse of value, and the law value(cst(fromValue(v))) == v") {
    val v = Xml.value(Xml.cst("""<Doc v="5-10"><a>1</a><a>2</a><b k='x'>t</b><c><d/>tail</c><e/></Doc>""", Xml.strict))
    assertEquals(Xml.fromValue(v), """<Doc v="5-10"><a>1</a><a>2</a><b k="x">t</b><c><d/>tail</c><e/></Doc>""")
    assertEquals(Xml.value(Xml.cst(Xml.fromValue(v), Xml.strict)), v)
    val e = Xml.value(Xml.cst("""<a x="&lt;&amp;&gt;&quot;">S&amp;P &#169; &lt;b&gt;</a>""", Xml.strict))
    assertEquals(Xml.fromValue(e), """<a x="&lt;&amp;&gt;&quot;">S&amp;P © &lt;b&gt;</a>""")
    assertEquals(Xml.value(Xml.cst(Xml.fromValue(e), Xml.strict)), e)
    assertEquals(Xml.fromValue(Json.JObj(Vector("n" -> Json.JNum(2.5), "t" -> Json.JBool(true), "z" -> Json.JNull))), "<n>2.5</n><t>true</t><z/>")
    val deep = (1 to 100000).foldLeft(Json.JStr("x"): Json)((acc, _) => Json.JObj(Vector("d" -> acc)))
    assertEquals(Xml.fromValue(deep).length, 100000 * 7 + 1)
  }

  test("strict: no element is void, so <source>Coal</source> opens and closes; the HTML set still makes <br> void") {
    val s = "<a><source>Coal</source><br></a>"
    assertEquals(Cst.errors(Xml.cst(s, Xml.strict)).map(_._2), Vector("<br> was never closed"))
    assertEquals(Xml.elements(Xml.cst(s, Xml.strict), "source").length, 1)
    assertEquals(Cst.errors(Xml.cst(s)).map(_._2), Vector("</source> closes nothing"))
    assertEquals(Xml.render(Xml.cst(s, Xml.strict)), s)
  }

  test("an incremental reparse of markup equals a full one") {
    val session = Xml.parse(doc, 16)
    val at = doc.indexOf("world")
    val edited = doc.patch(at, "WORLD", 5)
    val re = Xml.reparse(session, doc, edited, at, at + 5, at + 5, 16)
    assertEquals(Xml.render(re.tree), edited)
    assertEquals(re.tree, Xml.parse(edited, 16).tree)
  }

  test("depth: the projections walk a 20 000-deep document the parser builds") {
    val n = 20000
    val s = ("<a>" * n) + "x" + ("</a>" * n)
    val tree = Xml.cst(s)
    assertEquals(Xml.render(tree), s)
    assertEquals(Xml.text(tree), "x")
    assertEquals(Xml.elements(tree, "a").length, n)
  }
}
