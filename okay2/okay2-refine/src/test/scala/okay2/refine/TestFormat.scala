package okay2.refine

import java.nio.charset.StandardCharsets.UTF_8

/** the format level over the dialects' own trees — JSON and XML, the two the Scala 2 codec has */
class TestFormat extends munit.FunSuite {

  private def bytes(s: String): Array[Byte] = s.getBytes(UTF_8)

  private def took(v: Verdict[Doc]): (Doc, Path) = v match {
    case Verdict.Took(d, by, _) => (d, by)
    case other => fail(s"expected Took, got $other")
  }

  test("a JSON object is text/json and writes back to its bytes; xml says why not") {
    val in = bytes("""{"a": [1, 2], "b": {"c": null}}""")
    val v = Format.detect.run(in)
    val (doc, by) = took(v)
    assertEquals(by, Path("text", "json"))
    assert(doc.isInstanceOf[Doc.Json])
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
    assertEquals(v.reasons, Vector(Refusal(Path("text", "xml"), "begins with '{', not <")))
  }

  test("an XML document is text/xml and writes back; json says why not") {
    val in = bytes("""<trade id="1"><leg>fixed</leg><!-- c --></trade>""")
    val v = Format.detect.run(in)
    val (doc, by) = took(v)
    assertEquals(by, Path("text", "xml"))
    assert(doc.isInstanceOf[Doc.Xml])
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
    assertEquals(v.reasons, Vector(Refusal(Path("text", "json"), "begins with '<', not { or [")))
  }

  test("xml is STRICT: <source>Coal</source> is an element, and an HTML page's <br> is declined as never closed") {
    val coal = bytes("""<swap><source>Coal</source></swap>""")
    assertEquals(took(Format.detect.run(coal))._2, Path("text", "xml"))
    val html = Format.detect.run(bytes("""<p>a<br>b</p>"""))
    assertEquals(html.reasons.find(_.at == Path("text", "xml")).map(_.reason), Some("<br> was never closed"))
  }

  test("a bare scalar is no document; empty is empty; damage is the parser's own words") {
    assertEquals(Format.detect.run(bytes("42")).reasons.map(_.reason), Vector("begins with '4', not { or [", "begins with '4', not <"))
    assertEquals(Format.detect.run(bytes("  ")).reasons.map(_.reason), Vector("empty", "empty"))
    val damaged = Format.detect.run(bytes("""{"a": """))
    assert(damaged.reasons.exists(r => r.at == Path("text", "json") && r.reason.nonEmpty), damaged.toString)
  }

  test("bytes that are not UTF-8 are declined at text, by the offending byte") {
    val v = Format.detect.run(Array[Byte](0x7B, 0xFF.toByte, 0x7D))
    assertEquals(v.reasons, Vector(Refusal(Path("text"), "not UTF-8 at byte 1")))
    assertEquals(Format.Utf8.invalidAt(bytes("héllo ✓")), -1)
  }

  test("a byte-order mark before the root is skipped, and the document still writes back with it") {
    val in = bytes("﻿<a/>")
    val (doc, by) = took(Format.detect.run(in))
    assertEquals(by, Path("text", "xml"))
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right("﻿<a/>"))
  }
}
