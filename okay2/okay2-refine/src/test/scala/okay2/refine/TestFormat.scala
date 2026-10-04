package okay2.refine

import java.nio.charset.StandardCharsets.UTF_8

/** the format level over the dialects' own trees — JSON, XML, YAML and CBOR, the four okay2-codec reads */
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
    assertEquals(v.reasons.map(_.at), Vector(Path("cbor"), Path("text", "xml"), Path("text", "yaml")))
    assertEquals(v.reasons.find(_.at == Path("text", "xml")).map(_.reason), Some("begins with '{', not <"))
  }

  test("an XML document is text/xml and writes back; json says why not") {
    val in = bytes("""<trade id="1"><leg>fixed</leg><!-- c --></trade>""")
    val v = Format.detect.run(in)
    val (doc, by) = took(v)
    assertEquals(by, Path("text", "xml"))
    assert(doc.isInstanceOf[Doc.Xml])
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
    assertEquals(v.reasons.map(_.at), Vector(Path("cbor"), Path("text", "json"), Path("text", "yaml")))
    assertEquals(v.reasons.find(_.at == Path("text", "json")).map(_.reason), Some("begins with '<', not { or ["))
  }

  test("xml is STRICT: <source>Coal</source> is an element, and an HTML page's <br> is declined as never closed") {
    val coal = bytes("""<swap><source>Coal</source></swap>""")
    assertEquals(took(Format.detect.run(coal))._2, Path("text", "xml"))
    val html = Format.detect.run(bytes("""<p>a<br>b</p>"""))
    assertEquals(html.reasons.find(_.at == Path("text", "xml")).map(_.reason), Some("<br> was never closed"))
  }

  test("a bare scalar is no document; empty is empty; damage is the parser's own words") {
    assertEquals(Format.detect.run(bytes("42")).reasons.map(_.reason),
      Vector("bytes after the first item", "begins with '4', not { or [", "begins with '4', not <", "not a YAML mapping or sequence"))
    assertEquals(Format.detect.run(bytes("  ")).reasons.drop(1).map(_.reason), Vector("empty", "empty", "not a YAML mapping or sequence"))
    val damaged = Format.detect.run(bytes("""{"a": """))
    assert(damaged.reasons.exists(r => r.at == Path("text", "json") && r.reason.nonEmpty), damaged.toString)
  }

  test("bytes that are not UTF-8 are declined at text, by the offending byte") {
    val v = Format.detect.run(Array[Byte](0x7B, 0xFF.toByte, 0x7D))
    assertEquals(v.reasons.map(_.at), Vector(Path("cbor"), Path("text")))
    assertEquals(v.reasons(1), Refusal(Path("text"), "not UTF-8 at byte 1"))
    assertEquals(Format.Utf8.invalidAt(bytes("héllo ✓")), -1)
  }

  test("a byte-order mark before the root is skipped, and the document still writes back with it") {
    val in = bytes("﻿<a/>")
    val (doc, by) = took(Format.detect.run(in))
    assertEquals(by, Path("text", "xml"))
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right("﻿<a/>"))
  }

  test("a YAML mapping is text/yaml, writes back, and projects into the same Json as JSON") {
    val in = bytes("trade: swap\nlegs:\n  - fixed\n  - floating\n")
    val v = Format.detect.run(in)
    val (doc, by) = took(v)
    assertEquals(by, Path("text", "yaml"))
    assert(doc.isInstanceOf[Doc.Yaml])
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
    assertEquals(v.reasons.find(_.at == Path("text", "json")).map(_.reason), Some("begins with 't', not { or ["))
    val asJson = Format.value.run(Doc.Json(okay2.codec.Json.cst("""{"trade": "swap", "legs": ["fixed", "floating"]}""")))
    assertEquals(Format.value.run(doc).toOption, asJson.toOption)
  }

  test("one CBOR item is cbor and writes back; the text formats say why not; it has no value projection") {
    val in = Array[Byte](0xA1.toByte, 0x61, 0x61, 0x01) // {"a": 1}
    val v = Format.detect.run(in)
    val (doc, by) = took(v)
    assertEquals(by, Path("cbor"))
    assertEquals(Format.detect.write(doc).map(_.toSeq), Right(in.toSeq))
    assertEquals(v.reasons.map(_.at), Vector(Path("text")))
    assertEquals(Format.value.run(doc).reasons.map(_.reason), Vector("no value projection for cbor without a schema"))
    assertEquals(Format.cbor.run(Array[Byte](0x01, 0x02)).reasons.map(_.reason), Vector("bytes after the first item"))
  }

  test("a JSON object is NOT also YAML: the block dialect declines flow style before parsing") {
    val v = Format.detect.run(bytes("""{"a": 1}"""))
    assertEquals(took(v)._2, Path("text", "json"))
    assertEquals(v.reasons.find(_.at == Path("text", "yaml")).map(_.reason), Some("begins with '{': flow style is not the block dialect"))
  }

  test("an XML prolog is never a YAML key, and a YAML key beginning with '<' is still YAML") {
    val xml = Format.detect.run(bytes("<?xml version=\"1.0\"?>\n<a>1</a>"))
    assertEquals(took(xml)._2, Path("text", "xml"))
    assertEquals(xml.reasons.find(_.at == Path("text", "yaml")).map(_.reason), Some("begins with an XML prolog"))
    assertEquals(took(Format.detect.run(bytes("<<: x\nb: 1\n")))._2, Path("text", "yaml"))
  }
}
