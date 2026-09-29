package okay.refine

import okay.testkit.Munit.Diagnosed
import java.nio.charset.StandardCharsets.UTF_8

/** specs/refine.md, stage 1: the format level over the dialects' own trees */
class TestFormat extends Diagnosed:

  private def bytes(s: String): Array[Byte] = s.getBytes(UTF_8)

  private def took(v: Verdict[Doc]): (Doc, Path) = v match
    case Verdict.Took(d, by, _) => (d, by)
    case other => fail(s"expected Took, got $other")

  test("a JSON object is text/json and writes back to its bytes") {
    val in = bytes("""{"a": [1, 2], "b": {"c": null}}""")
    val v = Format.detect.run(in)
    note(v.toString)
    val (doc, by) = took(v)
    assertEquals(by, Path("text", "json"))
    assert(doc.isInstanceOf[Doc.Json])
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
  }

  test("an XML document is text/xml and writes back") {
    val in = bytes("""<trade id="1"><leg>fixed</leg><!-- c --></trade>""")
    val v = Format.detect.run(in)
    note(v.toString)
    val (doc, by) = took(v)
    assertEquals(by, Path("text", "xml"))
    assert(doc.isInstanceOf[Doc.Xml])
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
  }

  test("a YAML mapping is text/yaml and writes back") {
    val in = bytes("trade: swap\nlegs:\n  - fixed\n  - floating\n")
    val v = Format.detect.run(in)
    note(v.toString)
    val (doc, by) = took(v)
    assertEquals(by, Path("text", "yaml"))
    assert(doc.isInstanceOf[Doc.Yaml])
    assertEquals(Format.detect.write(doc).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
  }

  test("one CBOR item is cbor and writes back; the text formats say why not") {
    val in = Array[Byte](0xA1.toByte, 0x61, 0x61, 0x01)   // {"a": 1}
    val v = Format.detect.run(in)
    note(v.toString)
    val (doc, by) = took(v)
    assertEquals(by, Path("cbor"))
    assertEquals(doc.asInstanceOf[Doc.Cbor].bytes.toSeq, in.toSeq)
    assertEquals(Format.detect.write(doc).map(_.toSeq), Right(in.toSeq))
    assertEquals(v.reasons.map(_.at), Vector(Path("text")))
  }

  test("a bare scalar is declined by all three text formats, each with its reason") {
    val v = Format.detect.run(bytes("hello"))
    note(v.toString)
    v match
      case Verdict.Declined(tried) =>
        assertEquals(tried.map(_.at), Vector(Path("cbor"), Path("text", "json"), Path("text", "xml"), Path("text", "yaml")))
        // the JSON tree's own words for a bare word; a bare NUMBER is a
        // valid JSON value and is declined as no document instead
        assertEquals(tried.find(_.at == Path("text", "json")).map(_.reason), Some("unexpected 'hello' at Span(0,0,0,5)"))
        assertEquals(tried.find(_.at == Path("text", "xml")).map(_.reason), Some("no element"))
        assertEquals(tried.find(_.at == Path("text", "yaml")).map(_.reason), Some("not a YAML mapping or sequence"))
      case other => fail(s"expected Declined, got $other")
    assertEquals(Format.json.run("42").reasons.map(_.reason), Vector("not a JSON object or array"))
  }

  test("KNOWN GAP (backlog xml-processing-instruction): an XML prolog is read as an unclosed tag") {
    val v = Format.detect.run(bytes("""<?xml version="1.0"?><a/>"""))
    note(v.toString)
    // pinned so the fix flips this test: when the dialect learns `<?…?>`,
    // this becomes Took(text/xml) and the box in specs/refine.md closes
    assertEquals(v.reasons.find(_.at == Path("text", "xml")).map(_.reason), Some("unclosed"))
  }

  test("damaged JSON is declined by json in the tree's own words") {
    val v = Format.detect.run(bytes("""{"a":"""))
    note(v.toString)
    val json = v.reasons.find(_.at == Path("text", "json"))
    assert(json.isDefined, v.toString)
    assertNotEquals(json.get.reason, "not a JSON object or array")
  }

  test("bytes that are not UTF-8 decline text with the offset, and cbor was tried too") {
    val v = Format.detect.run(Array[Byte](0x7B, 0xFF.toByte, 0xFE.toByte))
    note(v.toString)
    v match
      case Verdict.Declined(tried) =>
        assertEquals(tried.map(_.at), Vector(Path("cbor"), Path("text")))
        assertEquals(tried(1).reason, "not UTF-8 at byte 1")
      case other => fail(s"expected Declined, got $other")
  }

  test("a JSON object is NOT also YAML here: the block dialect's root scalar is the tell") {
    // `{"a": 1}` IS a YAML flow mapping by the YAML spec; this dialect
    // is block-only, reads `{` as a scalar and the rest as pairs with no
    // error node, and claimed every JSON object on TestFormat's first
    // run. When flow style lands, this verdict becomes Unclear naming
    // both — by design (specs/refine.md, Decisions)
    val v = Format.detect.run(bytes("""{"a": 1}"""))
    note(v.toString)
    assertEquals(took(v)._2, Path("text", "json"))
    assertEquals(v.reasons.find(_.at == Path("text", "yaml")).map(_.reason), Some("not a YAML mapping or sequence"))
  }

  test("UTF-8 validity: ASCII, multi-byte, a lone continuation byte, a truncated sequence") {
    assertEquals(Format.Utf8.invalidAt(bytes("plain")), -1)
    assertEquals(Format.Utf8.invalidAt(bytes("своп €")), -1)
    assertEquals(Format.Utf8.invalidAt(Array[Byte](0x61, 0x80.toByte)), 1)
    assertEquals(Format.Utf8.invalidAt(Array[Byte](0x61, 0xE2.toByte, 0x82.toByte)), 1)
    assertEquals(Format.Utf8.invalidAt(Array[Byte](0xC0.toByte, 0x80.toByte)), 0)
  }
