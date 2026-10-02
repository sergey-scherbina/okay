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

  test("xml is STRICT: <source>Coal</source> is an element, and an HTML page's <br> is declined as never closed") {
    val coal = bytes("""<swap><source>Coal</source></swap>""")
    assertEquals(took(Format.detect.run(coal))._2, Path("text", "xml"))
    val html = Format.detect.run(bytes("""<p>a<br>b</p>"""))
    assertEquals(html.reasons.find(_.at == Path("text", "xml")).map(_.reason), Some("<br> was never closed"))
  }

  test("a bare scalar is declined by all three text formats, each with its reason") {
    val v = Format.detect.run(bytes("hello"))
    note(v.toString)
    v match
      case Verdict.Declined(tried) =>
        assertEquals(tried.map(_.at), Vector(Path("cbor"), Path("text", "json"), Path("text", "xml"), Path("text", "yaml")))
        // since format-cheap-decline JSON and XML say it by the first character, before any parse
        assertEquals(tried.find(_.at == Path("text", "json")).map(_.reason), Some("begins with 'h', not { or ["))
        assertEquals(tried.find(_.at == Path("text", "xml")).map(_.reason), Some("begins with 'h', not <"))
        assertEquals(tried.find(_.at == Path("text", "yaml")).map(_.reason), Some("not a YAML mapping or sequence"))
      case other => fail(s"expected Declined, got $other")
    assertEquals(Format.json.run("42").reasons.map(_.reason), Vector("begins with '4', not { or ["))
  }

  test("an XML document WITH its declaration is text/xml (was a KNOWN GAP until xml-processing-instruction)") {
    val in = bytes("""<?xml version="1.0" encoding="UTF-8"?><a/>""")
    val v = Format.detect.run(in)
    note(v.toString)
    assertEquals(took(v)._2, Path("text", "xml"))
    assertEquals(Format.detect.write(took(v)._1).map(new String(_, UTF_8)), Right(new String(in, UTF_8)))
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
    // since format-cheap-decline the block dialect says so before parsing: the verdict is the same, the reason sooner
    assertEquals(v.reasons.find(_.at == Path("text", "yaml")).map(_.reason), Some("begins with '{': flow style is not the block dialect"))
  }

  test("format-cheap-decline: each dialect declines by its first character before parsing, with the same verdicts as before") {
    def reasons(s: String) = Format.detect.run(s.getBytes("UTF-8")) match
      case Verdict.Declined(tried) => tried.map(r => r.at.steps.last -> r.reason).toMap
      case Verdict.Took(_, by, declined) => declined.map(r => r.at.steps.last -> r.reason).toMap + ("took" -> by.toString)
      case other => fail(other.toString)
    val xml = reasons("<?xml version=\"1.0\"?>\n<a>1</a>")
    assertEquals(xml("took"), "text/xml")
    assertEquals(xml("json"), "begins with '<', not { or [")
    assertEquals(xml("yaml"), "begins with an XML prolog")
    val json = reasons("  {\"a\": [1, 2]}")
    assertEquals(json("took"), "text/json")
    assertEquals(json("xml"), "begins with '{', not <")
    assertEquals(json("yaml"), "begins with '{': flow style is not the block dialect")
    val yaml = reasons("a: 1\nb:\n  - x\n")
    assertEquals(yaml("took"), "text/yaml")
    assertEquals(yaml("json"), "begins with 'a', not { or [")
    // text before the root element is not a well-formed XML document
    assertEquals(reasons("hello <a/>")("xml"), "begins with 'h', not <")
    assertEquals(reasons("   ")("json"), "empty")
    // a byte-order mark before the declaration: the XML parser accepts it, so the first-character test must too
    // (format-lead-bom: okay-fin's corpus cds-index-tranche.xml declined under format-cheap-decline)
    assertEquals(reasons("\uFEFF<?xml version=\"1.0\"?>\n<a>1</a>")("took"), "text/xml")
    // a YAML mapping whose first key begins with '<' is still YAML
    assertEquals(reasons("<<: x\nb: 1\n").get("yaml"), None)
  }

  test("UTF-8 validity: ASCII, multi-byte, a lone continuation byte, a truncated sequence") {
    assertEquals(Format.Utf8.invalidAt(bytes("plain")), -1)
    assertEquals(Format.Utf8.invalidAt(bytes("своп €")), -1)
    assertEquals(Format.Utf8.invalidAt(Array[Byte](0x61, 0x80.toByte)), 1)
    assertEquals(Format.Utf8.invalidAt(Array[Byte](0x61, 0xE2.toByte, 0x82.toByte)), 1)
    assertEquals(Format.Utf8.invalidAt(Array[Byte](0xC0.toByte, 0x80.toByte)), 0)
  }
