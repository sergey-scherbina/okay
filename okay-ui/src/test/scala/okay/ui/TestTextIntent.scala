package okay.ui

import okay.codec.Json

/**
 * ui-text-intent (specs/ui-product.md stage 2): what a text IS, as a
 * token the tree carries, so a host sets it in its own idiom and no
 * page has to say it with positional selectors.
 */
class TestTextIntent extends munit.FunSuite {

  import Ui.*

  test("the browser: a kind and an alignment are classes, and a defaulted Style writes none") {
    assertEquals(React.elem(Text("x")), Elem("span", Vector.empty, text = Some("x")))
    def cls(st: Style): String =
      React.elem(Text("x", st)).props.collectFirst { case ("className", c) => c }.getOrElse("")
    assertEquals(cls(Style(kind = Kind.Ident)), "okay-kind-ident")
    assertEquals(cls(Style(kind = Kind.Number, align = Align.End)), "okay-kind-number okay-align-end")
    // orthogonal, and beside the tokens that were already there
    assertEquals(cls(Style(bold = true, tone = Tone.Danger, kind = Kind.Ident)),
      "okay-bold okay-tone-danger okay-kind-ident")
  }

  test("live.js writes the same two classes, by the protocol's own short names") {
    val js = LiveJs.source
    assert(js.contains("""cls.push("okay-kind-" + st.kind)"""), js.take(0))
    assert(js.contains("""cls.push("okay-align-" + st.align)"""), js.take(0))
  }

  test("the terminal: an End-aligned cell is padded on the LEFT, and Start is unchanged") {
    // a weighted box gives the second column more width than its text
    // needs, which is the only place alignment can show
    // the weights have to give the number's column more room than its
    // text needs: that slack IS where an alignment can show
    def line(st: Style): String =
      Frame.render(Box(Vector(Text("a wide label"), Text("7", st)), Dir.Horizontal,
        weights = Vector(1, 3))).head
    val start = line(Style())
    val end = line(Style(align = Align.End))
    // the label's column is its natural 12; the number's is 9 by the
    // weights, so eight spaces move from one side to the other
    assertEquals(start, "a wide label" + "7" + " " * 8)
    assertEquals(end,   "a wide label" + " " * 8 + "7")
    assertEquals(Frame.width(start), Frame.width(end))
  }

  test("a defaulted Style renders exactly as it did on every host that reads it") {
    val tree = Column(Vector(Text("title", Style(bold = true)), Text("body")))
    assertEquals(Frame.render(tree), Vector("[1mtitle[0m", "body"))
    assert(!Html.render(tree).contains("okay-kind"), Html.render(tree))
    assert(!Html.render(tree).contains("okay-align"), Html.render(tree))
  }

  test("the wire: the tokens round-trip, and a Style from an OLDER server still decodes") {
    val st = Style(tone = Tone.Danger, kind = Kind.Ident, align = Align.End)
    val tree: Ui = Text("DE00", st)
    val line = Protocol.line(Protocol.Msg.Tree(tree))
    assert(line.contains("\"kind\":\"ident\""), line)
    assert(line.contains("\"align\":\"end\""), line)
    assertEquals(Protocol.parse(line), Some(Protocol.Msg.Tree(tree)))
    // what a server that predates this lane wrote: no kind, no align.
    // The defaults are what a new client must read them as, or a new
    // client cannot talk to an old server at all
    val old = """{"Tree":{"ui":{"Text":{"s":"DE00","style":{"bold":false,"dim":false,"tone":"danger","size":"normal"}}}}}"""
    assertEquals(Protocol.parse(old), Some(Protocol.Msg.Tree(Text("DE00", Style(tone = Tone.Danger)))))
  }

  test("CBOR carries them too — one definition, two encodings") {
    val tree: Ui = Text("7", Style(kind = Kind.Number, align = Align.End))
    val bytes = Protocol.bytes(Protocol.Msg.Tree(tree))
    assertEquals(Protocol.ofBytes(bytes), Some(Protocol.Msg.Tree(tree)))
    val _ = Json.JNull
  }
}
