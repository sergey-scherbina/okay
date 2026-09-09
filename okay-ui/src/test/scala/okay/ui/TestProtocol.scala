package okay.ui

import okay.*
import okay.given
import okay.codec.Json
import Protocol.Msg

/**
 * Stage 1 of specs/frontend.md: the protocol as an artifact. One
 * derived definition, two encodings, a hello that carries the
 * vocabulary, a document and a conformance script that cannot drift
 * from the code because they are rendered from it.
 */
class TestProtocol extends munit.FunSuite {

  import Ui.*

  def view(n: Int): Ui = Ui.Items(Vector(Ui.Text(s"n=$n"), Ui.Input(n.toString, "n"), Ui.Button("+", "f")), "l")
  def update(n: Int, e: Event): Int = e match
    case Event.Pressed("f") => n + 1
    case _ => n

  def talk(vocab: Option[Set[String]], lines: String*): Seq[String] =
    val hello = vocab.map(v => Protocol.line(Protocol.hello(v))).toList
    !.run(Writer.run(through(Writer.of(hello ++ lines.toList))(Wire.serve(0)(view)(update))))._1

  test("every message shape round-trips through JSON lines and CBOR bytes, and the enumerations spell themselves short") {
    val msgs = Vector[Msg](Protocol.hello(Set("table", "form")),
      Msg.Tree(Box(Vector(Text("x", Style(tone = Tone.Danger, size = Size.Large)), Image("/a", "a"),
        Input("", "pw", "Pw", InputKind.Secret, live = true), Button("go", "go", Role.Primary)),
        Dir.Horizontal, Vector(1, 2, 1, 1), gap = 1, pad = 2, key = "b")),
      Msg.Patch(Patch.Insert(List(0, 1), 2, Scroll(Text("s"), "sc"))),
      Msg.Event(Event.Key('x')), Msg.Event(Event.Closed), Msg.Close)
    for m <- msgs do
      assertEquals(Protocol.parse(Protocol.line(m)), Some(m))
      assertEquals(Protocol.ofBytes(Protocol.bytes(m)), Some(m))
    val l = Protocol.line(msgs(1))
    assert(l.contains("\"dir\":\"h\"") && l.contains("\"kind\":\"secret\"") && l.contains("\"tone\":\"danger\""), l)
    assert(l.startsWith("{\"Tree\":{\"ui\":{\"Box\":{"), l)
    assertEquals(Protocol.parse("{ not json"), None)
    assertEquals(Protocol.parse("""{"Tree":{"ui":{"Nope":{}}}}"""), None)
  }

  test("hello: a client claiming nothing receives no semantic node; one claiming items receives Items") {
    val plain = talk(Some(Set.empty))
    assert(!plain.head.contains("\"Items\""), plain.head)
    assertEquals(Protocol.treeOf(plain.head), Some(Ui.lower(view(0), Set.empty)))
    val rich = talk(Some(Set("items", "nonsense")))
    assertEquals(Protocol.treeOf(rich.head), Some(view(0)))
    // no hello at all: served as level L, the first line still handled
    val none = talk(None, Protocol.eventLine(Event.Pressed("f")))
    assertEquals(Protocol.treeOf(none.head), Some(Ui.lower(view(0), Set.empty)))
    assertEquals(none.length, 3)   // the tree, then the press's two narrow patches
    assertEquals(none.drop(1).map(Protocol.patchOf), Seq(
      Some(Patch.SetText(List(0), "n=1")), Some(Patch.SetValue(List(1), "1"))))
  }

  test("the client says hello first, with its vocabulary, then forwards events") {
    val sent = scala.collection.mutable.Buffer[String]()
    val down = Channel[String]()
    val feed = Channel[Event]()
    val host = new Host:
      def render(ui: Ui): Unit ! Async = pure(())
      def events: Source[Event] = Writer.of(feed)
    val fiber = Async.spawn(Wire.client(host, Set("table"))(Writer.of(down), l => async { sent += l; () }))
    val _ = feed.offer(Event.Pressed("x"))
    val _ = feed.offer(Event.Closed)
    down.close()
    fiber.join()
    assertEquals(sent.head, Protocol.line(Msg.Hello(Vector("table"), Protocol.version)))
    assertEquals(sent.drop(1).toList, List(Protocol.eventLine(Event.Pressed("x")), Protocol.eventLine(Event.Closed)))
  }

  test("the conformance script: a client that applies the ins holds every tree it names, and says every out") {
    val script = Protocol.conformance
    val ins = script.flatMap(r => Json.parse(r) match
      case Json.JObj(fs) if fs.exists(_._1 == "in") =>
        val line = fs.collectFirst { case ("in", v) => Json.print(v) }.get
        val tree = fs.collectFirst { case ("tree", v) => Json.print(v) }.get
        Some(line -> tree)
      case _ => None)
    val outs = script.flatMap(r => Json.parse(r) match
      case Json.JObj(fs) if fs.exists(_._1 == "out") => fs.collectFirst { case ("out", v) => Json.print(v) }
      case _ => None)
    assert(ins.nonEmpty && outs.nonEmpty)
    // Wire.client over a recording host reproduces the named trees
    val frames = scala.collection.mutable.Buffer[Ui]()
    val down = Channel[String]()
    val host = new Host:
      def render(ui: Ui): Unit ! Async = async { frames += ui; () }
      def events: Source[Event] = pure(())
    val fiber = Async.spawn(Wire.client(host)(Writer.of(down), _ => pure(())))
    for (l, _) <- ins do { val _ = down.offer(l) }
    down.close()
    fiber.join()
    assertEquals(frames.toVector.map(u => Json.write(u)(using Protocol.given_Schema_Ui)), ins.map(_._2))
    // and the outs are what the server's own script sent: hello first
    assertEquals(Protocol.parse(outs.head), Some(Protocol.hello(Set.empty)))
    assert(outs.forall(o => Protocol.parse(o).isDefined))
  }

  test("the document and the script are rendered from the schemas — the files cannot drift") {
    val root = Iterator.iterate(java.nio.file.Paths.get(sys.props("user.dir")).toAbsolutePath)(_.getParent)
      .takeWhile(_ != null).find(p => java.nio.file.Files.exists(p.resolve("build.sbt"))).get
    val doc = root.resolve("docs/protocol/frontend.md")
    val conf = root.resolve("docs/protocol/conformance.jsonl")
    val wantDoc = Protocol.document
    val wantConf = Protocol.conformance.mkString("", "\n", "\n")
    if sys.env.contains("OKAY_RENDER") then
      java.nio.file.Files.createDirectories(doc.getParent)
      val _ = java.nio.file.Files.writeString(doc, wantDoc)
      val _ = java.nio.file.Files.writeString(conf, wantConf)
    def read(p: java.nio.file.Path) =
      if java.nio.file.Files.exists(p) then java.nio.file.Files.readString(p) else ""
    val hint = "stale — regenerate with OKAY_RENDER=1 sbt okayUiJVM/testOnly okay.ui.TestProtocol"
    assertEquals(read(doc), wantDoc, s"docs/protocol/frontend.md $hint")
    assertEquals(read(conf), wantConf, s"docs/protocol/conformance.jsonl $hint")
    assert(wantDoc.contains("| Box {children: [Ui]") && wantDoc.contains("Msg = Hello {vocab: [string], version: int}"), wantDoc)
  }
}
