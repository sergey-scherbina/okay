package okay.ui

import okay.*
import okay.codec.{Json, Schema}
import okay.persist.MemoryStore

/**
 * stack-safety-ui: three walks fed from outside the program — a browser's
 * dotted path (`Form.renderAt` -> `focusAt`), a session's journal
 * (`Sessions.recover` -> `segments`, one per connection that closed) and
 * a document under edit (`JsonEditor.view` -> `outline`) — descended once
 * per level of what they were handed, and each was a StackOverflowError
 * at the depth a long-lived client or session reaches.
 */
class TestUiDepth extends munit.FunSuite:

  enum Chain derives Schema:
    case Leaf(label: String)
    case Node(next: Chain)

  def deepChain(n: Int): Chain =
    var c: Chain = Chain.Leaf("bottom")
    var i = 0
    while i < n do { c = Chain.Node(c); i += 1 }
    c

  test("Form.renderAt follows a 100 000-segment path to the leaf") {
    val n = 100000
    val value = Json.parse(Json.write(deepChain(n)))
    val path = ("next." * (n - 1)) + "next"
    val ui = Form.renderAt[Chain](value, path)
    // the drill view of the LEAF's node: one level, with its label widget
    def texts(u: Ui): Vector[String] = u match
      case Ui.Input(v, k, _, _, _) => Vector(s"$k=$v")
      case Ui.Text(t, _) => Vector(t)
      case Ui.Button(t, k, _) => Vector(s"[$t]$k")
      case other => Ui.plate.children(other).flatMap(texts)
    assert(texts(ui).exists(_.contains("bottom")), texts(ui).take(8).toString)
  }

  test("Sessions.recover refolds a journal that closed 200 000 times") {
    final case class Count(n: Int) derives Schema
    def view(s: Count): Ui = Ui.Column(Vector(Ui.Text(s.n.toString), Ui.Button("inc", "inc")))
    def update(s: Count, e: Event): Count = e match
      case Event.Pressed("inc") => Count(s.n + 1)
      case _ => s
    val s = Sessions.Session(MemoryStore().topic("ui", 2), "s1")
    val closed = Protocol.eventLine(Event.Closed)
    val press = Protocol.eventLine(Event.Pressed("inc"))
    var i = 0
    while i < 200000 do { val _ = Sessions.append(s, closed); i += 1 }
    val _ = Sessions.append(s, press)
    val (recovered, _) = Sessions.recover(s)(Count(0))(view)(update)
    assertEquals(recovered, Count(1))
  }

  test("JsonEditor.view outlines a document nested 100 000 deep") {
    var j: Json = Json.JNum(1)
    var i = 0
    while i < 100000 do { j = Json.JArr(Vector(j)); i += 1 }
    val ui = JsonEditor.view(JsonEditor.Ed(Zipper(j), "", ""))
    ui match
      case Ui.Column(Vector(Ui.Column(lines, "$doc"), _*), _) =>
        assertEquals(lines.length, 100001)
        assert(lines.head.asInstanceOf[Ui.Text].style.bold)
      case other => fail(s"not an editor view: ${other.getClass}")
  }
