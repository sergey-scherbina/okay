package okay.ui

import okay.*
import okay.codec.Json
import okay.codec.Json.*

/**
 * specs/zipper.md — the editor driven through `Nav.update` as
 * `TestScreens` drives: the mark follows the cursor, edits land in
 * the document, done and cancel answer through the continuation.
 */
class TestJsonEditor extends munit.FunSuite {

  import Event.*

  val doc: Json = JObj(Vector(
    "name" -> JStr("ada"),
    "tags" -> JArr(Vector(JStr("a"), JStr("b"))),
    "age" -> JNum(36)))

  /** a driven editor: the frames after each event and what `done` was answered */
  final class Drive(start: Json = doc):
    var answer: Option[Option[Json]] = None
    var stack: List[Screen] = Nav.state(JsonEditor(start) { a => answer = Some(a); Nav.Pop })
    def step(events: Event*): Unit =
      for e <- events do stack = Nav.update(stack, e)
    def apply(events: Event*): Ui =
      step(events*)
      Nav.view(stack)

  /** the marked lines of a view — the law says there is exactly one */
  def marked(ui: Ui): Vector[String] = ui match
    case Ui.Column(Vector(Ui.Column(lines, "$doc"), _*), _) =>
      lines.collect { case Ui.Text(s, st) if st.bold => s.trim }
    case other => fail(s"not an editor view: $other")

  def mark(ui: Ui): String =
    val ms = marked(ui)
    assertEquals(ms.length, 1, "exactly one line is marked")
    ms.head

  test("the root is focused at start, and only it") {
    assertEquals(mark(Drive()()), "> {3}")
  }

  test("into/out/prev/next move the mark; a move that does not exist changes nothing") {
    val d = Drive()
    assertEquals(mark(d(Pressed("$into"))), "> name: \"ada\"")
    assertEquals(mark(d(Pressed("$next"))), "> tags: [2]")
    assertEquals(mark(d(Pressed("$into"))), "> 0: \"a\"")
    assertEquals(mark(d(Pressed("$next"))), "> 1: \"b\"")
    val before = d()
    assertEquals(d(Pressed("$next")), before)          // last sibling: nothing moves
    assertEquals(d(Pressed("$into")), before)          // a scalar: nothing to enter
    assertEquals(mark(d(Pressed("$out"))), "> tags: [2]")
    assertEquals(mark(d(Pressed("$prev"))), "> name: \"ada\"")
    assertEquals(d(Pressed("$prev")), d())             // first sibling: nothing moves
    assertEquals(mark(d(Pressed("$out"))), "> {3}")
    assertEquals(d(Pressed("$out")), d())              // the root has no parent
  }

  test("the value input shows the focused scalar; set replaces it with the parsed text, or the text") {
    val d = Drive()
    val v = d(Pressed("$into"), Pressed("$next"), Pressed("$next"))
    assertEquals(mark(v), "> age: 36")
    val input = v match
      case Ui.Column(Vector(_, _, Ui.Row(Vector(Ui.Input(value, "$value", _, _, _), _*), _), _*), _) => value
      case other => fail(s"no value input in $other")
    assertEquals(input, "36")
    d.step(Edited("$value", "37"), Pressed("$set"))
    d.step(Pressed("$prev"), Edited("$value", "not json"), Pressed("$set"))
    d.step(Pressed("$done"))
    assertEquals(d.answer, Some(Some(JObj(Vector(
      "name" -> JStr("ada"), "tags" -> JStr("not json"), "age" -> JNum(37))))))
  }

  test("delete removes the focus and moves to the parent; delete at the root is a no-op") {
    val d = Drive()
    assertEquals(d(Pressed("$delete")), d())
    d.step(Pressed("$into"), Pressed("$next"), Pressed("$into"))
    assertEquals(mark(d(Pressed("$delete"))), "> tags: [1]")
    d.step(Pressed("$done"))
    assertEquals(d.answer, Some(Some(JObj(Vector(
      "name" -> JStr("ada"), "tags" -> JArr(Vector(JStr("b"))), "age" -> JNum(36))))))
  }

  test("add inserts null after the focus — with the key draft in an object — and focuses it; at the root it appends inside") {
    val d = Drive()
    d.step(Pressed("$into"), Pressed("$next"), Pressed("$into"))          // tags[0]
    assertEquals(mark(d(Pressed("$add"))), "> 1: null")
    d.step(Pressed("$out"), Edited("$key", "city"))
    assertEquals(mark(d(Pressed("$add"))), "> city: null")
    d.step(Pressed("$out"), Edited("$key", "last"))
    assertEquals(mark(d(Pressed("$add"))), "> last: null")
    d.step(Pressed("$done"))
    assertEquals(d.answer, Some(Some(JObj(Vector(
      "name" -> JStr("ada"),
      "tags" -> JArr(Vector(JStr("a"), JNull, JStr("b"))),
      "city" -> JNull,
      "age" -> JNum(36),
      "last" -> JNull)))))
    // a scalar root cannot take a child
    val s = Drive(JNum(1))
    assertEquals(s(Pressed("$add")), s())
  }

  test("cancel answers None and the caller keeps the original; done answers the edited document") {
    val d = Drive()
    d.step(Pressed("$into"), Edited("$value", "\"grace\""), Pressed("$set"), Pressed("$cancel"))
    assertEquals(d.answer, Some(None))
    assertEquals(d.stack, Nil)
    val e = Drive()
    e.step(Pressed("$into"), Edited("$value", "\"grace\""), Pressed("$set"), Pressed("$done"))
    assertEquals(e.answer.flatten.map(j => j match { case JObj(fs) => fs.head; case _ => fail("") }), Some("name" -> JStr("grace")))
  }
}
