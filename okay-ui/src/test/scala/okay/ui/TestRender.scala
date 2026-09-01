package okay.ui

import okay.codec.{Json, Schema}

/** The terminal's pure half, the React mapping and the Form — all
 * values, all with no screen, which is the seam's claim in action. */
class TestRender extends munit.FunSuite {

  import Ui.*

  val tree = Column(Vector(
    Text("title"),
    Row(Vector(Button("ok", "ok"), Button("no", "no"))),
    Input("abc", "name", "name"),
    Check(false, "sure", "sure?")))

  test("the terminal frame: rows side by side, columns stacked, focus marked") {
    assertEquals(Frame.render(tree), Vector(
      "title",
      "[ ok ] [ no ]",
      "name: [abc]",
      " [ ] sure?"))
    val focused = Ui.focusable(tree).lift(0)
    assertEquals(Frame.render(tree, focused)(1), "[>ok<] [ no ]")
  }

  test("keys interpret against the tree: tab, enter, edit, erase") {
    var focus = 0
    def hit(ch: Char): Option[Event] =
      val (f, e) = Frame.interpret(tree, focus, ch); focus = f; e
    assertEquals(hit('\t'), None)                      // -> "no"
    assertEquals(hit('\n'), Some(Event.Pressed("no")))
    assertEquals(hit('\t'), None)                      // -> input
    assertEquals(hit('x'), Some(Event.Edited("name", "abcx")))
    assertEquals(hit('\b'), Some(Event.Edited("name", "ab")))
    assertEquals(hit('\t'), None)                      // -> check
    assertEquals(hit('\n'), Some(Event.Toggled("sure", true)))
    assertEquals(hit('\t'), None)                      // wraps
    assertEquals(hit('\n'), Some(Event.Pressed("ok")))
  }

  test("the React mapping is pure and carries keys") {
    val e = React.elem(tree)
    assertEquals(e.tag, "div")
    assertEquals(e.props, Vector("className" -> "okay-col"))
    val row = e.children(1)
    assertEquals(row.children.map(_.tag), Vector("button", "button"))
    assertEquals(row.children(0).props, Vector("data-key" -> "ok"))
    assertEquals(row.children(0).text, Some("ok"))
    // the input is wrapped in a label, value carried
    val labeled = e.children(2)
    assertEquals(labeled.tag, "label")
    assertEquals(labeled.children(1).props,
      Vector("data-key" -> "name", "value" -> "abc"))
  }

  test("a DOM event comes back as ours, by key and kind") {
    assertEquals(React.event(tree, "ok", "click", ""), Some(Event.Pressed("ok")))
    assertEquals(React.event(tree, "name", "input", "xyz"),
      Some(Event.Edited("name", "xyz")))
    assertEquals(React.event(tree, "sure", "change", ""),
      Some(Event.Toggled("sure", true)))
    assertEquals(React.event(tree, "ghost", "click", ""), None)
  }
}

class TestForm extends munit.FunSuite {

  final case class Person(name: String, age: Int, ok: Boolean, note: Option[String])
  given Schema[Person] = Schema.derived

  val empty = Json.JObj(Vector.empty)

  test("a form renders from the Schema: inputs, a check, optional unrequired") {
    val ui = Form.of[Person](empty)
    val Ui.Column(fields, _) = ui: @unchecked
    assertEquals(fields.collect { case Ui.Input(_, k, _) => k },
      Vector("name", "age", "note"))
    assertEquals(fields.collect { case Ui.Check(_, k, _) => k }, Vector("ok"))
    // the optional field says so on its label, but keeps its plain key
    assert(fields.collectFirst { case Ui.Input(_, "note", l) => l }.get.contains("optional"))
  }

  test("edits fold in, typed by the schema; decode answers the SAME A") {
    val edits = Seq(
      Event.Edited("name", "ada"),
      Event.Edited("age", "36"),
      Event.Toggled("ok", true))
    val value = edits.foldLeft(empty: Json)(Form.edit[Person])
    assertEquals(Form.decode[Person](value), Right(Person("ada", 36, true, None)))
    // a non-number in a numeric field stays raw and FAILS decode — the
    // form cannot smuggle what the parser would not take
    val bad = Form.edit[Person](value, Event.Edited("age", "old"))
    assert(Form.decode[Person](bad).isLeft)
  }

  test("the dynamic form: a JSON Schema (as elicitation sends one)") {
    val schema = Json.parse("""{"type":"object","properties":{
      "path":{"type":"string"},"depth":{"type":"integer"},
      "mode":{"type":"string","enum":["fast","full"]},"dry":{"type":"boolean"}}}""")
    val ui = Form.ofSchema(schema)(Json.JObj(Vector.empty))
    val Ui.Column(fields, _) = ui: @unchecked
    assertEquals(fields.length, 4)
    assert(fields.exists { case Ui.Select(o, _, "mode") => o == Vector("fast", "full"); case _ => false })

    val value = Seq(
      Event.Edited("path", "/tmp"),
      Event.Edited("depth", "3"),
      Event.Chosen("mode", 1),
      Event.Toggled("dry", true))
      .foldLeft(Json.JObj(Vector.empty): Json)(Form.editSchema(schema, _, _))
    assertEquals(Json.print(value),
      """{"path":"/tmp","depth":3,"mode":"full","dry":true}""")
  }
}
