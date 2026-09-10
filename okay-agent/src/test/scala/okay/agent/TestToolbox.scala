package okay.agent

import okay.codec.{Json, Schema}
import Json.{JObj, JStr, JNum, JArr}

/**
 * One declaration, three interpretations (specs/optics-outside.md,
 * stage 2).
 *
 * The tests worth reading are the two that state a COUPLING: the
 * declaration and the decode come from the same `Schema`, and `specs`
 * and `table` come from the same vector. Those are the properties the
 * hand-written pair could not have.
 */
class TestToolbox extends munit.FunSuite {

  final case class Add(text: String, owner: String)
  given Schema[Add] = Schema.derived

  final case class Listing(who: Option[String])
  given Schema[Listing] = Schema.derived

  val box: Toolbox = Toolbox.empty
    .on[Add]("add", "Add a task.")(a => s"${a.text}/${a.owner}")
    .on[Listing]("list", "List them.")(l => l.who.getOrElse("everyone"))

  val add: Toolbox.Entry = box.entries(0)
  val list: Toolbox.Entry = box.entries(1)

  def call(name: String, args: (String, Json)*): ToolCall =
    ToolCall("1", name, JObj(args.toVector))

  def props(t: ToolSpec): Vector[String] = t.schema match
    case JObj(fs) => fs.collectFirst { case ("properties", JObj(ps)) => ps.map(_._1) }.getOrElse(Vector.empty)
    case _ => Vector.empty

  def required(t: ToolSpec): Vector[String] = t.schema match
    case JObj(fs) => fs.collectFirst { case ("required", JArr(rs)) => rs.collect { case JStr(s) => s } }
      .getOrElse(Vector.empty)
    case _ => Vector.empty

  test("the declaration is derived from the argument type") {
    assertEquals(add.spec.name, "add")
    assertEquals(add.spec.description, "Add a task.")
    assertEquals(props(add.spec), Vector("text", "owner"))
    assertEquals(required(add.spec), Vector("text", "owner"))
  }

  test("an Option field is a property but not required") {
    assertEquals(props(list.spec), Vector("who"))
    assertEquals(required(list.spec), Vector.empty)
  }

  test("the same Schema that declared the tool decodes the call") {
    assertEquals(add.handle(call("add", "text" -> JStr("mow"), "owner" -> JStr("ann"))), "mow/ann")
    assertEquals(list.handle(call("list")), "everyone")
    assertEquals(list.handle(call("list", "who" -> JStr("ann"))), "ann")
  }

  test("a call that does not decode answers with data, naming the tool") {
    val r = add.handle(call("add", "text" -> JStr("mow")))
    assert(r.contains("error"), r)
    assert(r.contains("add"), r)
    // and it is JSON a model can read, not an exception
    assert(Json.parse(r).isInstanceOf[JObj], r)
  }

  test("a wrongly typed argument is the same kind of answer") {
    val r = add.handle(call("add", "text" -> JNum(1), "owner" -> JStr("ann")))
    assert(r.contains("error"), r)
  }

  test("specs and table are the same names, by construction") {
    assertEquals(box.specs.map(_.name).toVector, Vector("add", "list"))
    assertEquals(box.table.keySet, Set("add", "list"))
    assertEquals(box.specs.length, box.table.size)
  }

  test("the table dispatches to the declaring tool") {
    assertEquals(box.table("add")(call("add", "text" -> JStr("mow"), "owner" -> JStr("ann"))), "mow/ann")
    assertEquals(box.call(call("list")), Some("everyone"))
    assertEquals(box.call(call("nope")), None)
  }

  test("a duplicate name is the one inconsistency left, and it is named") {
    val twice = box.on[Add]("add", "again.")(_ => "second")
    assertEquals(twice.duplicates, Vector("add"))
    // the shapes genuinely disagree, which is why it is reported and
    // not silently reconciled
    assertEquals(twice.specs.length, 3)
    assertEquals(twice.table.size, 2)
    assertEquals(box.duplicates, Vector.empty)
  }

  test("a raw tool keeps its own schema and gets the arguments untouched") {
    val schema = JObj(Vector("type" -> JStr("object"), "additionalProperties" -> Json.JBool(true)))
    val patched = Toolbox.empty.raw("patch", "Merge a patch.", schema)(j => Json.print(j))
    assertEquals(patched.specs.head.schema, schema)
    assertEquals(patched.call(call("patch", "a" -> JNum(1))), Some("""{"a":1}"""))
  }
}
