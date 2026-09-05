package okay.demo

import okay.agent.ToolCall
import okay.codec.Json
import okay.codec.Json.*

/**
 * The demo's own domain, and the one claim it exists to carry: the
 * board is DERIVED and the log is the truth.
 */
class TestBoard extends munit.FunSuite {

  def fresh: Board = Board(Board.topicOf(Board.store(":memory:")))

  test("a task added is a task listed") {
    val b = fresh
    assertEquals(b.add("water the plants", "ann").map(_.id), Some(1L))
    assertEquals(b.add("call the plumber", "bob").map(_.id), Some(2L))
    assertEquals(b.all.map(_.text), Vector("water the plants", "call the plumber"))
    assertEquals(b.of("ann").map(_.text), Vector("water the plants"))
  }

  test("assigning shows up on both sides of the board") {
    val b = fresh
    b.add("fix the door", "ann"): Unit
    b.assign(1, "bob"): Unit
    assertEquals(b.of("bob").map(_.id), Vector(1L))
    assertEquals(b.of("ann").map(_.id), Vector(1L), "the owner still sees their own task")
  }

  /**
   * Drop the projection, rebuild it from the log, and get the same
   * board. If this fails, nothing else the demo says about durability
   * is worth reading.
   */
  test("the board is a projection: replay rebuilds exactly what was there") {
    val store = Board.store(":memory:")
    val b = Board(Board.topicOf(store))
    b.add("water the plants", "ann"): Unit
    b.add("call the plumber", "bob"): Unit
    b.assign(2, "ann"): Unit
    b.complete(1): Unit
    val before = b.all

    val rebuilt = Board(Board.topicOf(store))
    assertEquals(rebuilt.all, Vector.empty, "a fresh projection starts empty")
    assertEquals(rebuilt.replay(), 4L, "every record was walked")
    assertEquals(rebuilt.all, before, "the rebuilt board differs from the one that was live")
  }

  test("replaying twice is replaying once") {
    val store = Board.store(":memory:")
    val b = Board(Board.topicOf(store))
    b.add("one", "ann"): Unit
    b.assign(1, "bob"): Unit
    val once = { val r = Board(Board.topicOf(store)); r.replay(): Unit; r.all }
    val twice = { val r = Board(Board.topicOf(store)); r.replay(): Unit; r.replay(): Unit; r.all }
    assertEquals(twice, once)
  }

  test("a change rings the listeners, and a replay does not") {
    val store = Board.store(":memory:")
    val b = Board(Board.topicOf(store))
    var rung = Vector.empty[String]
    b.onChange(w => rung = rung :+ w)
    b.add("one", "ann"): Unit
    b.complete(1): Unit
    assertEquals(rung, Vector("add", "done"))
    // a restore rebuilds what happened; it does not announce it again
    val r = Board(Board.topicOf(store))
    var replayed = Vector.empty[String]
    r.onChange(w => replayed = replayed :+ w)
    r.replay(): Unit
    assertEquals(replayed, Vector.empty)
  }

  test("the tools answer with data, including when they cannot") {
    val b = fresh
    val t = BoardTools.table(b)
    def call(name: String, args: (String, Json)*): String =
      t(name)(ToolCall("1", name, JObj(args.toVector)))
    assert(call("board_add", "text" -> JStr("mow the lawn"), "owner" -> JStr("ann"))
      .contains("mow the lawn"))
    assert(call("board_list").contains("mow the lawn"))
    // a tool that cannot do the thing says so as DATA: a model given
    // an exception learns nothing, one given an error can explain it
    assert(call("board_done", "id" -> JNum(99)).contains("error"), call("board_done", "id" -> JNum(99)))
    assert(call("board_assign", "id" -> JNum(1)).contains("error"))
  }
}
