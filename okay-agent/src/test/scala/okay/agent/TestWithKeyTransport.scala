package okay.agent

import okay.Answers
import okay.codec.Json

class TestWithKeyTransport extends okay.testkit.Munit.Diagnosed:
  test("first Tool attempt replaces client keys but journals original arguments") {
    val args = Json.JObj(Vector("amount" -> Json.JNum(100),
      Durable.KeyField -> Json.JStr("client-key"), Durable.KeyField -> Json.JStr("duplicate")))
    val call = ToolCall("call", "charge", args)
    val journal = Durable.MemoryJournal()
    var sent = Vector.empty[(String, Json)]
    val provider = new Answers[Tool]:
      def handle[A](op: Tool[A]): A = op match
        case Tool.Call(c) =>
          assertEquals(journal.all.size, 1)
          c.args match
            case Json.JObj(fs) => sent = fs
            case other => fail(s"unexpected arguments $other")
          "receipt"
    onFailure(s"journal=${journal.all} sent=$sent")
    assertEquals(Durable.tools(provider, journal)(_ => Durable.OnRepeat.WithKey)
      .handle(Tool.Call(call)), "receipt")
    assertEquals(sent.filter(_._1 == Durable.KeyField),
      Vector(Durable.KeyField -> Json.JStr(journal.all.head.key)))
    assertEquals(sent.filterNot(_._1 == Durable.KeyField), Vector("amount" -> Json.JNum(100)))
    assertEquals(journal.all.head.fingerprint, s"charge(${Json.print(args)})")
    assertEquals(Durable.replaying(journal).handle(Tool.Call(call)), "receipt")
  }
