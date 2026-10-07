package okay.telegram

import okay.{Async, Timer, async}
import okay.freer.*
import okay.given
import okay.freer.given
import okay.ui.Telegram.{Act, Key, Message}

/** a timer that fires when the test says so — a window closes when
 * `close()` is called, never on the wall clock */
final class ManualTimer extends Timer:
  private val armed = scala.collection.mutable.ListBuffer.empty[() => Unit]
  def after(millis: Long)(k: () => Unit): () => Unit =
    armed.synchronized { armed += k; () }
    () => armed.synchronized { armed -= k; () }
  def pending: Int = armed.synchronized(armed.size)
  def close(): Unit =
    val ks = armed.synchronized { val l = armed.toList; armed.clear(); l }
    ks.foreach(_())

/** specs/telegram-live.md — edits coalesced, last write wins; the
 * command table installed and dispatched */
class TestLive extends munit.FunSuite:
  def go[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  val token = "1:T"
  def api() = FakeApi { case "sendMessage" => FakeApi.ok("""{"message_id":9"""+"}")
    case "editMessageText" => FakeApi.ok("true"); case "answerCallbackQuery" => FakeApi.ok("true") }
  def m(s: String) = Message(s, Vector(Vector(Key.Press("x", "x"))))
  def until(what: => Boolean, ms: Int = 5000): Unit =
    val end = System.currentTimeMillis + ms
    while !what && System.currentTimeMillis < end do Thread.sleep(5)
    assert(what, s"waited ${ms}ms")

  test("three edits in one window are ONE call, the third's; the next window is the burst's, not a cadence") {
    val a = api(); given t: ManualTimer = ManualTimer()
    val perform = Chats.performThrottled(Bot(a, token), 5, 2000)
    assertEquals(go(perform(Act.Edit(1, m("n=1")))), None)
    assertEquals(a.of("editMessageText").map(Js.str(_, "text")), Vector("n=1"))   // the first, at once
    go(perform(Act.Edit(1, m("n=2")))): Unit
    go(perform(Act.Edit(1, m("n=3")))): Unit
    assertEquals(a.of("editMessageText").size, 1, "held")
    assertEquals(t.pending, 1)
    t.close()
    until(a.of("editMessageText").size == 2)
    assertEquals(a.of("editMessageText").map(Js.str(_, "text")), Vector("n=1", "n=3"))
    until(t.pending == 1)          // the send opened the next window
    t.close()                      // nothing held: it just closes
    assertEquals(t.pending, 0)
    go(perform(Act.Edit(1, m("n=4")))): Unit
    assertEquals(a.of("editMessageText").map(Js.str(_, "text")), Vector("n=1", "n=3", "n=4"), "after the window: at once")
  }

  test("an edit equal to the last sent is dropped; two messages do not hold each other; Send and Answer are never held") {
    val a = api(); given t: ManualTimer = ManualTimer()
    val perform = Chats.performThrottled(Bot(a, token), 5, 2000)
    go(perform(Act.Edit(1, m("a")))): Unit
    go(perform(Act.Edit(1, m("a")))): Unit
    assertEquals(a.of("editMessageText").size, 1, "not modified: not sent")
    go(perform(Act.Edit(2, m("b")))): Unit
    assertEquals(a.of("editMessageText").map(Js.long(_, "message_id")), Vector(1L, 2L))
    go(perform(Act.Edit(1, m("a2")))): Unit // held
    assertEquals(go(perform(Act.Send(m("new")))), Some(9L))
    go(perform(Act.Answer("cb", "ok"))): Unit
    assertEquals(a.snapshot.map(_._1).toList.takeRight(2), List("sendMessage", "answerCallbackQuery"))
    t.close()
    until(a.of("editMessageText").size == 3)
    assertEquals(Js.str(a.of("editMessageText").last, "text"), "a2")
  }

  test("a held edit the API refuses reaches `refused` once, naming the method") {
    val a = FakeApi { case "sendMessage" => FakeApi.ok("""{"message_id":9}""") } // no editMessageText: refused
    given t: ManualTimer = ManualTimer()
    val told = scala.collection.mutable.ListBuffer.empty[Refused]
    val perform = Chats.performThrottled(Bot(a, token), 5, 2000, r => async { told.synchronized { told += r } })
    go(perform(Act.Edit(1, m("a")))): Unit
    go(perform(Act.Edit(1, m("b")))): Unit
    t.close()
    until(told.synchronized(told.size) == 2)
    assertEquals(told.synchronized(told.map(_.method).toList), List("editMessageText", "editMessageText"))
  }

  test("Command: a bad name is refused here, without a call; dispatch reads /name, /name@bot, /name args") {
    val a = api()
    val cmds = Vector(Command("start", "Home", "home"), Command("agents", "Agents", "agents"))
    assertEquals(go(Command.install(Bot(a, token), cmds :+ Command("Agents", "x", "x"))),
      Left(Refused("setMyCommands", 400, "command 'Agents' is not [a-z0-9_]{1,32}")))
    assertEquals(a.snapshot.size, 0)
    val a2 = FakeApi { case "setMyCommands" => FakeApi.ok("true") }
    assertEquals(go(Command.install(Bot(a2, token), cmds)), Right(()))
    assertEquals(Js.arr(a2.of("setMyCommands").head, "commands").size, 2)
    assertEquals(Command.dispatch(cmds, "/agents"), Some("agents"))
    assertEquals(Command.dispatch(cmds, "/agents@nadia_bot"), Some("agents"))
    assertEquals(Command.dispatch(cmds, "/agents now"), Some("agents"))
    assertEquals(Command.dispatch(cmds, "agents"), None)
    assertEquals(Command.dispatch(cmds, "/models"), None)
  }
