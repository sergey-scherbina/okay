package okay.telegram

import okay.{Async, async}
import okay.freer.*


import okay.given
import okay.freer.given
import okay.codec.Json
import okay.ui.{Event, Telegram, Ui}
import okay.ui.Telegram.{Act, Message}
import okay.ui.Telegram.Key

/** specs/telegram-bot.md — the performer, the router, and THE GATE: an
 * okay-ui application pressed through a chat edits its one message */
class TestChats extends munit.FunSuite:
  def go[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  val token = "1:T"

  test("perform: a Send answers its id, an Edit edits, an Answer answers, an Ask forces a reply; a refusal is told and answers None") {
    val api = FakeApi { case "sendMessage" => FakeApi.ok("""{"message_id":31}"""); case "editMessageText" => FakeApi.ok("true") }
    val told = scala.collection.mutable.ListBuffer.empty[Refused]
    val perform = Chats.perform(Bot(api, token), 5, r => async { told += r })
    val m = Message("<b>n=0</b>", Vector(Vector(Key.Press("+", "f1.0"))))
    assertEquals(go(perform(Act.Send(m))), Some(31L))
    assertEquals(go(perform(Act.Edit(31, m.copy(text = "n=1")))), None)
    assertEquals(Js.str(api.of("editMessageText").head, "text"), "n=1")
    assertEquals(go(perform(Act.Ask("Name?"))), None)
    assertEquals(Js.field(api.of("sendMessage")(1), "reply_markup"), Some(Json.parse("""{"force_reply":true}""")))
    // answerCallbackQuery is not in the fake's answers: refused, told, None — and nothing thrown
    assertEquals(go(perform(Act.Answer("cb", "outdated"))), None)
    assertEquals(told.map(_.method).toList, List("answerCallbackQuery"))
  }

  test("heard: a press and a message are the chat's host's; a payment is not") {
    assertEquals(Chats.heard(Update.Callback(1, 5, 42, 7, "f1.0", "cb")), Some(5L -> Telegram.Update.Pressed("f1.0", "cb")))
    assertEquals(Chats.heard(Update.Message(2, 5, 42, 8, "hello")), Some(5L -> Telegram.Update.Said("hello")))
    assertEquals(Chats.heard(Update.Paid(3, 5, 42, "p", "XTR", 250, "a", "b")), None)
    assertEquals(Chats.heard(Update.Other(4, "edited_message")), None)
  }

  test("AWAITING: between the screen's question and the answer, the chat is the screen's — and then it is not") {
    import Ui.*
    val api = FakeApi { case "sendMessage" => FakeApi.ok("""{"message_id":1}"""); case "editMessageText" => FakeApi.ok("true")
      case "answerCallbackQuery" => FakeApi.ok("true") }
    val bot = Bot(api, token)
    def view(s: String): Ui = Column(Vector(Input(s, "name", "Name"), Button("go", "go")))
    def update(s: String, e: Event): String = e match { case Event.Edited("name", v) => v; case _ => s }
    val chats = Chats(bot, (_, host) => Ui.run("")(view)(update)(host).map(_ => ()))
    def until(what: => Boolean, ms: Int = 5000): Unit =
      val end = System.currentTimeMillis + ms
      while !what && System.currentTimeMillis < end do Thread.sleep(10)
      assert(what, s"waited ${ms}ms: ${api.snapshot.map(_._1).toList}")
    assert(!chats.awaiting(5), "nothing asked yet")
    go(chats.hear(Update.Message(1, 5, 42, 9, "hi")))
    until(api.of("sendMessage").nonEmpty)
    // the pencil beside the Input: the screen asks, with ForceReply
    val markup = Js.field(api.of("sendMessage").head, "reply_markup").get
    val pencil = Js.arr(Js.arr(markup, "inline_keyboard").head, "").headOption
      .orElse(Js.arr(markup, "inline_keyboard").headOption.flatMap { case Json.JArr(row) => row.headOption; case _ => None })
      .map(k => Js.str(k, "callback_data")).getOrElse(fail(s"no keyboard in ${Json.print(markup)}"))
    go(chats.hear(Update.Callback(2, 5, 42, 1, pencil, "cb-1")))
    until(chats.awaiting(5))
    assert(Js.field(api.of("sendMessage")(1), "reply_markup").exists(j => Json.print(j).contains("force_reply")))
    // the typed value: the screen's, and the chat is free again
    go(chats.hear(Update.Message(3, 5, 42, 10, "ada")))
    until(api.of("editMessageText").exists(j => Js.str(j, "text").contains("ada")))
    assert(!chats.awaiting(5), "the question was answered")
  }

  test("THE GATE: a counter application, pressed through Chats, sends one message and then edits it") {
    import Ui.*
    val api = FakeApi { case "sendMessage" => FakeApi.ok("""{"message_id":1}"""); case "editMessageText" => FakeApi.ok("true")
      case "answerCallbackQuery" => FakeApi.ok("true") }
    val bot = Bot(api, token)
    def view(n: Int): Ui = Column(Vector(Text(s"count: $n"), Button("+", "inc")))
    def update(n: Int, e: Event): Int = e match { case Event.Pressed("inc") => n + 1; case _ => n }
    val chats = Chats(bot, (_, host) => Ui.run(0)(view)(update)(host).map(_ => ()))
    def until(what: => Boolean, ms: Int = 5000): Unit =
      val end = System.currentTimeMillis + ms
      while !what && System.currentTimeMillis < end do Thread.sleep(10)
      assert(what, s"waited ${ms}ms: ${api.snapshot.map(_._1).toList}")
    // the first message from chat 5 opens its application, which draws frame 1
    go(chats.hear(Update.Message(1, 5, 42, 9, "hi")))
    until(api.of("sendMessage").nonEmpty)
    val first = api.of("sendMessage").head
    assertEquals(Js.str(first, "text"), "count: 0")
    val data = Js.str(Json.JArr(Js.arr(Js.field(first, "reply_markup").get, "inline_keyboard")).vs.head match
      case Json.JArr(row) => row.head; case _ => Json.JNull, "callback_data")
    // the press: answered, and the ONE message edited with the new count
    go(chats.hear(Update.Callback(2, 5, 42, 1, data, "cb-1")))
    // BOTH calls, in either order (scheduler-default-flip): the press is
    // an event for the application, whose re-render on its own fiber is
    // the edit, and an `Answer` act the door performs — two independent
    // calls. Waiting for the edit alone and then asserting the answer
    // read their order; on `adaptive` the edit sometimes came first
    until(api.of("editMessageText").nonEmpty && api.of("answerCallbackQuery").nonEmpty)
    assertEquals(Js.str(api.of("editMessageText").head, "text"), "count: 1")
    assertEquals(Js.long(api.of("editMessageText").head, "message_id"), 1L)
    assertEquals(api.of("answerCallbackQuery").map(Js.str(_, "callback_query_id")), Vector("cb-1"))
    assertEquals(chats.opened, Set(5L))
    // a second chat is a second application, not the first one's state
    go(chats.hear(Update.Message(3, 6, 43, 1, "hi")))
    until(api.of("sendMessage").size >= 2)
    assertEquals(Js.str(api.of("sendMessage")(1), "text"), "count: 0")
    // a chat OPENED by the consumer, with no message heard: the screen, and nothing read as a value
    go(chats.open(7))
    until(api.of("sendMessage").size >= 3)
    assertEquals(chats.opened, Set(5L, 6L, 7L))
    go(chats.open(7))
    assertEquals(api.of("sendMessage").size, 3, "opening an open chat opens nothing twice")
  }
