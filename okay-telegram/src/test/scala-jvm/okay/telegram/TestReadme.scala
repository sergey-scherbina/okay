package okay.telegram
import okay.freer.*

import okay.*

import okay.given
import okay.freer.given
import okay.ui.{Event, Ui}

/** the README's examples, compiled — a readme whose examples do not
 * compile is worse than none */
class TestReadme extends munit.FunSuite:

  test("an okay-ui application, in a chat (the loop is not run: it would poll)") {
    val api = FakeApi { case "getMe" => FakeApi.ok("""{"id":1}""") }
    val bot = Bot(api, "BOT_TOKEN")

    def view(n: Int): Ui = Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+", "inc")))
    def update(n: Int, e: Event): Int = e match
      case Event.Pressed("inc") => n + 1
      case _ => n

    val chats = Chats(bot, (_, host) => Ui.run(0)(view)(update)(host).map(_ => ()))
    val loop: Long ! Async = bot.serve(chats.hear)
    assert(loop != null)
  }

  test("a plain bot") {
    def answer(bot: Bot)(u: Update): Unit ! Async = u match
      case Update.Message(_, chat, _, _, text) => bot.send(chat, s"you said: $text").map(_ => ())
      case Update.PreCheckout(_, _, id, _, _, _) => bot.answerPreCheckout(id, ok = true).map(_ => ())
      case _ => pure(())
    val api = FakeApi { case "sendMessage" => FakeApi.ok("""{"message_id":1}""") }
    Async.run[Unit, Pure](answer(Bot(api, "t"))(Update.Message(1, 5, 5, 1, "hi"))).runWith
    assertEquals(Js.str(api.of("sendMessage").head, "text"), "you said: hi")
  }
