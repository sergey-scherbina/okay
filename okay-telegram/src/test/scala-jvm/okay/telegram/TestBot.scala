package okay.telegram

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Http, Request, Response}
import okay.ui.Telegram.Key

/**
 * A RECORDING TRANSPORT: what the Bot API would be asked, and what it
 * answers — so every claim of specs/telegram-bot.md is checked with no
 * network and no token that is real.
 */
final class FakeApi(answers: PartialFunction[String, String]) extends Http:
  val calls = scala.collection.mutable.ListBuffer.empty[(String, Json, Request)]
  def method(r: Request): String = r.url.substring(r.url.lastIndexOf('/') + 1)
  def send(r: Request): Response ! Async = async {
    val m = method(r)
    calls += ((m, Json.parse(new String(r.body.bytes, "UTF-8")), r))
    val (status, body) = answers.lift(m).fold(500 -> "<html>bad gateway</html>")(200 -> _)
    Response(status, Nil, Http.one(body.getBytes("UTF-8")))
  }
  def of(m: String): Vector[Json] = calls.toVector.collect { case (`m`, j, _) => j }

object FakeApi:
  def ok(result: String): String = s"""{"ok":true,"result":$result}"""

class TestBot extends munit.FunSuite:
  def run[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  val token = "123:SECRET-TOKEN"

  test("A CALL IS A POST OF JSON, and the API's refusal is a value that never carries the token") {
    val api = FakeApi {
      case "getMe" => FakeApi.ok("""{"id":1,"username":"okay_bot"}""")
      case "sendMessage" => """{"ok":false,"error_code":403,"description":"Forbidden: bot was blocked by the user"}"""
    }
    val bot = Bot(api, token)
    assertEquals(run(bot.getMe).map(Js.str(_, "username")), Right("okay_bot"))
    val (_, _, req) = api.calls.head
    assertEquals(req.url, s"https://api.telegram.org/bot$token/getMe")
    assert(req.headers.exists((k, v) => k == "content-type" && v == "application/json"), req.headers.toString)
    val refused = run(bot.send(7, "hi"))
    assertEquals(refused, Left(Refused("sendMessage", 403, "Forbidden: bot was blocked by the user")))
    assert(!refused.toString.contains("SECRET"), "the method is named, the URL is not")
    // a transport that answers something else entirely is a refusal too, not a throw
    assertEquals(run(bot.call("getChat")), Left(Refused("getChat", 500, "<html>bad gateway</html>")))
  }

  test("poll answers the offset after the highest update seen — the given one when the round was empty") {
    var rounds = 0
    val api = FakeApi { case "getUpdates" =>
      rounds += 1
      if rounds == 1 then FakeApi.ok("""[{"update_id":10,"message":{"message_id":1,"chat":{"id":5},"from":{"id":5},"text":"a"}},
        {"update_id":11,"message":{"message_id":2,"chat":{"id":5},"from":{"id":5},"text":"b"}}]""")
      else FakeApi.ok("[]")
    }
    val bot = Bot(api, token)
    val seen = scala.collection.mutable.ListBuffer.empty[String]
    val next = run(bot.poll(3, u => async { seen += u.toString }))
    assertEquals(next, Right(12L))
    assertEquals(seen.size, 2)
    assertEquals(run(bot.poll(12, _ => pure(()))), Right(12L))
    val asked = api.of("getUpdates").head
    assertEquals(Js.long(asked, "offset"), 3L); assertEquals(Js.long(asked, "timeout"), 25L)
  }

  test("A LOOP THAT CANNOT POLL SAYS SO, and waits as long as the API asked — the two silent deaths of a live bot") {
    var rounds = 0
    val api = FakeApi { case "getUpdates" =>
      rounds += 1
      if rounds == 1 then """{"ok":false,"error_code":409,"description":"Conflict: terminated by other getUpdates request"}"""
      else if rounds == 2 then """{"ok":false,"error_code":429,"description":"Too Many Requests: retry after 3","parameters":{"retry_after":3}}"""
      else if rounds == 3 then """{"ok":false,"error_code":401,"description":"Unauthorized"}"""
      else FakeApi.ok("[]")
    }
    val told = scala.collection.mutable.ListBuffer.empty[Refused]
    val slept = scala.collection.mutable.ListBuffer.empty[Long]
    given Timer = new Timer:
      def after(ms: Long)(k: () => Unit) = { slept += ms; k(); () => () }
    run(Bot(api, token).serve(_ => pure(()), from = 0, retryMs = 2000,
      stop = () => rounds >= 4, onRefused = r => async { told += r }))
    // every refusal reached the consumer, with the code that says which it was
    assertEquals(told.map(_.code).toList, List(409, 429, 401))
    assert(told.exists(_.description.contains("terminated by other getUpdates")), told.toString)
    assertEquals(told.find(_.code == 429).flatMap(_.retryAfter), Some(3), "the API's own retry_after, read")
    assertEquals(told.find(_.code == 401).exists(_.fatal), true, "a wrong token is not something waiting fixes")
    // and it waited what the API asked where the API asked: 3s, not our 2s
    assertEquals(slept.toList, List(2000L, 3000L, 2000L))
  }

  test("serve keeps the offset across a refused round and stops when told") {
    var rounds = 0
    val api = FakeApi { case "getUpdates" =>
      rounds += 1
      if rounds == 1 then FakeApi.ok("""[{"update_id":20,"message":{"message_id":1,"chat":{"id":5},"from":{"id":5},"text":"a"}}]""")
      else if rounds == 2 then """{"ok":false,"error_code":429,"description":"Too Many Requests"}"""
      else FakeApi.ok("[]")
    }
    val resume = run(Bot(api, token).serve(_ => pure(()), from = 0, retryMs = 1, stop = () => rounds >= 3))
    assertEquals(resume, 21L)
    assertEquals(api.of("getUpdates").map(Js.long(_, "offset")), Vector(0L, 21L, 21L), "the refused round asked again with the same offset")
  }

  test("send carries the keyboard, HTML and force_reply as the API spells them, and answers the message id") {
    val api = FakeApi { case "sendMessage" => FakeApi.ok("""{"message_id":77}""") ; case "editMessageText" => FakeApi.ok("true") }
    val bot = Bot(api, token)
    val id = run(bot.send(5, "<b>hi</b>", Vector(Vector(Key.Press("+", "f1.0"), Key.Press("-", "f1.1")), Vector(Key.Open("site", "https://x.y")))))
    assertEquals(id, Right(77L))
    val body = api.of("sendMessage").head
    assertEquals(Js.str(body, "parse_mode"), "HTML")
    assertEquals(Js.field(body, "reply_markup"), Some(Json.parse(
      """{"inline_keyboard":[[{"text":"+","callback_data":"f1.0"},{"text":"-","callback_data":"f1.1"}],[{"text":"site","url":"https://x.y"}]]}""")))
    assertEquals(run(bot.send(5, "Name?", forceReply = true)), Right(77L))   // the fake answers one id
    assertEquals(Js.field(api.of("sendMessage")(1), "reply_markup"), Some(Json.parse("""{"force_reply":true}""")))
    assertEquals(run(bot.edit(5, 77, "hi again")), Right(()))
    assertEquals(Js.long(api.of("editMessageText").head, "message_id"), 77L)
  }

  test("STARS: an invoice in XTR with no provider, the pre-checkout answer, the refund") {
    val api = FakeApi { case "sendInvoice" => FakeApi.ok("""{"message_id":9}"""); case "answerPreCheckoutQuery" => FakeApi.ok("true")
      case "refundStarPayment" => FakeApi.ok("true") }
    val bot = Bot(api, token)
    assertEquals(run(bot.invoice(5, "Check Pass", "30 days of unlimited checks", "check-pass-30", 250, "Check Pass, 30 days")), Right(9L))
    val inv = api.of("sendInvoice").head
    assertEquals(Js.str(inv, "currency"), "XTR")
    assertEquals(Js.field(inv, "provider_token"), None,
      "OMITTED for Stars, not empty: the Bot API changelog says must be omitted")
    assertEquals(Js.field(inv, "prices"), Some(Json.parse("""[{"label":"Check Pass, 30 days","amount":250}]""")))
    assertEquals(run(bot.answerPreCheckout("q-9", ok = false, "sold out")), Right(()))
    val pre = api.of("answerPreCheckoutQuery").head
    assertEquals(Js.bool(pre, "ok"), false); assertEquals(Js.str(pre, "error_message"), "sold out")
    assertEquals(run(bot.refundStars(42, "tpc-1")), Right(()))
    assertEquals(Js.str(api.of("refundStarPayment").head, "telegram_payment_charge_id"), "tpc-1")
  }
