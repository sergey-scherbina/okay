package okay.telegram

import okay.*
import okay.codec.Json
import okay.codec.Json.*
import okay.http.{Body, Http, Request, Response}
import okay.ui.Telegram.Key

/** one call refused, as the API said it: the method, its error code, its
 * words — and never the URL, which carries the token */
final case class Refused(method: String, code: Int, description: String)

/**
 * THE BOT API OVER okay-http (specs/telegram-bot.md).
 *
 * `call` is the whole transport — a POST of JSON, the answer read whole
 * — and the typed methods are thin: each builds one object and reads two
 * or three fields of the result. The untyped `call` stays public because
 * the Bot API grows monthly and a client that names every method is
 * stale on release.
 *
 * A refusal is a VALUE (`Left(Refused)`), as okay-http's 4xx is a
 * status: nothing here throws for what the API said, and nothing retries
 * — okay-resilience is where retries live, and the poll's own retry is
 * `serve`'s, stated there.
 */
final class Bot(http: Http, token: String, base: String = "https://api.telegram.org"):

  def call(method: String, params: Json = JObj(Vector.empty)): Either[Refused, Json] ! Async =
    val req = Request.post(s"$base/bot$token/$method", Body.Text(Json.print(params)),
      Seq("content-type" -> "application/json"))
    http.send(req).flatMap { res =>
      Http.bytes(res).map { bs =>
        val text = new String(bs.toArray, "UTF-8")
        Json.parse(text) match
          case j @ JObj(_) if Js.bool(j, "ok") => Right(Js.field(j, "result").getOrElse(JNull))
          case j @ JObj(_) if Js.field(j, "ok").isDefined =>
            Left(Refused(method, Js.long(j, "error_code").toInt, Js.str(j, "description")))
          case _ => Left(Refused(method, res.status, text.take(200)))
      }
    }

  def getMe: Either[Refused, Json] ! Async = call("getMe")

  def getUpdates(offset: Long, timeoutSeconds: Int = 25): Either[Refused, Vector[Update]] ! Async =
    call("getUpdates", obj("offset" -> JNum(offset.toDouble), "timeout" -> JNum(timeoutSeconds.toDouble)))
      .map(_.map { case JArr(us) => us.map(Update.parse); case _ => Vector.empty })

  /** one round: every update to `handle`, in order; the offset to ask
   * with next — after the highest `update_id` seen, or the one given
   * when the round was empty or refused */
  def poll(offset: Long, handle: Update => Unit ! Async, timeoutSeconds: Int = 25): Either[Refused, Long] ! Async =
    getUpdates(offset, timeoutSeconds).flatMap {
      case Left(r) => pure(Left(r))
      case Right(us) =>
        okay.!.each(us)(handle).map(_ => Right(us.map(_.updateId + 1).foldLeft(offset)(_ max _)))
    }

  /** the loop: from `from`, until `stop` says so; a refused poll waits
   * `retryMs` and asks again with the SAME offset. Answers the offset to
   * resume from. */
  def serve(handle: Update => Unit ! Async, from: Long = 0, retryMs: Long = 2000,
            stop: () => Boolean = () => false, timeoutSeconds: Int = 25)(using Timer): Long ! Async =
    def loop(offset: Long): Long ! Async =
      if stop() then pure(offset)
      else poll(offset, handle, timeoutSeconds).flatMap {
        case Right(next) => loop(next)
        case Left(_) => Async.sleep(retryMs).flatMap(_ => loop(offset))
      }
    loop(from)

  /** a message; answers its id */
  def send(chat: Long, text: String, keyboard: Vector[Vector[Key]] = Vector.empty,
           html: Boolean = true, forceReply: Boolean = false): Either[Refused, Long] ! Async =
    val fields = Vector("chat_id" -> JNum(chat.toDouble), "text" -> JStr(text)) ++
      (if html then Vector("parse_mode" -> JStr("HTML")) else Vector.empty) ++
      (if forceReply then Vector("reply_markup" -> obj("force_reply" -> JBool(true)))
       else if keyboard.nonEmpty then Vector("reply_markup" -> Bot.markup(keyboard)) else Vector.empty)
    call("sendMessage", JObj(fields)).map(_.map(m => Js.long(m, "message_id")))

  def edit(chat: Long, messageId: Long, text: String, keyboard: Vector[Vector[Key]] = Vector.empty,
           html: Boolean = true): Either[Refused, Unit] ! Async =
    val fields = Vector("chat_id" -> JNum(chat.toDouble), "message_id" -> JNum(messageId.toDouble), "text" -> JStr(text)) ++
      (if html then Vector("parse_mode" -> JStr("HTML")) else Vector.empty) ++
      (if keyboard.nonEmpty then Vector("reply_markup" -> Bot.markup(keyboard)) else Vector.empty)
    call("editMessageText", JObj(fields)).map(_.map(_ => ()))

  /** every press is answered, or the person's client spins */
  def answerCallback(callbackId: String, notice: String = ""): Either[Refused, Unit] ! Async =
    call("answerCallbackQuery", JObj(Vector("callback_query_id" -> JStr(callbackId)) ++
      (if notice.nonEmpty then Vector("text" -> JStr(notice)) else Vector.empty))).map(_.map(_ => ()))

  /** the `/command` menu: (command, description) */
  def setCommands(commands: Vector[(String, String)]): Either[Refused, Unit] ! Async =
    call("setMyCommands", obj("commands" -> JArr(commands.map((c, d) =>
      obj("command" -> JStr(c), "description" -> JStr(d)))))).map(_.map(_ => ()))

  // ---- Telegram Stars -----------------------------------------------------

  /** an invoice in Stars (`XTR`): no provider, one price line. Telegram's
   * terms allow Stars for digital goods only — the consumer's rule to
   * keep, this module's to state. Answers the invoice message's id. */
  def invoice(chat: Long, title: String, description: String, payload: String,
              stars: Long, label: String): Either[Refused, Long] ! Async =
    call("sendInvoice", obj("chat_id" -> JNum(chat.toDouble), "title" -> JStr(title),
      "description" -> JStr(description), "payload" -> JStr(payload), "provider_token" -> JStr(""),
      "currency" -> JStr("XTR"),
      "prices" -> JArr(Vector(obj("label" -> JStr(label), "amount" -> JNum(stars.toDouble))))))
      .map(_.map(m => Js.long(m, "message_id")))

  /** the pre-checkout answer, within 10 s of the query */
  def answerPreCheckout(queryId: String, ok: Boolean, error: String = ""): Either[Refused, Unit] ! Async =
    call("answerPreCheckoutQuery", JObj(Vector("pre_checkout_query_id" -> JStr(queryId), "ok" -> JBool(ok)) ++
      (if !ok && error.nonEmpty then Vector("error_message" -> JStr(error)) else Vector.empty))).map(_.map(_ => ()))

  def refundStars(user: Long, chargeId: String): Either[Refused, Unit] ! Async =
    call("refundStarPayment", obj("user_id" -> JNum(user.toDouble), "telegram_payment_charge_id" -> JStr(chargeId)))
      .map(_.map(_ => ()))

  private def obj(fs: (String, Json)*): Json = JObj(fs.toVector)

object Bot:
  /** the inline keyboard, row by row: a Press is a callback button, an Open a URL button */
  def markup(keyboard: Vector[Vector[Key]]): Json =
    JObj(Vector("inline_keyboard" -> JArr(keyboard.map(row => JArr(row.map {
      case Key.Press(label, data) => JObj(Vector("text" -> JStr(label), "callback_data" -> JStr(data)))
      case Key.Open(label, url) => JObj(Vector("text" -> JStr(label), "url" -> JStr(url)))
    })))))
