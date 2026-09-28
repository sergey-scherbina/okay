package okay.telegram

import okay.codec.Json
import okay.codec.Json.*

/**
 * WHAT THE CHAT SAID, as a value (specs/telegram-bot.md).
 *
 * The Bot API delivers an update as an object with `update_id` and ONE
 * more key naming its kind — `message`, `callback_query`,
 * `pre_checkout_query`, `edited_message`, … — and this parse is total
 * over that shape: the kinds the okay consumers act on become their
 * case, every other kind is `Other(kind)`, named so a consumer can count
 * what it ignores rather than wonder where an update went.
 */
enum Update:
  /** a text message in a chat (`from` is the person, `chat` the room) */
  case Message(updateId: Long, chat: Long, from: Long, messageId: Long, text: String)
  /** a press on an inline button: `data` is the button's, `callbackId` must be answered */
  case Callback(updateId: Long, chat: Long, from: Long, messageId: Long, data: String, callbackId: String)
  /** the last question before a payment is taken — answered within 10 s or the payment fails */
  case PreCheckout(updateId: Long, from: Long, queryId: String, payload: String, currency: String, total: Long)
  /** a payment taken: `chargeId` is what a refund names */
  case Paid(updateId: Long, chat: Long, from: Long, payload: String, currency: String, total: Long,
            chargeId: String, providerChargeId: String)
  /** every other kind of update, by the API's own key */
  case Other(updateId: Long, kind: String)

  def updateId: Long

object Update:

  def parse(u: Json): Update =
    val id = Js.long(u, "update_id")
    val kind = u match
      case JObj(fs) => fs.map(_._1).find(_ != "update_id").getOrElse("")
      case _ => ""
    kind match
      case "message" =>
        val m = Js.field(u, "message").getOrElse(JNull)
        val chat = Js.long(Js.field(m, "chat").getOrElse(JNull), "id")
        val from = Js.long(Js.field(m, "from").getOrElse(JNull), "id")
        val mid = Js.long(m, "message_id")
        Js.field(m, "successful_payment") match
          case Some(p) =>
            Paid(id, chat, from, Js.str(p, "invoice_payload"), Js.str(p, "currency"), Js.long(p, "total_amount"),
              Js.str(p, "telegram_payment_charge_id"), Js.str(p, "provider_payment_charge_id"))
          case None => Js.field(m, "text") match
            case Some(JStr(t)) => Message(id, chat, from, mid, t)
            case _ => Other(id, "message")
      case "callback_query" =>
        val c = Js.field(u, "callback_query").getOrElse(JNull)
        val m = Js.field(c, "message").getOrElse(JNull)
        Callback(id, Js.long(Js.field(m, "chat").getOrElse(JNull), "id"), Js.long(Js.field(c, "from").getOrElse(JNull), "id"),
          Js.long(m, "message_id"), Js.str(c, "data"), Js.str(c, "id"))
      case "pre_checkout_query" =>
        val q = Js.field(u, "pre_checkout_query").getOrElse(JNull)
        PreCheckout(id, Js.long(Js.field(q, "from").getOrElse(JNull), "id"), Js.str(q, "id"),
          Js.str(q, "invoice_payload"), Js.str(q, "currency"), Js.long(q, "total_amount"))
      case k => Other(id, k)

/** reading the API's JSON: absent is empty, never a throw */
private[telegram] object Js:
  def field(j: Json, k: String): Option[Json] = j match
    case JObj(fs) => fs.collectFirst { case (`k`, v) => v }
    case _ => None
  def str(j: Json, k: String): String = field(j, k) match
    case Some(JStr(s)) => s
    case _ => ""
  def long(j: Json, k: String): Long = field(j, k) match
    case Some(JNum(n)) => n.toLong
    case Some(JStr(s)) => s.toLongOption.getOrElse(0L)
    case _ => 0L
  def bool(j: Json, k: String): Boolean = field(j, k).contains(JBool(true))
  def arr(j: Json, k: String): Vector[Json] = field(j, k) match
    case Some(JArr(vs)) => vs
    case _ => Vector.empty
