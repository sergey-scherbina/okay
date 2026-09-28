package okay.telegram

import okay.codec.Json

/** specs/telegram-bot.md — `Update.parse` is total over the API's shape */
class TestUpdate extends munit.FunSuite:

  val message = Json.parse("""{"update_id":10,"message":{"message_id":5,"from":{"id":42,"is_bot":false,"first_name":"Ann"},
    "chat":{"id":-100,"type":"group"},"date":1790600000,"text":"bc1qxyz"}}""")
  val callback = Json.parse("""{"update_id":11,"callback_query":{"id":"cb-1","from":{"id":42},"message":{"message_id":7,
    "chat":{"id":42,"type":"private"}},"data":"f1.0"}}""")
  val preCheckout = Json.parse("""{"update_id":12,"pre_checkout_query":{"id":"q-9","from":{"id":42},"currency":"XTR",
    "total_amount":250,"invoice_payload":"check-pass-30"}}""")
  val paid = Json.parse("""{"update_id":13,"message":{"message_id":8,"from":{"id":42},"chat":{"id":42,"type":"private"},
    "successful_payment":{"currency":"XTR","total_amount":250,"invoice_payload":"check-pass-30",
    "telegram_payment_charge_id":"tpc-1","provider_payment_charge_id":"ppc-1"}}}""")

  test("a text message, a press, a pre-checkout and a payment become their case") {
    assertEquals(Update.parse(message), Update.Message(10, -100, 42, 5, "bc1qxyz"))
    assertEquals(Update.parse(callback), Update.Callback(11, 42, 42, 7, "f1.0", "cb-1"))
    assertEquals(Update.parse(preCheckout), Update.PreCheckout(12, 42, "q-9", "check-pass-30", "XTR", 250))
    assertEquals(Update.parse(paid), Update.Paid(13, 42, 42, "check-pass-30", "XTR", 250, "tpc-1", "ppc-1"))
  }

  test("EVERY OTHER KIND IS NAMED, NOT DROPPED: an edit, a join, a photo") {
    assertEquals(Update.parse(Json.parse("""{"update_id":14,"edited_message":{"message_id":5,"text":"x"}}""")),
      Update.Other(14, "edited_message"))
    assertEquals(Update.parse(Json.parse("""{"update_id":15,"my_chat_member":{}}""")), Update.Other(15, "my_chat_member"))
    // a message with no text (a photo, a sticker) is not a Message("")
    assertEquals(Update.parse(Json.parse("""{"update_id":16,"message":{"message_id":9,"chat":{"id":1},"from":{"id":2},"photo":[]}}""")),
      Update.Other(16, "message"))
    assertEquals(Update.parse(Json.parse("""not even json""")).updateId, 0L, "damaged input is a value too")
  }
