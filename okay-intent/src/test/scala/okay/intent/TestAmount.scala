package okay.intent

import okay.frame.{Frame, Found}

/** specs/intent-classify.md, intent-extract-amount */
class TestAmount extends munit.FunSuite {

  private def amt(v: String, c: String) = Amount(BigDecimal(v), c)

  test("the shapes, per language") {
    val cases = List(
      "$20" -> amt("20", "USD"), "20$" -> amt("20", "USD"), "$ 20" -> amt("20", "USD"), "20 dollars" -> amt("20", "USD"),
      "twenty dollars" -> amt("20", "USD"), "USD 20" -> amt("20", "USD"), "20 USD" -> amt("20", "USD"), "usd20" -> amt("20", "USD"),
      "€15.50" -> amt("15.50", "EUR"), "15,50 €" -> amt("15.50", "EUR"), "15.5 euros" -> amt("15.5", "EUR"),
      "1,000 dollars" -> amt("1000", "USD"), "1.000,50 €" -> amt("1000.50", "EUR"), "1,000.50 dollars" -> amt("1000.50", "USD"),
      "1 000 евро" -> amt("1000", "EUR"), "2.500 euro" -> amt("2500", "EUR"),
      "£30" -> amt("30", "GBP"), "30 GBP" -> amt("30", "GBP"), "two hundred euros" -> amt("200", "EUR"),
      "two thousand five hundred dollars" -> amt("2500", "USD"), "2 thousand dollars" -> amt("2000", "USD"), "5k dollars" -> amt("5000", "USD"),
      "twenty five bucks" -> amt("25", "USD"), "a hundred quid" -> amt("100", "GBP"),
      "vingt euros" -> amt("20", "EUR"), "deux cents euros" -> amt("200", "EUR"), "mille euros" -> amt("1000", "EUR"),
      "zwanzig Euro" -> amt("20", "EUR"), "zweihundert Euro" -> amt("200", "EUR"), "20 Dollar" -> amt("20", "USD"),
      "veinte euros" -> amt("20", "EUR"), "20 dólares" -> amt("20", "USD"), "doscientos euros" -> amt("200", "EUR"), "mil euros" -> amt("1000", "EUR"),
      "200 гривен" -> amt("200", "UAH"), "двести гривен" -> amt("200", "UAH"), "30 грн" -> amt("30", "UAH"), "500 руб" -> amt("500", "RUB"),
      "пятьсот рублей" -> amt("500", "RUB"), "20 евро" -> amt("20", "EUR"), "50 долларов" -> amt("50", "USD"), "две тысячи гривен" -> amt("2000", "UAH"),
      "1,5 тыс. грн" -> amt("1500", "UAH"),
      "200 гривень" -> amt("200", "UAH"), "двісті гривень" -> amt("200", "UAH"), "20 євро" -> amt("20", "EUR"), "50 доларів" -> amt("50", "USD"),
      "20 zł" -> amt("20", "PLN"), "20zł" -> amt("20", "PLN"), "dwadzieścia złotych" -> amt("20", "PLN"), "sto złotych" -> amt("100", "PLN"),
      "20 euro" -> amt("20", "EUR"), "50 dolarów" -> amt("50", "USD"), "1000 PLN" -> amt("1000", "PLN"),
      "3000円" -> amt("3000", "JPY"), "3,000円" -> amt("3000", "JPY"), "¥500" -> amt("500", "JPY"), "20ドル" -> amt("20", "USD"))
    val wrong = cases.filter((s, a) => !Amount.parse(s).contains(a)).map((s, a) => s"'$s' -> ${Amount.parse(s)} (expected $a)")
    assertEquals(wrong, Nil)
  }

  test("a number with no currency beside it is not an amount, and neither is a currency with no number") {
    for s <- List("20", "room 20", "at 20:30", "20 minutes", "4 people", "dollars", "the euro", "0 dollars",
                  "20 pounds of flour", "j'ai lu 20 livres", "", "twenty") do
      assertEquals(Amount.parse(s), None, s"'$s'")
  }

  test("the number nearest the currency wins, and the first amount in the sentence") {
    assertEquals(Amount.parse("2 rooms for 3 nights at 120 dollars"), Some(amt("120", "USD")))
    assertEquals(Amount.parse("€40 or 50 dollars"), Some(amt("40", "EUR")))
    assertEquals(Amount.parse("the 3 of us, 20 dollars each"), Some(amt("20", "USD")))
  }

  test("the evidence is the phrase, and a frame fills the amount from one sentence") {
    assertEquals(Amount.find("Can we keep it under 200 dollars for the room?"), Some(Found("200 dollars", amt("200", "USD"))))
    assertEquals(Amount.find("Budget: $1,500 total."), Some(Found("$1,500", amt("1500", "USD"))))
    val f = Frame.of("Booking", Slots.people, Slots.amount).fillFrom("A room for four people, up to 150 euros a night")
    assertEquals(f.valueOf(Slots.people), Some(4))
    assertEquals(f.valueOf(Slots.amount), Some(amt("150", "EUR")))
    assertEquals(f.remaining, 0)
    assertEquals(Slots.amount.show(amt("15.50", "EUR"), "en"), "15.5 EUR")
    assertEquals(Slots.amount.show(amt("1000", "USD"), "en"), "1000 USD")
  }
}
