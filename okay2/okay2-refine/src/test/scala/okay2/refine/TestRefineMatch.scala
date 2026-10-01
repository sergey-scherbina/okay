package okay2.refine

import okay2.codec.Json

/** okay's refine-match on the Scala 2 core: a pattern is an extractor in a plain `match` */
class TestRefineMatch extends munit.FunSuite {
  import Refine.json._

  val trade: Refine[Json, Json] = at("dataDocument", "trade")
  val swap: Refine[Json, (String, Double)] = (field("swap") >>> field("currency") >>> str) and (field("swap") >>> field("notional") >>> num)
  val fxForward: Refine[Json, String] = field("fxForward") >>> field("pair") >>> str

  def route(j: Json): String = j match {
    case trade(swap((ccy, n))) if ccy == "EUR" => s"eur swap of $n"
    case trade(swap((ccy, _))) => s"$ccy swap"
    case trade(fxForward(pair)) => s"fx $pair"
    case _ => "unknown"
  }

  test("a pattern is a case: nested extractors are a path, guards work") {
    assertEquals(route(Json.parse("""{"dataDocument": {"trade": {"swap": {"currency": "EUR", "notional": 5}}}}""")), "eur swap of 5.0")
    assertEquals(route(Json.parse("""{"dataDocument": {"trade": {"swap": {"currency": "USD", "notional": 7}}}}""")), "USD swap")
    assertEquals(route(Json.parse("""{"dataDocument": {"trade": {"fxForward": {"pair": "EURUSD"}}}}""")), "fx EURUSD")
    assertEquals(route(Json.parse("""{"letter": "hello"}""")), "unknown")
  }

  test("an Unclear reading matches no case") {
    val even = Refine.step[Int, Int]("even")(n => if (n % 2 == 0) Right(n) else Left("odd"))(identity)
    val small = Refine.step[Int, Int]("small")(n => if (n < 10) Right(n) else Left("big"))(identity)
    val both = even or small
    def which(n: Int): String = n match {
      case both(k) => s"one reading: $k"
      case _ => "none, or more than one"
    }
    assertEquals(which(12), "one reading: 12")
    assertEquals(which(4), "none, or more than one")
  }
}
