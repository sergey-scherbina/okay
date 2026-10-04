package okay2.refine

import okay2.codec.Json

/** the patterns, and a Dispatch table written with them — at the top level, as a table is an `object` */
object RefineMatchFixtures {
  import Refine.json._

  val trade: Refine[Json, Json] = at("dataDocument", "trade")
  val swap: Refine[Json, (String, Double)] = (field("swap") >>> field("currency") >>> str) and (field("swap") >>> field("notional") >>> num)
  val fxForward: Refine[Json, String] = field("fxForward") >>> field("pair") >>> str

  object Desk extends Dispatch(Refine.id[Json]) {
    // a lane is checked by its CLASS (a ClassTag): `lane[(String, Double)]` would check only `Tuple2`
    val eur = lane[Double]("swaps/eur")
    val other = lane[String]("swaps/other")
    val fxs = lane[String]("fx")
    def table(j: Json): To = j match {
      case trade(swap((ccy, n))) if ccy == "EUR" => eur(n)
      case trade(swap((ccy, _))) => other(ccy)
      case trade(fxForward(pair)) => fxs(pair)
      case _ => unrouted("not a desk document")
    }
  }
}

/** okay's refine-match on the Scala 2 core: a pattern is an extractor in a plain `match` */
class TestRefineMatch extends munit.FunSuite {
  import RefineMatchFixtures._

  def route(j: Json): String = j match {
    case trade(swap((ccy, n))) if ccy == "EUR" => s"eur swap of ${n.toLong}"   // a Double prints "5.0" on the JVM, "5" on JS
    case trade(swap((ccy, _))) => s"$ccy swap"
    case trade(fxForward(pair)) => s"fx $pair"
    case _ => "unknown"
  }

  test("a pattern is a case: nested extractors are a path, guards work") {
    assertEquals(route(Json.parse("""{"dataDocument": {"trade": {"swap": {"currency": "EUR", "notional": 5}}}}""")), "eur swap of 5")
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

  test("the same patterns route documents to lanes in a Dispatch table (every platform)") {
    val doc = (s: String) => Json.parse(s)
    val eurSwap = doc("""{"dataDocument": {"trade": {"swap": {"currency": "EUR", "notional": 5}}}}""")
    val usdSwap = doc("""{"dataDocument": {"trade": {"swap": {"currency": "USD", "notional": 7}}}}""")
    val fx = doc("""{"dataDocument": {"trade": {"fxForward": {"pair": "EURUSD"}}}}""")
    val letter = doc("""{"letter": "hello"}""")
    val out = Desk.split(Vector(eurSwap, usdSwap, fx, letter))
    assertEquals(out(Desk.eur), Vector(5.0))
    assertEquals(out(Desk.other), Vector("USD"))
    assertEquals(out(Desk.fxs), Vector("EURUSD"))
    assertEquals(out.rejected.map(_.why), Vector("not a desk document"))
    assertEquals(out.counts.under("swaps"), 2)
  }
}
