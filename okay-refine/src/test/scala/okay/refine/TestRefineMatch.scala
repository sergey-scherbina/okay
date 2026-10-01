package okay.refine

import okay.codec.Json
import okay.testkit.Munit.Diagnosed

/** specs/refine.md, refine-match: a pattern is an extractor in a plain `match` */
class TestRefineMatch extends Diagnosed:
  import Refine.json.*

  // a little document vocabulary, as a domain would write it
  val trade: Refine[Json, Json] = at("dataDocument", "trade")
  val swap: Refine[Json, (String, Double)] = (field("swap") >>> field("currency") >>> str) and (field("swap") >>> field("notional") >>> num)
  val fxForward: Refine[Json, String] = field("fxForward") >>> field("pair") >>> str
  val notADocument: Refine[Json, Json] = field("letter")

  def doc(s: String): Json = Json.parse(s)
  val eurSwap = doc("""{"dataDocument": {"trade": {"swap": {"currency": "EUR", "notional": 5}}}}""")
  val usdSwap = doc("""{"dataDocument": {"trade": {"swap": {"currency": "USD", "notional": 7}}}}""")
  val fx = doc("""{"dataDocument": {"trade": {"fxForward": {"pair": "EURUSD"}}}}""")
  val letter = doc("""{"letter": "hello"}""")

  /** recognition and routing in ONE construct of the language: nested extractors are paths, guards are guards */
  def route(j: Json): String = j match
    case trade(swap((ccy, n))) if ccy == "EUR" => s"eur swap of ${n.toLong}"   // a Double prints "5.0" on the JVM, "5" on JS
    case trade(swap((ccy, _))) => s"$ccy swap"
    case trade(fxForward(pair)) => s"fx $pair"
    case notADocument(_) => "a letter"
    case _ => "unknown"

  test("a pattern is a case: nested extractors are a path, guards work, the bound value is the pattern's value") {
    assertEquals(route(eurSwap), "eur swap of 5")
    assertEquals(route(usdSwap), "USD swap")
    assertEquals(route(fx), "fx EURUSD")
    assertEquals(route(letter), "a letter")
    assertEquals(route(doc("""{"dataDocument": {"trade": {}}}""")), "unknown")
  }

  test("an Unclear reading matches NO case: a match never takes one reading of an ambiguous document by accident") {
    val even = Refine.step[Int, Int]("even")(n => if n % 2 == 0 then Right(n) else Left("odd"))(identity)
    val small = Refine.step[Int, Int]("small")(n => if n < 10 then Right(n) else Left("big"))(identity)
    val both = even or small
    def which(n: Int): String = n match
      case both(k) => s"one reading: $k"
      case _ => "none, or more than one"
    assertEquals(which(12), "one reading: 12")
    assertEquals(which(7), "one reading: 7")
    assertEquals(which(4), "none, or more than one")
    assert(both.run(4) match { case Verdict.Unclear(_, _) => true; case _ => false }, "and run says which it was")
  }

  test("the same patterns route documents to lanes in a Dispatch table") {
    object Desk extends Dispatch(Refine.id[Json]):
      // a lane's type must be checkable at run time: `lane[(String, Double)]` is warned unchecked (E092)
      val eur = lane[Double]("swaps/eur")
      val other = lane[String]("swaps/other")
      val fxs = lane[String]("fx")
      def table(j: Json): To = j match
        case trade(swap((ccy, n))) if ccy == "EUR" => eur(n)
        case trade(swap((ccy, _))) => other(ccy)
        case trade(fxForward(pair)) => fxs(pair)
        case _ => unrouted("not a desk document")
    val out = Desk.split(Vector(eurSwap, usdSwap, fx, letter))
    assertEquals(out(Desk.eur), Vector(5.0))
    assertEquals(out(Desk.other), Vector("USD"))
    assertEquals(out(Desk.fxs), Vector("EURUSD"))
    assertEquals(out.rejected.map(_.why), Vector("not a desk document"))
  }
