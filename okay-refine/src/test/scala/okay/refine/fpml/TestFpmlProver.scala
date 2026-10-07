package okay.refine.fpml

import okay.refine.{Format, Path, Refine, Verdict}
import okay.testkit.Munit.Diagnosed
import java.nio.charset.StandardCharsets.UTF_8

/** specs/refine.md §4: the public prover — bytes → xml → value → FpML → swap | forward */
class TestFpmlProver extends Diagnosed:

  val fromBytes: Refine[Array[Byte], Product] = Format.detect andThen Format.value andThen Fpml.instrument

  private def read(doc: String): Verdict[Product] =
    val v = fromBytes.run(doc.getBytes(UTF_8))
    note(v.toString)
    v

  test("ird-ex01: the vanilla swap is read, with its path, and the fx branch's refusal") {
    read(Samples.vanillaSwap) match
      case Verdict.Took(s: Fpml.Swap, by, declined) =>
        assertEquals(s, Fpml.Swap("SW2000", "1994-12-12", "EUR", 50000000.0, 0.06, "EUR-LIBOR-BBA", "1994-12-14", "1999-12-14"))
        assertEquals(by, Path("text", "xml", "value", "dataDocument", "trade", "swap"))
        assertEquals(declined.map(_.at).last, Path("text", "xml", "value", "dataDocument", "trade", "fxForward"))
        assertEquals(declined.last.reason, "no exchangedCurrency1 currency")
      case other => fail(s"expected the swap, got $other")
  }

  test("fx-ex03: the FX forward is read, and the swap branch says what it lacked") {
    read(Samples.fxForward) match
      case Verdict.Took(f: Fpml.FxForward, by, declined) =>
        assertEquals(f, Fpml.FxForward("ABN1234", "2001-11-19", "EUR", 10000000.0, "USD", 9175000.0, "2001-12-21", 0.9175))
        assertEquals(by, Path("text", "xml", "value", "dataDocument", "trade", "fxForward"))
        assertEquals(declined.find(_.at.steps.last == "swap").map(_.reason), Some("no field `swap`"))
      case other => fail(s"expected the forward, got $other")
  }

  test("neither: an XML document that is not FpML is declined at the root, by name") {
    val v = read("""<?xml version="1.0"?><trade><swap/></trade>""")
    assertEquals(v.reasons.find(_.at.steps.last == "dataDocument").map(_.reason), Some("root element is <trade>, not <dataDocument>"))
    val v2 = read("""<dataDocument><trade/></dataDocument>""")
    assertEquals(v2.reasons.find(_.at.steps.last == "dataDocument").map(_.reason), Some("a dataDocument without fpmlVersion"))
  }

  test("the way back: read(write(x)) == x for both, and the skeleton is FpML-shaped") {
    val s = Fpml.Swap("SW2000", "1994-12-12", "EUR", 50000000.0, 0.06, "EUR-LIBOR-BBA", "1994-12-14", "1999-12-14")
    val sj = Fpml.instrument.write(s)
    assert(sj.isRight, sj.toString)
    assertEquals(Fpml.instrument.run(sj.toOption.get).toOption, Some(s))
    val f = Fpml.FxForward("ABN1234", "2001-11-19", "EUR", 10000000.0, "USD", 9175000.0, "2001-12-21", 0.9175)
    val fj = Fpml.instrument.write(f)
    assertEquals(Fpml.instrument.run(fj.toOption.get).toOption, Some(f))
    // through the whole path the skeleton comes out as JSON bytes, and reads back by the json branch
    val bytes = fromBytes.write(f).toOption.get
    note(new String(bytes, UTF_8))
    fromBytes.run(bytes) match
      case Verdict.Took(x, by, _) =>
        assertEquals(x, f)
        assertEquals(by, Path("text", "json", "value", "dataDocument", "trade", "fxForward"))
      case other => fail(s"expected the forward back, got $other")
  }

  test("search: the two documents under Logic — if it is a swap then its legs, else the forward's pair") {
    import okay.freer.{!, pure}
    import okay.std.{Logic, runChoice}
    def describe(doc: String): Seq[String] =
      val j = (Format.detect andThen Format.value).run(doc.getBytes(UTF_8)).toOption.get
      !.run(runChoice[String, okay.freer.Pure](
        Logic.ifte[Fpml.Swap, String, okay.freer.Pure](Fpml.swap.search(Fpml.trade.run(j).toOption.get))(
          s => pure(s"swap ${s.fixedRate} vs ${s.floatingIndex}"))(
          Fpml.fxForward.search(Fpml.trade.run(j).toOption.get).map(f => s"forward ${f.currency1}/${f.currency2}"))))
    assertEquals(describe(Samples.vanillaSwap), Seq("swap 0.06 vs EUR-LIBOR-BBA"))
    assertEquals(describe(Samples.fxForward), Seq("forward EUR/USD"))
  }
