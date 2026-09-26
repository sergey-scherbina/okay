package okay.clojure

import okay.{!, %, Reader}
import okay.foreign.{Foreign, Jvm, Shape}

/**
 * A Clojure program performs the caller's `Foreign.callbacks`
 * (foreign-one-ops, specs/foreign-one.md Decision 19): through
 * `okay.clojure.shop`, the namespace `okay.foreign.Jvm.clojure` writes from them.
 */
class TestClojureCallbacks extends munit.FunSuite:
  given Shape = Shape.python
  private val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  private val discount = Foreign.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))
  private val cbs = Foreign.callbacks(priceOf, discount)
  private val prices = Map("tea" -> 4.0, "rate" -> 0.5)

  private def fn(name: String) = Clj.fn("okay.clojure.quote", name).fold(e => fail(e), identity)

  test("the checked-in shop.clj is what the generator writes from the callbacks (regenerate, diff empty)") {
    val in = getClass.getResourceAsStream("/okay/clojure/shop.clj")
    assertEquals(Jvm.clojure("okay.clojure.shop", cbs), String(in.readAllBytes(), "UTF-8"))
  }

  test("a Clojure program performs the callbacks, each run under the caller's Reader") {
    val quote = Program.run[Reader % Map[String, Double], java.lang.Double](
      fn("quote-of").invoke("tea", Long.box(3L)), calls = Jvm.calls(cbs))
    assertEquals(!.run(Reader.run(prices)(quote)).doubleValue, 6.0)
  }

  test("a wrong argument is refused by name, by the callback's own Schema") {
    val wrong = Program.run[Reader % Map[String, Double], java.lang.Double](fn("wrong").invoke(), calls = Jvm.calls(cbs))
    val e = intercept[IllegalArgumentException](!.run(Reader.run(prices)(wrong)))
    assert(e.getMessage.contains("the callback 'price_of' refused its argument"), e.getMessage)
  }
