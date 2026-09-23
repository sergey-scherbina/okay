package okay.py

import okay.{Reader, given}
import okay.codec.Schema

object TestPyShop:
  final case class Order(sku: String, qty: Long, country: String) derives Schema
  final case class Catalog(prices: Map[String, Double], taxes: Map[String, Double])

  val shop = Py.module("shop", """
    import okay

    def quote(order):
        price = okay.call("price_of", order["sku"])
        tax = okay.call("tax_rate", order["country"])
        return price * order["qty"] * (1 + tax)

    def typo(order):
        return okay.call("prices_of", order["sku"])
  """)

/**
 * The docs' example of callbacks by NAME (docs/python-and-r.md): Python asks
 * okay for a price and a tax rate, and okay answers from its own Reader.
 */
class TestPyShop extends munit.FunSuite {
  import TestPyShop.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val w = PySubprocess.start(TestPy.python.get, modules = Seq(shop))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  private val catalog = Catalog(Map("tea" -> 4.0), Map("UA" -> 0.2))

  test("Python asks okay by name; okay answers from its Reader") {
    val priceOf = Py.callback[String, Double]("price_of")(sku => Reader.ask[Catalog].map(_.prices(sku)))
    val taxRate = Py.callback[String, Double]("tax_rate")(country => Reader.ask[Catalog].map(_.taxes(country)))
    val quote = shop.fn[Double]("quote").calling(Py.callbacks(priceOf, taxRate))
    val answer = Reader.run(catalog)(quote(Order("tea", 2L, "UA"))).runWith
    assert(answer.exists(a => math.abs(a - 9.6) < 1e-9), s"$answer")
  }

  test("a name okay did not offer to this call is refused in Python, listing what was offered") {
    val priceOf = Py.callback[String, Double]("price_of")(sku => Reader.ask[Catalog].map(_.prices(sku)))
    val typo = shop.fn[Double]("typo").calling(Py.callbacks(priceOf))
    val refused = Reader.run(catalog)(typo(Order("tea", 2L, "UA"))).runWith
    assertEquals(refused, Left(Condition("LookupError", "okay.call('prices_of'): this call was offered ['price_of']")))
  }
}
