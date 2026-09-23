package okay.r

import okay.{Reader, given}
import okay.codec.Schema

object TestRShop:
  final case class Order(sku: String, qty: Long, country: String) derives Schema
  final case class Catalog(prices: Map[String, Double], taxes: Map[String, Double])

  val shop = R.module("shop", """
    quote <- function(order) {
      price <- okay_call("price_of", order$sku)
      tax <- okay_call("tax_rate", order$country)
      price * order$qty * (1 + tax)
    }
  """)

/** the docs' callback example in R (docs/python-and-r.md) */
class TestRShop extends munit.FunSuite {
  import TestRShop.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  private lazy val r = RSubprocess.start(rscript = TestR.rscript.get, modules = Seq(shop))
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  test("R asks okay by name; okay answers from its Reader") {
    val priceOf = R.callback[String, Double]("price_of")(sku => Reader.ask[Catalog].map(_.prices(sku)))
    val taxRate = R.callback[String, Double]("tax_rate")(country => Reader.ask[Catalog].map(_.taxes(country)))
    val quote = shop.fn[Double]("quote").calling(R.callbacks(priceOf, taxRate))
    val answer = Reader.run(Catalog(Map("tea" -> 4.0), Map("UA" -> 0.2)))(quote(Order("tea", 2L, "UA"))).runWith
    assert(answer.exists(a => math.abs(a - 9.6) < 1e-9), s"$answer")
  }
}
