package okay.tsbrowser

import okay.*
import okay.given
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** the TypeScript library, called from okay on Scala.js through its
 * generated facade: a function, a class with a callback, a Promise */
class TestCheckout extends munit.FunSuite:

  test("a TypeScript function through the generated facade; its exception is a value") {
    assertEquals(Checkout.price("tea"), Right(4.0))
    assert(Checkout.price("coffee").left.exists(_.contains("no price for coffee")), Checkout.price("coffee").toString)
  }

  test("a TypeScript class with a Scala callback, inside an okay program under the caller's Reader") {
    val t = !.run(Reader.run(0.5)(Checkout.total(List("tea" -> 3, "cake" -> 2))))
    assertEquals(t, 8.5)
  }

  test("a TypeScript Promise, awaited in an okay Async program") {
    Async.runAsync(Checkout.rate("UAH")).map(r => assertEquals(r, 45.0))
  }

  test("a rejected Promise is the program's failure") {
    Async.runAsync(Checkout.rate("XXX")).failed.map(e => assert(e.getMessage.contains("no rate for XXX"), e.getMessage))
  }

  test("the facade is TYPED from the library's .d.ts: a wrong argument does not compile") {
    val e = compileErrors("okay.tsbrowser.facades.okayPricing.mod.priceOf(42)")
    assert(e.contains("Found:    (42 : Int)") && e.contains("Required: String"), e)
    val f = compileErrors("okay.tsbrowser.facades.okayPricing.mod.Item(qty = 1.0)")
    assert(f.contains("sku"), f)
  }
