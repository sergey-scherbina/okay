package okay.tsbrowser

import okay.*
import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import okay.tsbrowser.facades.okayPricing.mod as pricing

/**
 * A checkout written as okay programs over a TypeScript library
 * (polyglot-typescript stage 5). `okay.tsbrowser.facades.okayPricing` is not written by
 * hand: ScalablyTyped generated it from `okay-pricing`'s `index.d.ts`, so a
 * wrong argument, a missing field or a misspelt function is a Scala compile
 * error, and the library is called as the browser would call it.
 */
object Checkout:

  /** a catalogue price; the library's exception becomes a value */
  def price(sku: String): Either[String, Double] =
    try Right(pricing.priceOf(sku))
    catch case e: js.JavaScriptException => Left(e.getMessage)

  /** a cart built with the library's class, priced by the library's
   * function, less the discount the CALLER's Reader holds */
  def total(lines: List[(String, Int)]): Double ! Reader % Double =
    Reader.ask[Double].map { discount =>
      val cart = lines.foldLeft(new pricing.Cart())((c, l) => c.add(pricing.Item(l._2.toDouble, l._1)))
      cart.total(sku => pricing.priceOf(sku)) * (1 - discount)
    }

  /** a TypeScript Promise, awaited inside an okay program */
  def rate(currency: String): Double ! Async =
    Async.await[Double] { k =>
      pricing.fetchRate(currency).toFuture.onComplete(t => k(t.toEither))
      () => ()
    }
