package okay.py

import java.nio.file.{Files, Path}
import okay.{Reader, given}

object TestHaskellTypedEffects:
  /** the effect a Haskell program may perform, written ONCE, here, with its types */
  val priceOf = Py.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  val discount = Py.callback[Double, Double]("discount")(amount => Reader.ask[Map[String, Double]].map(m => amount * m("rate")))

  // no margin: the docs quote these lines
  val main: String = """{-# LANGUAGE DataKinds #-}
module Main (main) where

import Okay
import OkayEff
import Shop

total :: String -> Integer -> Eff '[Shop] Double
total sku qty = do
  price <- send (PriceOf sku)
  send (Discount (price * fromInteger qty))

main :: IO ()
main = serve [("total", \[sku, qty] -> program (total (fromValue sku) (fromValue qty)))]
"""

/** hs-typed-effects against a LIVE GHC: a Haskell program's effects in its type */
class TestHaskellTypedEffects extends munit.FunSuite {
  import TestHaskellTypedEffects.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val ghc = scala.util.Try(ProcessBuilder("ghc", "--version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !ghc

  private def project(source: String): Path =
    val dir = Files.createTempDirectory("okay-hs-typed")
    Files.writeString(dir.resolve("Shop.hs"), Hs.ops("Shop", Py.callbacks(priceOf, discount))): Unit
    Files.writeString(dir.resolve("Main.hs"), source): Unit
    dir

  private lazy val w = PySubprocess.speaking(Seq(HaskellWorker.build(project(main)).toString))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if ghc then w.close()

  test("the effect's Haskell module is written from the Scala callbacks") {
    val src = Hs.ops("Shop", Py.callbacks(priceOf, discount))
    assert(src.contains("data Shop a where\n  PriceOf :: String -> Shop Double\n  Discount :: Double -> Shop Double"), src)
    assert(src.contains("""  request (PriceOf a0) = ("price_of", [toValue a0])"""), src)
  }

  test("a program typed by its effects runs in the worker, each operation a Scala callback") {
    val run = Py.program[Double]("total").calling(Py.callbacks(priceOf, discount))("tea", 3L)
    assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(run.program).runWith, Right(6.0))
  }

  test("GHC refuses an effect the program did not declare, and names it") {
    val undeclared = main.replace("Eff '[Shop] Double", "Eff '[] Double")
    val e = intercept[IllegalStateException](HaskellWorker.build(project(undeclared)))
    assert(e.getMessage.contains("this program does not declare the effect Shop"), e.getMessage)
  }

  test("GHC refuses an argument of the wrong type") {
    val wrong = main.replace("send (PriceOf sku)", "send (PriceOf qty)")
    val e = intercept[IllegalStateException](HaskellWorker.build(project(wrong)))
    assert(e.getMessage.contains("Couldn't match type"), e.getMessage)
  }
}
