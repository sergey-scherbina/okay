package okay.frege

import okay.{!, %, Reader}
import okay.py.{Foreign, Jvm, Shape}

/** the caller's callbacks, declared once: the same two the wire
 * conformance suite offers Python, Go, Rust, Haskell, TypeScript and R */
object ShopCallbacks:
  given Shape = Shape.python
  val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  val discount = Foreign.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))
  val cbs = Foreign.callbacks(priceOf, discount)

/**
 * A Frege program performs the caller's `Foreign.callbacks` (foreign-one-ops,
 * specs/foreign-one.md Decision 19): through `Shop`, the module
 * `okay.py.Jvm.frege` writes from them, each operation typed by the
 * callback's Schemas.
 */
class TestFregeCallbacks extends munit.FunSuite:
  import ShopCallbacks.cbs

  private val prices = Map("tea" -> 4.0, "rate" -> 0.5)

  test("the checked-in Shop.fr is what the generator writes from the callbacks (regenerate, diff empty)") {
    // a forked suite runs in the module's directory, an unforked one in the build's
    val file = Seq("src/test/frege/okay/frege/Shop.fr", "okay-frege/src/test/frege/okay/frege/Shop.fr")
      .map(java.nio.file.Path.of(_)).find(java.nio.file.Files.exists(_)).get
    val checkedIn = java.nio.file.Files.readString(file)
    assertEquals(Jvm.frege("okay.frege.Shop", cbs), checkedIn)
  }

  test("a Frege program performs the callbacks, each run under the caller's Reader") {
    val quote = Frege.run[Reader % Map[String, Double], java.lang.Double](Quote.teaForThree.call(), calls = Jvm.calls(cbs))
    assertEquals(!.run(Reader.run(prices)(quote)).doubleValue, 6.0)
  }

  test("a callback nobody offered is refused by name, with the names that were") {
    val quote = Frege.run[Reader % Map[String, Double], java.lang.Double](Quote.teaForThree.call())
    val e = intercept[IllegalArgumentException](!.run(Reader.run(prices)(quote)))
    assert(e.getMessage.contains("called 'price_of', which no callback offered to it answers (offered: [])"), e.getMessage)
  }

  test("a wrong argument is a Frege TYPE error: the generated module types each operation") {
    val dir = java.nio.file.Files.createTempDirectory("okay-frege-bad")
    val src = dir.resolve("Bad.fr")
    java.nio.file.Files.writeString(src,
      """module okay.frege.Bad where
        |import okay.frege.Prog
        |import okay.frege.Shop
        |bad :: Prog Double
        |bad = perform (priceOf 42)
        |""".stripMargin): Unit
    val cp = System.getProperty("java.class.path")
    val javaBin = java.nio.file.Path.of(System.getProperty("java.home"), "bin", "java").toString
    val p = ProcessBuilder(javaBin, "-cp", cp, "frege.compiler.Main", "-d", dir.toString, "-fp", cp, src.toString)
      .redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes())
    assert(p.waitFor() != 0, out)
    assert(out.contains("String") && out.contains("Bad.fr:5"), out)
  }

  test("a callback whose Schemas have no Frege type is refused when the module is generated") {
    given Shape = Shape.python
    val listy = Foreign.callback[Vector[Long], Long]("total")(xs => Reader.ask[Long].map(_ + xs.sum))
    val e = intercept[IllegalArgumentException](Jvm.frege("okay.frege.Listy", Foreign.callbacks(listy)))
    assert(e.getMessage.contains("the callback 'total' has no Frege type"), e.getMessage)
  }
