package okay.py

/** the conformance programs in Haskell: programs only (no direct style) */
object HsConformance:
  val main: String = """module Main (main) where

import Okay

num :: Value -> Double
num (VDouble d) = d
num (VInt n) = fromInteger n
num v = error ("not a number: " ++ show v)

pairs :: [Value] -> Prog Value
pairs _ = do
  x <- perform "choose" [VList [VInt 1, VInt 2]]
  y <- perform "choose" [VList [VInt 10, VInt 20]]
  return (VInt (round (num x + num y)))

total :: [Value] -> Prog Value
total [sku, qty] = do
  price <- perform "price_of" [sku]
  perform "discount" [VDouble (num price * num qty)]
total _ = error "total takes a sku and a quantity"

boom :: [Value] -> Prog Value
boom _ = error "haskell says no"

main :: IO ()
main = serve [("pairs", pairs), ("total", total), ("boom", boom)]
"""

  lazy val ghc: Boolean = scala.util.Try(ProcessBuilder("ghc", "--version").start().waitFor() == 0).getOrElse(false)

  /** built once per run: GHC takes seconds, the suites share the binary */
  lazy val binary: String =
    val dir = java.nio.file.Files.createTempDirectory("okay-hs-conf")
    java.nio.file.Files.writeString(dir.resolve("Main.hs"), main): Unit
    HaskellWorker.build(dir).toString

/** (Haskell, pipes), JSON */
class TestHsPipes extends WireConformance:
  override def munitIgnore: Boolean = !HsConformance.ghc
  override def direct: Boolean = false
  lazy val engine: ForeignWorker = ForeignWorker.speaking(Seq(HsConformance.binary))
  override def afterAll(): Unit = if HsConformance.ghc then engine.close()

  test("the default DEFLATE is a preference: Haskell lacks it and keeps the plain wire, unrefused") {
    assertEquals(engine.wire, "json/none")
  }

/** (Haskell, pipes), CBOR chosen by a given */
class TestHsPipesCbor extends WireConformance:
  import WireFormat.Cbor.given
  override def munitIgnore: Boolean = !HsConformance.ghc
  override def direct: Boolean = false
  lazy val engine: ForeignWorker = ForeignWorker.speaking(Seq(HsConformance.binary))
  override def afterAll(): Unit = if HsConformance.ghc then engine.close()

  test("a Haskell worker has no DEFLATE, and a host whose given asks for it is refused by name") {
    import WireCompression.Deflate.given
    val e = intercept[IllegalStateException](ForeignWorker.speaking(Seq(HsConformance.binary)))
    assert(e.getMessage.contains("given WireCompression is deflate"), e.getMessage)
  }
