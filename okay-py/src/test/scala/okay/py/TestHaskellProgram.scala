package okay.py

import okay.{Choose, Reader, effect, runChoice, given}
import okay.agent.Durable

object TestHaskellProgram:
  // no margin: the docs quote these lines, and the snippet check reads them trimmed
  val main: String = """module Main (main) where

import Okay

asInt :: Value -> Integer
asInt (VInt n) = n
asInt (VDouble d) = round d
asInt v = error ("not an int: " ++ show v)

asDouble :: Value -> Double
asDouble (VDouble d) = d
asDouble (VInt n) = fromInteger n
asDouble v = error ("not a number: " ++ show v)

-- two choices; okay's Choice handler continues each continuation twice
pairs :: [Value] -> Prog Value
pairs _ = do
  x <- perform "choose" [VList [VInt 1, VInt 2]]
  y <- perform "choose" [VList [VInt 10, VInt 20]]
  return (VInt (asInt x + asInt y))

priced :: [Value] -> Prog Value
priced [sku, qty] = do
  p <- perform "price_of" [sku]
  return (VDouble (asDouble p * fromInteger (asInt qty)))
priced _ = error "priced takes a sku and a quantity"

boom :: [Value] -> Prog Value
boom _ = error "haskell says no"

main :: IO ()
main = serve [("pairs", pairs), ("priced", priced), ("boom", boom)]
"""

/** remote-foreign against a LIVE GHC (specs/remote-foreign.md): the same wire, a Haskell far side */
class TestHaskellProgram extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val ghc = scala.util.Try(ProcessBuilder("ghc", "--version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !ghc

  private lazy val w =
    val dir = java.nio.file.Files.createTempDirectory("okay-hs")
    java.nio.file.Files.writeString(dir.resolve("Main.hs"), TestHaskellProgram.main): Unit
    PySubprocess.speaking(Seq(HaskellWorker.build(dir).toString))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if ghc then w.close()

  private val choose = Py.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))

  test("MULTI-SHOT across a process, from Haskell: every branch of two choices") {
    assertEquals(w.pythonVersion, "haskell")
    val pairs = Py.program[Long]("pairs").calling(Py.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    pairs.forget.runWith
  }

  test("a Haskell program's operation is a Scala callback under the caller's Reader") {
    val price = Py.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val run = Py.program[Double]("priced").calling(Py.callbacks(price))("tea", 3L)
    assertEquals(Reader.run(Map("tea" -> 4.0))(run.program).runWith, Right(12.0))
  }

  test("a Haskell error is a condition by name, and the worker lives on") {
    val boom = Py.program[Long]("boom").calling(Py.callbacks(choose))()
    val got = runChoice(boom.program).runWith
    assert(got.headOption.exists(_.left.exists(c => c.kind == "HaskellError" && c.message.contains("haskell says no"))), s"$got")
    val again = Py.program[Long]("pairs").calling(Py.callbacks(choose))()
    assertEquals(runChoice(again.program).runWith.size, 4)
  }

  test("Durable journals a Haskell program's walk; the replay needs no Haskell") {
    val j = Durable.MemoryJournal()
    val pairs = Py.program[Long]("pairs").calling(Py.callbacks(choose))()
    val live = runChoice(pairs.program).runWith(using Durable.over[PyEval](w.handler, j)())
    assertEquals(runChoice(pairs.program).runWith(using Durable.replayingOver[PyEval](j)), live)
  }
}
