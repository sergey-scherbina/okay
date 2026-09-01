package okay

import okay.Direct.{*, given}
import scala.language.implicitConversions

/**
 * Coloring as POLICY (specs/direct-auto-coloring.md): Effect[G]
 * markers are ordinary givens, so provide/providing install them
 * per scope — a block's code auto-colors only where its caller
 * granted the permission.
 */
class TestEffectProvide extends munit.FunSuite {

  type RInt = [X] =>> Reader[Int, X]
  type F = Reader % Int + Writer % String
  def ask: Reader[Int, Int] = Reader.Ask()

  /** the block REQUIRES the coloring permission from its scope */
  def prog(using Effect[RInt]): Int ! F = direct {
    val env: Int = ask                    // colors by the grant
    Writer(s"env=$env").reflect
    env + 1
  }

  val grant: Effect[RInt] = new Effect[RInt] {}

  test("provide grants the coloring policy for one expression") {
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, Writer % String](41)(provide(grant)(prog))))
    assertEquals(ws, Seq("env=41"))
    assertEquals(a, 42)
  }

  test("providing composes the policy as a layer") {
    val base = providing[Effect[RInt]](grant)
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, Writer % String](7)(base { prog })))
    assertEquals(ws, Seq("env=7"))
    assertEquals(a, 8)
  }

  test("without the grant the same block does not color") {
    val e = compileErrors(
      "import okay.Direct.{*, given}; import scala.language.implicitConversions; " +
        "def ask: okay.Reader[Int, Int] = okay.Reader.Ask(); " +
        "okay.Direct.direct[[X] =>> X ! (okay.Reader % Int)] { val env: Int = ask; env } ")
    assert(e.nonEmpty, "the ungran­ted coloring must not compile")
  }
}
