package okay.staging

import okay.State
import scala.quoted.*
import scala.quoted.staging.{Compiler, run}
import scala.util.Try

/**
 * staging-scope-extrusion (2026-09-24): a generator with an EFFECT — a
 * cell — stores the code of a bound variable while inside its binder
 * and splices it after the binder closed. Kameyama, Kiselyov & Shan,
 * "Combinators for impure yet hygienic code generation" (PEPM 2014;
 * SCP 2015) name this scope extrusion; MetaOCaml finds it at run time.
 *
 * WHAT HAPPENS TODAY, measured: Scala 3's staging refuses it BY NAME
 * when the code is built, before anything is compiled or run — "a
 * reference to parameter x was used outside the scope where it was
 * defined". The cheaper road the backlog entry offered (a refusal at
 * splice time) is therefore the compiler's own, and okay adds nothing
 * on top of it:
 *  - `RuntimeStaged`'s generator keeps no `Expr` in any cell — its
 *    caches hold COMPILED codecs, and it is a pure recursion over the
 *    schema — and a generation that fails for any reason answers the
 *    interpreter and records why in `lastFailure`, so even a future
 *    extrusion bug there would degrade, not corrupt.
 *  - the other staging road, inline stagers (`Direct.staged`,
 *    `Stager`), has no code VALUE a cell could hold: the inliner's
 *    binders are ordinary run-time values by the time a cell sees them.
 * These tests pin the refusal (both a host `var` and an okay `State`
 * cell), and the control beside it, so a Scala release that stops
 * checking turns them red.
 */
class TestScopeExtrusion extends munit.FunSuite:

  given Compiler = Compiler.make(getClass.getClassLoader)

  private val refusal = "used outside the scope where it was defined"

  private def refused(r: Try[Int]): Unit =
    assert(r.isFailure, s"extruded code was built and ran: $r")
    assert(r.failed.get.getMessage.contains(refusal), s"refused for another reason: ${r.failed.get}")

  test("a var cell carrying x out of its lambda is refused, by name") {
    var leaked: Option[Expr[Int]] = None
    refused(Try(run {
      val f = '{ (x: Int) => ${ leaked = Some('x); 'x } + 1 }
      '{ val g = $f; g(1) + ${ leaked.get } }
    }))
  }

  test("an okay State cell, leaked through its answer, is refused the same way") {
    var leaked: Option[Expr[Int]] = None
    refused(Try(run {
      val f = '{ (x: Int) => ${
        leaked = State.run[Option[Expr[Int]], Option[Expr[Int]]](None)(State.set[Option[Expr[Int]]](Some('x)))._1
        'x } + 1 }
      '{ val g = $f; g(1) + ${ leaked.get } }
    }))
  }

  test("control: the same cell, spliced INSIDE its binder, generates and runs") {
    var kept: Option[Expr[Int]] = None
    val r = run {
      '{ (x: Int) => ${ kept = Some('x); '{ ${ kept.get } * 10 } } }
    }
    assertEquals(r(4), 40)
  }
