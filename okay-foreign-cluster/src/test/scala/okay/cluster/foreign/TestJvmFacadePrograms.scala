package okay.cluster.foreign

import frege.run8.Thunk

/**
 * Clojure and Frege programs through the facade's `Programs`
 * (foreign-jvm-programs): the SAME conformance body Python, R, TypeScript,
 * Go, Rust and Haskell answer — a callback under the caller's Reader, and a
 * continuation resumed on every branch — walked in this process. Default
 * gate: no toolchain, no wire.
 */
class TestJvmFacadePrograms extends munit.FunSuite:

  test("Programs over Clojure: a callback under a Reader, and a continuation resumed twice") {
    FacadeConformance.programs(CljModule("okay.cluster.facade"), "priced", "pairs")
  }

  /** the glue a Frege function needs: its arguments are lazy on the JVM */
  private val frege = FregeModule("facade")
    .program("priced") {
      case order: java.util.Map[?, ?] =>
        okay.frege.FacadePrograms.priced(Thunk.`lazy`(String.valueOf(order.get("sku"))),
          Thunk.`lazy`(order.get("qty") match { case n: java.lang.Long => n; case _ => java.lang.Long.valueOf(0L) })).call()
      case other => throw IllegalArgumentException(s"priced takes an order, not $other")
    }
    .program("pairs")(_ => okay.frege.FacadePrograms.pairs.call())

  test("Programs over Frege: a callback under a Reader, and a continuation resumed twice") {
    FacadeConformance.programs(frege, "priced", "pairs")
  }

  test("a Clojure function the namespace does not have is refused by name, as a program's answer") {
    val P = summon[Programs[CljModule]]
    val out = P.run(CljModule("okay.cluster.facade"))(okay.runChoice(P.program[Int, Long, okay.Choose](CljModule("okay.cluster.facade"), "nope", Vector.empty)(0)))
    assert(out.headOption.exists(_.left.exists(_.kind == "LookupError")), s"$out")
  }
