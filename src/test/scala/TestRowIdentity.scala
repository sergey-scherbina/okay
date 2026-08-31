package okay

import okay.given

/**
 * What a row can and cannot tell apart, made executable.
 *
 * `<|>` splits a union by a runtime test on the LEFT signature, and
 * that test is by ERASURE. For most signatures the erasure is the
 * whole identity and the split is total. For a signature parameterised
 * by something with no runtime trace it is NOT, and two instances of
 * the same signature at different parameters in ONE row will misroute.
 *
 * The compiler says so, at every use site, as "the type test cannot be
 * checked at runtime" — which is unactionable there and was drowning
 * out everything else it had to say. The limitation belongs here
 * instead: named, demonstrated, and impossible to be surprised by.
 */
class TestRowIdentity extends munit.FunSuite {

  test("Writer is told apart by its ELEMENT's class — the opaque type erases to W") {
    // `opaque type Writer[W, +A] = W`, so telling a String IS a String
    // at runtime and the test is complete for class-distinct W
    val prog: Unit ! (Writer % String + Writer % Int) =
      okay.effect[Writer % String + Writer % Int, Unit](Writer("hello"))
        .flatMap(_ => okay.effect[Writer % String + Writer % Int, Unit](Writer(42)))
        .map(_ => ())

    val outer = Writer.run[Int, Unit, Writer % String](prog)
    val (ints, rest) = Writer.run[String, (Seq[Int], Unit), Pure](outer).runWith
    val strings = ints
    assertEquals(rest._1.toList, List(42), "the Int writer collected the wrong")
    assertEquals(strings.toList, List("hello"), "the String writer collected the wrong")
  }

  test("Reader CANNOT be told apart: Ask() carries no trace of R") {
    // `enum Reader[R, +A] { case Ask() }` — one class, whatever R is.
    // So a row with two readers of different environments misroutes,
    // and this test exists to say exactly that rather than to pretend
    // otherwise. If it ever starts failing, the row grew real identity
    // and this limitation can be deleted from the docs.
    val prog: String ! (Reader % Int + Reader % String) =
      okay.effect[Reader % Int + Reader % String, Int](Reader.Ask())
        .flatMap(n =>
          okay.effect[Reader % Int + Reader % String, String](Reader.Ask())
            .map(s => s"$n/$s"))

    // The Int handler answers BOTH asks, because it cannot see the
    // difference — and the good news is HOW that surfaces: the String
    // continuation receives an Integer and the cast fails at once.
    // Loud, at the first wrong answer, not a plausible wrong result
    // returned to the caller. That is worth knowing precisely, so it
    // is asserted rather than described.
    val thrown = intercept[ClassCastException] {
      val half = Reader.run[Int, String, Reader % String](7)(prog)
      Reader.run[String, String, Pure]("s")(half).runWith
    }
    assert(thrown.getMessage.contains("Integer"), thrown.getMessage)
    assert(thrown.getMessage.contains("String"), thrown.getMessage)
  }

  test("the rule, stated positively: one instance per signature in a row") {
    // a row with ONE reader is exact, whatever the environment type
    val prog: Int ! (Reader % Int + Pure) =
      okay.effect[Reader % Int + Pure, Int](Reader.Ask()).map(_ * 2)
    assertEquals(Reader.run[Int, Int, Pure](21)(prog).runWith, 42)
  }
}
