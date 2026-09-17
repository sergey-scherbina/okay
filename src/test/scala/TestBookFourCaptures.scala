package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 11, COMPILED (docs/continuations/11-four-captures.md).
 *
 * The four captures differ on two independent switches, and this
 * suite establishes WHICH PROGRAMS CAN SEE THE DIFFERENCE -- which
 * turned out to be a smaller set than the switches suggest. Every
 * assertion here was discovered by running it, not copied from a
 * table.
 */
class TestBookFourCaptures extends munit.FunSuite {

  type Row = Delim + Pure

  def capture(name: String, p: Prompt[String])
             (f: (String => String ! Row) => String ! Row): String ! Row = name match
    case "shift" => Delim.shift[String, String, Pure](p)(f)
    case "shift0" => Delim.shift0[String, String, Pure](p)(f)
    case "control" => Delim.control[String, String, Pure](p)(f)
    case "control0" => Delim.control0[String, String, Pure](p)(f)

  def outcome(prog: String ! Row): String =
    try !.run(Delim.run[String, Pure](prog))
    catch case _: NoPrompt => "NoPrompt"

  // ---- all four agree on the ordinary programs

  test("on an ordinary capture, all four do the same thing") {
    for c <- List("shift", "shift0", "control", "control0") do
      val p = Delim.prompt[String]
      val prog = Delim.push(p)(capture(c, p)(k => k("a")).map(s => s"[$s]"))
      assertEquals(outcome(prog), "[a]", s"$c differed")
  }

  test("and on invoking the continuation twice") {
    for c <- List("shift", "shift0", "control", "control0") do
      val p = Delim.prompt[String]
      val prog = Delim.push(p)(
        capture(c, p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(s => s"<$s>"))
      assertEquals(outcome(prog), "<a><b>", s"$c differed")
  }

  // ---- SWITCH ONE: does the handler's body run under the delimiter?

  test("a second capture from the HANDLER BODY separates shift from shift0") {
    def probe(c: String): String =
      val p = Delim.prompt[String]
      outcome(Delim.push(p)(
        capture(c, p)(_ => capture("shift", p)(_ => okay.pure("inner-caught")))))
    assertEquals(probe("shift"), "inner-caught")
    assertEquals(probe("control"), "inner-caught")
    assertEquals(probe("shift0"), "NoPrompt")
    assertEquals(probe("control0"), "NoPrompt")
  }

  // ---- SWITCH TWO: is the captured continuation delimited?

  test("a capture INSIDE the continuation separates control0 from the rest") {
    def probe(c: String): String =
      val p = Delim.prompt[String]
      outcome(Delim.push(p)(
        capture(c, p)(k => k("a"))
          .flatMap(s => Delim.shift[String, String, Pure](p)(_ => okay.pure(s + "-second")))))
    assertEquals(probe("shift"), "a-second")
    assertEquals(probe("shift0"), "a-second")
    assertEquals(probe("control"), "a-second")     // NOT different from shift here
    assertEquals(probe("control0"), "NoPrompt")
  }

  test("THE HONEST PART: shift and control are not separated by either probe") {
    // both switches are real in the implementation, but a program
    // that sees `delimitK` alone has to invoke the continuation where
    // the delimiter is not otherwise in force -- and `control`'s body
    // IS under the delimiter, so it is.
    def bodyProbe(c: String): String =
      val p = Delim.prompt[String]
      outcome(Delim.push(p)(
        capture(c, p)(_ => capture("shift", p)(_ => okay.pure("x")))))
    def kProbe(c: String): String =
      val p = Delim.prompt[String]
      outcome(Delim.push(p)(
        capture(c, p)(k => k("a"))
          .flatMap(s => Delim.shift[String, String, Pure](p)(_ => okay.pure(s)))))
    assertEquals(bodyProbe("shift"), bodyProbe("control"))
    assertEquals(kProbe("shift"), kProbe("control"))
  }
}
