package okay

import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 11, COMPILED (docs/continuations/11-four-captures.md).
 *
 * Two captures since cont-core-design (2026-10-01): `shift` and
 * `shift0`, λ$'s pair. They differ on ONE switch — does the handler's
 * body run under the delimiter — and this suite establishes which
 * programs can see it. Every assertion was discovered by running it.
 * (`control`/`control0`, the other switch, left the library: no module
 * used them.)
 */
class TestBookFourCaptures extends munit.FunSuite {

  type Row = Delim + Pure

  def capture(name: String, p: Prompt[String])
             (f: (String => String ! Row) => String ! Row): String ! Row = name match
    case "shift" => Delim.shift[String, String, Pure](p)(f)
    case "shift0" => Delim.shift0[String, String, Pure](p)(f)

  def outcome(prog: String ! Row): String =
    try !.run(Delim.run[String, Pure](prog))
    catch case _: NoPrompt => "NoPrompt"

  // ---- both agree on the ordinary programs

  test("on an ordinary capture, both do the same thing") {
    for c <- List("shift", "shift0") do
      val p = Delim.prompt[String]
      val prog = Delim.push(p)(capture(c, p)(k => k("a")).map(s => s"[$s]"))
      assertEquals(outcome(prog), "[a]", s"$c differed")
  }

  test("and on invoking the continuation twice") {
    for c <- List("shift", "shift0") do
      val p = Delim.prompt[String]
      val prog = Delim.push(p)(
        capture(c, p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(s => s"<$s>"))
      assertEquals(outcome(prog), "<a><b>", s"$c differed")
  }

  // ---- THE SWITCH: does the handler's body run under the delimiter?

  test("a second capture from the HANDLER BODY separates shift from shift0") {
    def probe(c: String): String =
      val p = Delim.prompt[String]
      outcome(Delim.push(p)(
        capture(c, p)(_ => capture("shift", p)(_ => okay.pure("inner-caught")))))
    assertEquals(probe("shift"), "inner-caught")
    assertEquals(probe("shift0"), "NoPrompt")
  }

  test("a capture INSIDE the continuation finds the delimiter either way: k re-installs it") {
    def probe(c: String): String =
      val p = Delim.prompt[String]
      outcome(Delim.push(p)(
        capture(c, p)(k => k("a"))
          .flatMap(s => Delim.shift[String, String, Pure](p)(_ => okay.pure(s + "-second")))))
    assertEquals(probe("shift"), "a-second")
    assertEquals(probe("shift0"), "a-second")
  }
}
