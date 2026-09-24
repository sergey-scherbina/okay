package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * WHAT THE MACHINE CAN SAY ABOUT ITSELF (delim-diagnostics,
 * 2026-09-17). A captured continuation has no useful JVM stack trace,
 * so the question is not how to fake one — it is what the machine
 * actually knows: which delimiters are installed, where each was
 * written, and where the capture that failed was written. These tests
 * are that, and the `NoPrompt` message is checked as TEXT because it
 * is the documentation for the one hazard the types do not close.
 */
class TestDelimDiagnostics extends munit.FunSuite {

  type P = okay.Pure

  // ---- At: the caller's position, at compile time

  test("At.here names this file and this line, and two lines differ") {
    val a = At.here
    val b = At.here
    assert(a.where.startsWith("TestDelimDiagnostics.scala:"), a.where)
    assertNotEquals(a.where, b.where, "two call sites got the same position")
    // it is a constant: nothing is computed at run time
    assertEquals(a.where, At.here.where.takeWhile(_ != ':') + ":" + a.where.split(':')(1))
  }

  test("At.here is the CALLER's position, not the library's") {
    // the whole reason it is a given: a method that takes one is
    // labelled by whoever called it
    def door(using at: At): String = at.where
    assert(door.startsWith("TestDelimDiagnostics.scala:"), door)
  }

  // ---- the label a delimiter carries

  test("each door labels its prompt with what made it and where") {
    val label = !.run(Delim.delimited[String, P]: (p: Delim.Prompted[String]) ?=>
      okay.pure(p.prompt.label))
    assert(label.startsWith("delimited @ TestDelimDiagnostics.scala:"), label)

    val nested = !.run(Delim.delimited[String, P]:
      direct:
        !Delim.scope[String, P]: (in: Delim.Prompted[String]) ?=>
          okay.pure(in.prompt.label))
    assert(nested.startsWith("scope @ TestDelimDiagnostics.scala:"), nested)
  }

  // ---- NoPrompt: the message IS the documentation

  test("NoPrompt names the capture, the prompt it wanted, what IS installed, and the rule") {
    // the hole delim-safety stage 0 cannot close: an abstract row, so
    // a second machine is started inside one that already has a Delim
    def generic[F[+_]](p: Int ! Delim + F): Int ! F = Delim.run(p)
    val outer = Delim.prompt[Int]
    def prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        100 + !generic[Delim + P](Delim.shift[Int, Int, Delim + P](outer)(k => k(5)))

    val e = intercept[NoPrompt](!.run(prog))
    val msg = e.getMessage

    // WHERE the capture was written
    assert(e.from.startsWith("TestDelimDiagnostics.scala:"), s"from=${e.from}")
    assert(msg.contains(e.from), msg)
    // WHICH prompt it wanted, with the line that made it
    assert(e.wanted.startsWith("prompt @ TestDelimDiagnostics.scala:"), s"wanted=${e.wanted}")
    assert(msg.contains(e.wanted), msg)
    // and the RULE that explains the difference
    assert(msg.contains("ONE `Delim.run` PER PROGRAM"), msg)
    assert(msg.contains("scope"), msg)
    assert(msg.contains("collecting"), msg)
  }

  test("NoPrompt lists the delimiters that ARE installed, innermost first") {
    // two delimiters on one machine, and a capture to neither
    val stray = Delim.prompt[Int]
    def prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        !Delim.scope[Int, P]:
          direct:
            !Delim.shift[Int, Int, P](stray)(k => k(5))

    val e = intercept[NoPrompt](!.run(prog))
    assertEquals(e.installed.size, 2, s"installed=${e.installed}")
    assert(e.installed.head.startsWith("scope @ "), s"innermost is not the scope: ${e.installed}")
    assert(e.installed(1).startsWith("delimited @ "), s"outermost is not the delimited: ${e.installed}")
    assert(e.getMessage.contains("Installed here, innermost first"), e.getMessage)
  }

  test("a machine with no delimiter at all says so, rather than printing an empty list") {
    val stray = Delim.prompt[Int]
    val e = intercept[NoPrompt](
      !.run(Delim.run[Int, P](Delim.shift[Int, Int, P](stray)(k => k(1)))))
    assertEquals(e.installed, Nil)
    assert(e.getMessage.contains("no delimiter installed"), e.getMessage)
  }

  // ---- where a dialogue is waiting

  test("Paused.where is the line of the pause that made it") {
    def booking(using Delim.Asking[String, String, String, Delim + P]): String ! Delim + P =
      direct:
        val city = !Delim.pause("city?")
        val nights = !Delim.pause(s"nights in $city?")
        s"$city/$nights"

    val first = !.run(Delim.resumable[String, String, String, P](booking))
    assertEquals(first.asking, Some("city?"))
    val w1 = first.where.getOrElse(fail("a paused dialogue does not know where it is"))
    assert(w1.startsWith("TestDelimDiagnostics.scala:"), w1)

    // the SECOND pause is a different line, which is the point: a
    // dialogue that has not moved names the line it is sitting on
    val second = !.run(Delim.answer(first, Nil)("Kyiv"))._1
    val w2 = second.where.getOrElse(fail("no position after the first answer"))
    assertNotEquals(w1, w2, "both pauses reported the same line")

    // and a finished dialogue is not waiting anywhere
    val done = !.run(Delim.answer(second, Nil)("3"))._1
    assertEquals(done.where, None)
    assertEquals(done.finished, Some("Kyiv/3"))
  }
}
