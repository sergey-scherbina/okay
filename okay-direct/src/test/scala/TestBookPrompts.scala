package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 10, COMPILED (docs/continuations/10-prompts.md).
 *
 * Part III opens the machine, and this suite shows the one value the
 * whole thing is built around: a prompt. What it carries, what its
 * type buys, and what it says when it is used where it does not
 * belong.
 */
class TestBookPrompts extends munit.FunSuite {

  type Row = Shift % ? + Pure

  // ---- a prompt is a VALUE, and it carries its own origin

  test("a prompt says what made it and where") {
    val p = Shift.prompt[Int]
    assert(p.label.startsWith("prompt @ "), p.label)
    assert(p.label.contains("TestBookPrompts.scala"),
      s"the label does not name the file that made it: ${p.label}")
  }

  test("two prompts of the same type are different boundaries") {
    val a = Shift.prompt[Int]
    val b = Shift.prompt[Int]
    assert(a ne b, "two prompts collapsed into one")
  }

  // ---- the type is the contract: Prompt[R] says what leaves here

  test("the prompt's type is the boundary's answer type") {
    val r: String ! Pure = Shift.delimited[String, Pure]:
      direct:
        !Shift.exit("a string, because the boundary says String")
        "unreachable"
    assertEquals(!.run(r), "a string, because the boundary says String")
  }

  test("a boundary of a different type is a different boundary") {
    val r = Shift.delimited[String, Pure]: outer ?=>
      direct:
        val n = !Shift.scope[Int, Pure]:       // an Int boundary inside a String one
          direct:
            !Shift.exit(42)
            0
        s"got $n"
    assertEquals(!.run(r), "got 42")
  }

  // ---- using a prompt where it does not belong

  test("a prompt used outside its machine says so, and names the stack") {
    val stray = Shift.prompt[Int]
    val e = intercept[NoPrompt](
      !.run(Shift.delimited[Int, Pure]:
        direct:
          !Shift.abort[Int, Int, Pure](stray)(1)))
    assert(e.getMessage.contains("is not on the stack"), e.getMessage)
    assert(e.getMessage.contains("Installed here"), e.getMessage)
    // the message names the boundary that IS installed, so a reader
    // can see what they should have aimed at
    assert(e.getMessage.contains("delimited @"), e.getMessage)
  }

  // ---- first-class means it can be passed, which is what crossing needs

  def deep(n: Int)(using p: Shift.Prompted[String]): Int ! Row = direct:
    if n == 0 then
      // both branches must be Int, or the row cannot be inferred --
      // the exit's own answer is Unit and is discarded here
      !Shift.exit(using p)("bottom reached")
      0
    else !deep(n - 1)

  test("a prompt handed down thirty frames still names the same boundary") {
    val r = Shift.delimited[String, Pure]: outer ?=>
      direct:
        val _ = !deep(30)(using outer)
        "never"
    assertEquals(!.run(r), "bottom reached")
  }
}
