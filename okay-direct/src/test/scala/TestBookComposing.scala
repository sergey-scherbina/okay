package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 9, COMPILED (docs/continuations/09-composing.md).
 *
 * The chapter that shows the shapes DO NOT compose naively. The first
 * test is a compile error on purpose: it is the mistake, pinned, so
 * the page can promise that the wrong spelling is caught by the
 * compiler rather than by production.
 */
class TestBookComposing extends munit.FunSuite {

  type Row = Delim + Pure

  // ---- the mistake: a second machine inside the first

  test("the wrong spelling does not compile, and says why") {
    val e = compileErrors("""
      okay.Delim.delimited[Int, okay.Delim + okay.Pure](okay.pure(1))""")
    assert(e.nonEmpty, "a nested machine compiled")
    assert(e.contains("SECOND machine"), s"the message does not say what is wrong: $e")
  }

  // ---- the right spelling: the outermost runs, the inner installs

  test("scope nests under delimited: an inner boundary, an outer machine") {
    val r = Delim.delimited[String, Pure]:
      direct:
        val inner = !Delim.scope[Int, Pure]:
          direct:
            !Delim.exit(7)          // leaves the INNER boundary only
            0
        s"inner said $inner"
    assertEquals(!.run(r), "inner said 7")
  }

  // ---- crossing a boundary on purpose: the point of multi-prompt

  test("an inner scope can leave through the OUTER boundary, by naming it") {
    val r = Delim.delimited[String, Pure]: outer ?=>
      direct:
        val inner = !Delim.scope[Int, Pure]:
          direct:
            // not this scope's boundary — the one outside it
            !Delim.exit(using outer)("straight out")
            0
        s"inner said $inner"
    assertEquals(!.run(r), "straight out")
  }

  // ---- two shapes at once: collect a walk that may stop early

  enum Tree:
    case Leaf(n: Int)
    case Node(l: Tree, r: Tree)

  val t = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(99)), Tree.Leaf(3))

  def leaves(x: Tree)(using Delim.Emitting[Int]): Unit ! Row = direct:
    x match
      case Tree.Leaf(n) => !Delim.emit(n)
      case Tree.Node(l, r) => { !leaves(l); !leaves(r) }

  /** emits leaves, and leaves the walk at the first one over 50 */
  def upToBig(x: Tree)(using Delim.Emitting[Int], Delim.Prompted[Unit]): Unit ! Row =
    direct:
      x match
        case Tree.Leaf(n) =>
          if n > 50 then !Delim.exit(())
          !Delim.emit(n)
        case Tree.Node(l, r) => { !upToBig(l); !upToBig(r) }

  test("collect outside, exit inside: what was emitted before the exit is kept") {
    // `collect` is the OUTERMOST, so it runs the machine; the early
    // exit goes to a `scope` installed under it
    val got = !.run(Delim.collect[Int, Pure](
      direct:
        !Delim.scope[Unit, Pure](direct(!upToBig(t)))))
    assertEquals(got, List(1), "the emits before the exit were lost")
  }

  test("...and without the exit the same walk emits everything") {
    val all = !.run(Delim.collect[Int, Pure](direct(!leaves(t))))
    assertEquals(all, List(1, 99, 3))
  }

  // ---- onReturn and exit together, which chapter 8 promised

  test("a hook on the boundary sees an early exit from inside a nested scope") {
    val r = Delim.delimited[Int, Pure]:
      direct:
        !Delim.onReturn(n => n + 1000)
        val inner = !Delim.scope[Int, Pure]:
          direct:
            !Delim.exit(5)
            0
        inner
    assertEquals(!.run(r), 1005)
  }
}
