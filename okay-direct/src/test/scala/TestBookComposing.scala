package okay


import okay.freer.*
import okay.freer.given
import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 9, COMPILED (docs/continuations/09-composing.md).
 *
 * How the shapes compose: one machine, many delimiters. A
 * machine-starting door written inside a machine nests on it
 * (`Shift.Machine`, shift-merge-guard); `scope` says the same thing
 * explicitly.
 */
class TestBookComposing extends munit.FunSuite {

  type Row = Shift % ? + Pure

  // ---- a machine-starting door inside a machine: it nests

  test("delimited inside delimited stands on the running machine") {
    val r = Shift.delimited[String, Pure]:
      direct:
        val inner = !Shift.delimited[Int, Row]:
          direct:
            !Shift.exit(7)          // leaves the INNER boundary only
            0
        s"inner said $inner"
    assertEquals(!.run(r), "inner said 7")
  }

  // ---- the right spelling: the outermost runs, the inner installs

  test("scope nests under delimited: an inner boundary, an outer machine") {
    val r = Shift.delimited[String, Pure]:
      direct:
        val inner = !Shift.scope[Int, Pure]:
          direct:
            !Shift.exit(7)          // leaves the INNER boundary only
            0
        s"inner said $inner"
    assertEquals(!.run(r), "inner said 7")
  }

  // ---- crossing a boundary on purpose: the point of multi-prompt

  test("an inner scope can leave through the OUTER boundary, by naming it") {
    val r = Shift.delimited[String, Pure]: outer ?=>
      direct:
        val inner = !Shift.scope[Int, Pure]:
          direct:
            // not this scope's boundary — the one outside it
            !Shift.exit(using outer)("straight out")
            0
        s"inner said $inner"
    assertEquals(!.run(r), "straight out")
  }

  // ---- two shapes at once: collect a walk that may stop early

  enum Tree:
    case Leaf(n: Int)
    case Node(l: Tree, r: Tree)

  val t = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(99)), Tree.Leaf(3))

  def leaves(x: Tree)(using Shift.Emitting[Int]): Unit ! Row = direct:
    x match
      case Tree.Leaf(n) => !Shift.emit(n)
      case Tree.Node(l, r) => { !leaves(l); !leaves(r) }

  /** emits leaves, and leaves the walk at the first one over 50 */
  def upToBig(x: Tree)(using Shift.Emitting[Int], Shift.Prompted[Unit]): Unit ! Row =
    direct:
      x match
        case Tree.Leaf(n) =>
          if n > 50 then !Shift.exit(())
          !Shift.emit(n)
        case Tree.Node(l, r) => { !upToBig(l); !upToBig(r) }

  test("collect outside, exit inside: what was emitted before the exit is kept") {
    // `collect` is the OUTERMOST, so it runs the machine; the early
    // exit goes to a `scope` installed under it
    val got = !.run(Shift.collect[Int, Pure](
      direct:
        !Shift.scope[Unit, Pure](direct(!upToBig(t)))))
    assertEquals(got, List(1), "the emits before the exit were lost")
  }

  test("...and without the exit the same walk emits everything") {
    val all = !.run(Shift.collect[Int, Pure](direct(!leaves(t))))
    assertEquals(all, List(1, 99, 3))
  }

  // ---- onReturn and exit together, which chapter 8 promised

  test("a hook on the boundary sees an early exit from inside a nested scope") {
    val r = Shift.delimited[Int, Pure]:
      direct:
        !Shift.onReturn(n => n + 1000)
        val inner = !Shift.scope[Int, Pure]:
          direct:
            !Shift.exit(5)
            0
        inner
    assertEquals(!.run(r), 1005)
  }
}
