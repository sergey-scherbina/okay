package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * collect-early-stop (specs/delimited-control.md): `Delim.collectUntil`
 * runs the SAME producer `collect` runs and stops it where the fold
 * is done — counted, not read off the code: the walk says how many
 * leaves it visited.
 */
class TestCollectUntil extends munit.FunSuite {

  type R = Delim + okay.Pure

  enum Tree[+A]:
    case Leaf(a: A)
    case Node(l: Tree[A], r: Tree[A])

  /** the same walk `collect` reads, with a visit counter */
  def walk(t: Tree[Int], visited: () => Unit)(using Delim.Emitting[Int]): Unit ! R = direct:
    t match
      case Tree.Leaf(a) =>
        visited()
        !Delim.emit(a)
      case Tree.Node(l, r) =>
        !walk(l, visited)
        !walk(r, visited)

  /** 1 .. 8 as a balanced tree */
  def tree(lo: Int, hi: Int): Tree[Int] =
    if lo == hi then Tree.Leaf(lo) else Tree.Node(tree(lo, (lo + hi) / 2), tree((lo + hi) / 2 + 1, hi))

  test("take(3) over the walk: three leaves visited, the rest of the walk never runs") {
    var n = 0
    assertEquals(!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], okay.Pure](using FoldUntil.take(3))(walk(tree(1, 8), () => n += 1))), Vector(1, 2, 3))
    assertEquals(n, 3)
  }

  test("find pulls up to and including its match") {
    var n = 0
    assertEquals(!.run(Delim.collectUntil[Int, Option[Int], Option[Int], okay.Pure](using FoldUntil.find[Int](_ == 5))(walk(tree(1, 8), () => n += 1))), Some(5))
    assertEquals(n, 5)
  }

  test("done(init): take(0) runs no body at all") {
    var n = 0
    assertEquals(!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], okay.Pure](using FoldUntil.take(0))(walk(tree(1, 8), () => n += 1))), Vector.empty[Int])
    assertEquals(n, 0)
  }

  test("a fold that is never done answers what collect answers, on the same producer") {
    var n = 0
    val t = tree(1, 8)
    val whole = !.run(Delim.collect[Int, okay.Pure](walk(t, () => ())))
    assertEquals(!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], okay.Pure](using FoldUntil.take(100))(walk(t, () => n += 1))), whole.toVector)
    assertEquals(n, 8)
    // the operator's shape: a running sum that stops itself
    val until = FoldUntil.until[Int, Int, Int](0)((s, a) => if s + a > 10 then Right(s) else Left(s + a))(identity)
    assertEquals(!.run(Delim.collectUntil[Int, Either[Int, Int], Int, okay.Pure](using until)(walk(t, () => ()))), 10)   // 1+2+3+4 = 10, +5 stops
  }

  test("a producer that emits nothing, and one that stops on its last element") {
    assertEquals(!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], okay.Pure](using FoldUntil.take(3))(direct(()))), Vector.empty[Int])
    var n = 0
    assertEquals(!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], okay.Pure](using FoldUntil.take(8))(walk(tree(1, 8), () => n += 1))), (1 to 8).toVector)
    assertEquals(n, 8)
  }

  test("depth: ten thousand emits under collectUntil, the stop never firing") {
    def many(n: Int)(using Delim.Emitting[Int]): Unit ! R = direct:
      var i = 0
      while i < n do
        !Delim.emit(i)
        i += 1
    assertEquals(!.run(Delim.collectUntil[Int, Long, Long, okay.Pure](using FoldUntil.long[Int, Long](0L)((s, a) => s + a)(_ => false)(identity))(many(10000))), (0L until 10000L).sum)
  }

  test("docs/continuations-in-practice.md §2, verbatim") {
    enum Tree2[+A]:
      case Leaf(a: A)
      case Node(l: Tree2[A], r: Tree2[A])
    var visited = 0
    def walk(t: Tree2[Int])(using Delim.Emitting[Int]): Unit ! R = direct:
      t match
        case Tree2.Leaf(a)    => visited += 1; !Delim.emit(a)
        case Tree2.Node(l, r) => !walk(l); !walk(r)
    val t = Tree2.Node(Tree2.Node(Tree2.Leaf(1), Tree2.Leaf(2)), Tree2.Leaf(3))
    assertEquals(!.run(Delim.collect[Int, okay.Pure](walk(t))), List(1, 2, 3))
    assertEquals(!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], okay.Pure](using FoldUntil.take(2))(walk(t))), Vector(1, 2))
    assertEquals(visited, 3 + 2, "the third leaf is never visited by the stopping read")
    assertEquals(!.run(Delim.collectUntil[Int, Option[Int], Option[Int], okay.Pure](using FoldUntil.find[Int](_ > 1))(walk(t))), Some(2))
  }

  test("docs/tutorial.md §13, verbatim") {
    enum Tree[+A]:
      case Leaf(a: A)
      case Node(l: Tree[A], r: Tree[A])

    def walk(t: Tree[Int])(using Delim.Emitting[Int]): Unit ! Delim + Pure = direct:
      t match
        case Tree.Leaf(a)    => !Delim.emit(a)
        case Tree.Node(l, r) => !walk(l); !walk(r)

    val tree = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(2)), Tree.Node(Tree.Leaf(3), Tree.Leaf(4)))

    assertEquals(!.run(Delim.collect[Int, Pure](walk(tree))), List(1, 2, 3, 4))
    assertEquals(!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], Pure](using FoldUntil.take(2))(walk(tree))), Vector(1, 2))
    assertEquals(!.run(Delim.collectUntil[Int, Boolean, Boolean, Pure](using FoldUntil.exists[Int](_ > 2))(walk(tree))), true)
    // the claims in the comments, counted through a twin of the walk
    var visited = 0
    def counted(t: Tree[Int])(using Delim.Emitting[Int]): Unit ! Delim + Pure = direct:
      t match
        case Tree.Leaf(a)    => visited += 1; !Delim.emit(a)
        case Tree.Node(l, r) => !counted(l); !counted(r)
    val _ = !.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], Pure](using FoldUntil.take(2))(counted(tree)))
    assertEquals(visited, 2, "the walk stops at its second leaf")
    visited = 0
    val _ = !.run(Delim.collectUntil[Int, Boolean, Boolean, Pure](using FoldUntil.exists[Int](_ > 2))(counted(tree)))
    assertEquals(visited, 3, "true, after three leaves")
  }

  test("collectingUntil nests under a delimited, and stops there too") {
    var n = 0
    val prog: Vector[Int] ! okay.Pure = Delim.delimited[Vector[Int], okay.Pure]:
      Delim.collectingUntil[Int, Vector[Int], Vector[Int], okay.Pure](using FoldUntil.take(2))(walk(tree(1, 8), () => n += 1))
    assertEquals(!.run(prog), Vector(1, 2))
    assertEquals(n, 2)
  }
}
