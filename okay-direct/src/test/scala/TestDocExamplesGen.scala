package okay

import okay.Direct.*

/** the generator snippets of docs/guide.md §3 and docs/theory/07-logic-streams.md, verbatim */
class TestDocExamplesGen extends munit.FunSuite:

  test("guide §3: squares by for-comprehension, countdown as a block") {
    val squares: Gen[Long] = for n <- Gen.unfold(1L)(i => Some((i, i + 1))) yield n * n
    val firstThree =
      squares.take(3).toList                        // List(1, 4, 9) — the body ran three steps, then stopped
    assertEquals(firstThree, List(1L, 4L, 9L))

    val countdown: Gen[Int] = generator[Int] {    // a block: while/if/recursion, emit, stop
      var i = 3
      while i > 0 do { Gen.emit(i).!?; i -= 1 }
    }
    val first =
      countdown.iterator.next()                     // 3 — the body has run to its first yield and no further
    assertEquals(first, 3)
    assertEquals(countdown.toList, List(3, 2, 1))
  }

  enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
  import Tree.*

  def leaves(t: Tree): Gen[Int] = generator[Int] {     // chapter 2's tree walk, as a Gen
    t match
      case Leaf(v)    => Gen.emit(v).!?
      case Node(l, r) => leaves(l).!?; leaves(r).!?
  }

  test("theory ch. 7: the tree walk as a Gen, read one leaf at a time") {
    var visited = 0
    def counted(t: Tree): Gen[Int] = generator[Int] {
      t match
        case Leaf(v)    => visited += 1; Gen.emit(v).!?
        case Node(l, r) => counted(l).!?; counted(r).!?
    }
    val tree = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    val it = leaves(tree).iterator
    assertEquals(it.next(), 1)
    assertEquals(leaves(tree).toList, List(1, 2, 3))
    val it2 = counted(tree).iterator
    assertEquals(it2.next(), 1)
    assertEquals(visited, 1, "the walk ran to its first leaf and holds the rest")
  }
