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

  val evens: Gen[Int] =
    for x <- Gen.unfold(1)(i => Some((i, i + 1))) if x % 2 == 0 yield x * x

  val fib: Gen[Long] = generator[Long] {
    var (a, b) = (0L, 1L)
    while true do
      Gen.emit(a).!?
      val t = a; a = b; b = t + b
  }

  def countdown(n: Int): Gen[Int] = generator[Int] {
    if n > 0 then
      Gen.emit(n).!?
      countdown(n - 1).!?                       // a recursive generator, flat on the stack
  }

  val infinite: Gen[Int] = generator[Int] { var i = 0; while true do { i += 1; Gen.emit(i).!? } }

  @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
  def docThreeWaysDemo(): Unit =
    // 1. with no macro at all — a plain for-comprehension over Gen is a
    //    generator, lazy, nested, guarded
    evens.take(3).toList                          // List(4, 16, 36) — the body ran to its sixth yield

    // 2. as a generator block — while/if/recursion, `yield` inside `for` emits
    fib.drop(10).first                            // Some(55)

    // 3. any Writer program you already have, or two generators in sequence
    Gen.of(Writer.tell(1).flatMap(_ => Writer.tell(2))).toList   // List(1, 2)
    (Gen(1) ++ Gen(2, 3)).toList                                   // List(1, 2, 3); flatMap is `yield from`

  test("docs/direct-style.md: three ways to write a generator, and they compose") {
    docThreeWaysDemo()
    assertEquals(evens.take(3).toList, List(4, 16, 36))
    assertEquals(fib.drop(10).first, Some(55L))
    assertEquals(countdown(5).toList, List(5, 4, 3, 2, 1))
    assertEquals(Gen.of(Writer.tell(1).flatMap(_ => Writer.tell(2))).toList, List(1, 2))
    assertEquals((Gen(1) ++ Gen(2, 3)).toList, List(1, 2, 3))
  }

  @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
  def docGenEndsDemo(): Unit =
    Gen(1, 2, 3).iterator.toList                   // the body ended: exhausted
    generator[Int] {                               // Gen.stop from inside a loop:
      var i = 0                                    //   nothing after it runs
      while true do { i += 1; if i > 3 then Gen.stop[Int].!?; Gen.emit(i).!? }
    }.toList                                       // List(1, 2, 3)
    infinite.take(5)                               // the reader stopped: the rest never runs

  test("docs/direct-style.md: how a generation ends, three ways") {
    docGenEndsDemo()
    assertEquals(Gen(1, 2, 3).iterator.toList, List(1, 2, 3))
    val ended = generator[Int] {
      var i = 0
      while true do { i += 1; if i > 3 then Gen.stop[Int].!?; Gen.emit(i).!? }
    }.toList
    assertEquals(ended, List(1, 2, 3))
    assertEquals(infinite.take(5).toList, List(1, 2, 3, 4, 5))
  }
