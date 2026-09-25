package okay.chain

import scala.collection.mutable

/**
 * okay-watch can adopt this module (specs/chain.md §5): its shapes —
 * a block is (number, hash, parent) strings, an EMPTY hash is a gap,
 * `head`/`block(n)` block at the transport edge — driven through
 * `Poller` + `Tracker` give the events its own `TestFollower` expects.
 * Its `Progress.Rewound(from, to)` is read off `RolledBack(to, from)`
 * as `(from.height, to.height + 1)`.
 *
 * Nothing of okay-watch is imported: the fake below reproduces its
 * `Fake` chain, and each test is one of its `TestFollower` cases.
 */
class TestWatchShape extends munit.FunSuite:

  /** okay-watch's Block, with the fields the follower reads */
  final case class WBlock(number: Long, hash: String, parent: String)
  given BlockOf[WBlock] with
    type Tx = Nothing
    def ref(b: WBlock): BlockRef = BlockRef(Point(b.number, BlockId(b.hash)), BlockId(b.parent), None)
    def txs(b: WBlock): Vector[Nothing] = Vector.empty

  /** okay-watch's `Fake` chain, as a PollSource */
  final class Fake extends PollSource[WBlock]:
    val blocks = mutable.Map.empty[Long, WBlock]
    var tip = -1L
    def head: Tip = Tip(Point(tip, BlockId(blocks.get(tip).fold("")(_.hash))))
    def block(n: Long): Polled[WBlock] = blocks.get(n) match
      case None => Polled.Missing
      case Some(b) if b.hash.isEmpty => Polled.Gap
      case Some(b) => Polled.Found(b)
    def grow(n: Int, fork: String = "a"): Unit =
      for _ <- 1 to n do
        tip += 1
        val parent = blocks.get(tip - 1).map(_.hash).getOrElse("genesis")
        blocks(tip) = WBlock(tip, s"$fork$tip", parent)
    def reorg(at: Long, n: Int, fork: String): Unit =
      blocks.keys.filter(_ >= at).toList.foreach(blocks.remove)
      tip = at - 1
      grow(n, fork)

  /** okay-watch's Progress, read off this module's events */
  enum Progress:
    case Confirmed(b: WBlock)
    case Rewound(from: Long, to: Long)
  private def watch(es: Vector[Event[WBlock]]): Vector[Progress] = es.map {
    case Event.Confirmed(b) => Progress.Confirmed(b)
    case Event.RolledBack(to, from) => Progress.Rewound(from.height, to.height + 1)
  }
  private def step(f: Follow[WBlock]): Vector[Progress] = watch(f.step().fold(b => fail(b.reason), identity))
  private def confirmed(ps: Vector[Progress]): Vector[Long] = ps.collect { case Progress.Confirmed(b) => b.number }

  test("catches up to the safe height and stops") {
    val c = Fake(); c.grow(10)
    val f = Follow(c, Finality.Depth(2), from = 0)
    assertEquals(confirmed(step(f)), (0L to 7L).toVector)
    assertEquals(f.next, 8L)
    assertEquals(step(f), Vector.empty)
    c.grow(1)
    assertEquals(confirmed(step(f)), Vector(8L))
  }

  test("a block not yet there ends the step and is asked again") {
    val c = Fake(); c.grow(3); c.tip = 10
    val f = Follow(c, Finality.Depth(0), from = 0)
    assertEquals(confirmed(step(f)), Vector(0L, 1L, 2L))
    assertEquals(f.next, 3L)
  }

  test("depth hides a reorg shallower than itself") {
    val c = Fake(); c.grow(10)
    val f = Follow(c, Finality.Depth(3), from = 0)
    val _ = step(f)
    c.reorg(at = 8, n = 4, fork = "b")
    val ps = step(f)
    assert(!ps.exists(_.isInstanceOf[Progress.Rewound]), ps)
    assertEquals(confirmed(ps), Vector(7L, 8L))
    assertEquals(ps.collect { case Progress.Confirmed(b) => b.hash }.last, "b8")
  }

  test("a reorg deeper than depth is said, at the fork point, then followed") {
    val c = Fake(); c.grow(10)
    val f = Follow(c, Finality.Depth(0), from = 0)
    assertEquals(confirmed(step(f)), (0L to 9L).toVector)
    c.reorg(at = 6, n = 6, fork = "b")
    val ps = step(f)
    assertEquals(ps.head, Progress.Rewound(9L, 6L))
    assertEquals(confirmed(ps), (6L to 11L).toVector)
    val hashes = ps.collect { case Progress.Confirmed(b) => b.hash }
    assert(hashes.forall(_.startsWith("b")), hashes)
  }

  test("continuity holds across steps: the ring remembers what was emitted") {
    val c = Fake(); c.grow(5)
    val f = Follow(c, Finality.Depth(0), from = 0)
    val _ = step(f)
    c.reorg(at = 4, n = 2, fork = "b")
    assertEquals(step(f),
      Vector(Progress.Rewound(4L, 4L), Progress.Confirmed(c.blocks(4)), Progress.Confirmed(c.blocks(5))))
  }

  test("a gap is stepped over, not linked, and continuity resumes across it") {
    // okay-watch CONFIRMS its gap as an empty block; here a gap is not a
    // block, so 3 is absent — the one stated difference (specs/chain.md §3)
    val c = Fake(); c.grow(3)
    c.tip = 5
    c.blocks(3) = WBlock(3, "", "")
    c.blocks(4) = WBlock(4, "a4", "a2")
    c.blocks(5) = WBlock(5, "a5", "a4")
    val f = Follow(c, Finality.Depth(0), from = 0)
    val ps = step(f)
    assertEquals(confirmed(ps), Vector(0L, 1L, 2L, 4L, 5L))
    assert(!ps.exists(_.isInstanceOf[Progress.Rewound]), ps)
  }

  test("a reorg below every kept block is Broken (okay-watch restarted from its first height)") {
    val c = Fake(); c.grow(10)
    val f = Follow(c, Finality.Depth(0), from = 0, keep = 3)
    val _ = step(f)
    c.reorg(at = 2, n = 10, fork = "b")
    assert(f.step().isLeft)
  }

  // follow-keeps-progress: a source that fails in the middle of a step
  test("a failure after progress keeps what was confirmed; the height is asked again next step") {
    val c = Fake(); c.grow(10)
    var failAt = Option(6L)
    val flaky = new PollSource[WBlock]:
      def head: Tip = c.head
      def block(h: Long): Polled[WBlock] =
        if failAt.contains(h) then throw RuntimeException(s"429 at $h") else c.block(h)
    val f = Follow(flaky, Finality.Depth(0), from = 0)
    val first = f.step().toOption.get.collect { case Event.Confirmed(b) => b.number }
    assertEquals(first, (0L to 5L).toVector, "0..5 were confirmed before 6 failed, and are said")
    assertEquals(f.next, 6L)
    val again = intercept[RuntimeException](f.step())   // nothing gained: the failure is said
    assert(again.getMessage.contains("429 at 6"))
    failAt = None
    assertEquals(f.step().toOption.get.collect { case Event.Confirmed(b) => b.number }, (6L to 9L).toVector)
  }
