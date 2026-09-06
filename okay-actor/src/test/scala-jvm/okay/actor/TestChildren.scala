package okay.actor

import okay.*
import okay.given

/** LAW 8: children die first — a parent's stop completes only after
 * its children's mailboxes are closed AND drained. */
class TestChildren extends munit.FunSuite {

  given Scheduler = Schedulers.loom

  test("law: stopping a parent stops its children first") {
    val parent = Actor.spawn(0) { (n: Int, m: Int) => async(n + m) }.runWith
    val child = parent.spawnChild(0)((n: Int, m: Int) => async(n + m)).runWith
    val grand = child.spawnChild(0)((n: Int, m: Int) => async(n + m)).runWith
    assert(!child.stopped && !grand.stopped)
    parent.stop().runWith
    assert(child.stopped, "a child must not outlive its parent's mailbox")
    assert(grand.stopped, "and neither must a grandchild")
  }

  test("law: a parent's stop waits for its children to DRAIN") {
    val handled = java.util.concurrent.atomic.AtomicInteger(0)
    val parent = Actor.spawn(0) { (n: Int, m: Int) => async(n + m) }.runWith
    val child = parent.spawnChild(0, Channel[Int](1024))((n: Int, _: Int) =>
      async { handled.incrementAndGet(): Unit; n + 1 }).runWith
    (0 until 300).foreach(i => { val _ = child.tell(i).runWith })
    parent.stop().runWith
    assertEquals(handled.get, 300,
      "the accepted messages were still coming; the parent must wait for them")
  }

  test("a child stopped on its own leaves the parent running") {
    val parent = Actor.spawn(0) { (n: Int, m: Int) => async(n + m) }.runWith
    val child = parent.spawnChild(0)((n: Int, m: Int) => async(n + m)).runWith
    child.stop().runWith
    val deadline = System.currentTimeMillis() + 5000
    while !child.stopped && System.currentTimeMillis() < deadline do Thread.`yield`()
    assert(child.stopped)
    assert(!parent.stopped, "a child's death is not its parent's")
    assert(parent.tell(1).runWith, "and the parent still accepts")
    parent.stop().runWith
  }
}
