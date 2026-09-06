package okay.actor

import okay.*
import okay.given

/**
 * The laws a poisonous message must keep, whatever the loop does to
 * read the mailbox: every message once, in order; supervision knows
 * WHICH message failed; Stop closes after the ones before it. Written
 * for actor-receive-offer-first, whose loop change was measured and
 * declined -- the laws stay, because they held for the old loop too
 * and nobody had written them.
 */
class TestPoisonLaws extends munit.FunSuite:

  test("law: every message once, in order") {
    val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
    val a = Actor.spawn(()) { (_: Unit, m: Int) => okay.effect[Async, Unit](Async.Run(() => seen += m)) }.runWith
    for i <- 0 until 2000 do assert(a.tell(i).runWith)
    a.stop().runWith
    val deadline = System.currentTimeMillis() + 5000
    while !a.stopped && System.currentTimeMillis() < deadline do Thread.`yield`()
    assertEquals(seen.toList, (0 until 2000).toList)
  }

  test("law: a poisonous message is the ONE that fails, and Resume skips exactly it") {
    val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
    val a = Actor.spawn(0, Channel[Int](256), Supervise.Resume) { (n: Int, m: Int) =>
      if m == 7 then throw RuntimeException("poison")
      else okay.effect[Async, Int](Async.Run(() => { seen += m; n + 1 }))
    }.runWith
    for i <- 0 until 20 do assert(a.tell(i).runWith)
    a.stop().runWith
    val deadline = System.currentTimeMillis() + 5000
    while !a.stopped && System.currentTimeMillis() < deadline do Thread.`yield`()
    assertEquals(seen.toList, (0 until 20).filter(_ != 7).toList)
  }

  test("law: Stop on a poisonous message closes the mailbox after the messages before it, and nothing after it runs") {
    val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
    val a = Actor.spawn(0, Channel[Int](256), Supervise.Stop) { (n: Int, m: Int) =>
      if m == 5 then throw RuntimeException("poison")
      else okay.effect[Async, Int](Async.Run(() => { seen += m; n + 1 }))
    }.runWith
    for i <- 0 until 10 do { val _ = a.tell(i).runWith }
    // NOT `a.stopped`: Stop closes the mailbox with 6..9 still accepted
    // inside it and nobody left to drain them, so `finished` -- "every
    // accepted element handed over" -- is never true. That is the
    // module's standing semantics (filed as actor-stop-strands), and
    // the observable here is the closed mailbox: a tell is refused.
    val deadline = System.currentTimeMillis() + 5000
    while a.tell(99).runWith && System.currentTimeMillis() < deadline do Thread.sleep(1)
    assert(!a.tell(100).runWith, "the mailbox did not close after the poisonous message")
    assertEquals(seen.toList, List(0, 1, 2, 3, 4))
  }
