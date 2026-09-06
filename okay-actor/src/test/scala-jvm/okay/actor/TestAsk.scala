package okay.actor

import okay.*
import okay.given

/** LAW 7: an ask answers or times out — never both, never neither. */
class TestAsk extends munit.FunSuite {

  given Scheduler = Schedulers.loom

  enum Msg:
    case Add(n: Int)
    case Get(reply: Reply[Int])
    case Ignore(reply: Reply[Int])   // deliberately never answers

  test("law: an ask gets the answer the behaviour puts in the box") {
    val a = Actor.spawn(0) { (n: Int, m: Msg) =>
      async(m match
        case Msg.Add(k) => n + k
        case Msg.Get(r) => r(n); n
        case Msg.Ignore(_) => n)
    }.runWith
    // `tell` answers whether the mailbox took the message, and a
    // test that throws that away is asserting the sum of messages
    // that may never have been sent
    assert(a.tell(Msg.Add(2)).runWith)
    assert(a.tell(Msg.Add(3)).runWith)
    assertEquals(a.ask(Msg.Get.apply, within = 5000).runWith, Some(5))
    a.stop().runWith
  }

  test("law: an ask nobody answers times out rather than waiting for ever") {
    val a = Actor.spawn(0) { (n: Int, m: Msg) =>
      async(m match
        case Msg.Add(k) => n + k
        case Msg.Get(r) => r(n); n
        case Msg.Ignore(_) => n)
    }.runWith
    val t0 = System.currentTimeMillis()
    assertEquals(a.ask(Msg.Ignore.apply, within = 200).runWith, None)
    val waited = System.currentTimeMillis() - t0
    assert(waited >= 150 && waited < 3000, s"waited $waited ms, expected about 200")
    a.stop().runWith
  }

  test("law: asking a stopped actor answers None rather than hanging") {
    val a = Actor.spawn(0) { (n: Int, _: Msg) => async(n) }.runWith
    a.stop().runWith
    val deadline = System.currentTimeMillis() + 5000
    while !a.stopped && System.currentTimeMillis() < deadline do Thread.`yield`()
    assertEquals(a.ask(Msg.Get.apply, within = 5000).runWith, None)
  }

  test("a behaviour that replies twice does not corrupt anything") {
    val a = Actor.spawn(0) { (n: Int, m: Msg) =>
      async(m match
        case Msg.Get(r) => r(1); r(2); n   // the second answer is dropped
        case _ => n)
    }.runWith
    assertEquals(a.ask(Msg.Get.apply, within = 5000).runWith, Some(1))
    a.stop().runWith
  }
}
