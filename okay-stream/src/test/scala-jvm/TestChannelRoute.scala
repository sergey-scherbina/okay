package okay


import okay.testkit.Munit

/**
 * channel-route-per-producer (specs/channel-route-per-producer.md): a
 * partitioned channel keeps a producer's order when the producer says
 * who it is, whatever thread each send happens to run on — the case of
 * a fiber resumed somewhere else on `own`/`adaptive`.
 */
class TestChannelRoute extends Munit.Diagnosed:

  /** ONE producer writing 0 until 2n: the first half from one thread, the
   * second from another (a fiber that moved), then read back whole */
  private def movedProducer(send: (Channel[Int], Int) => Boolean, route: Channel[Int] => Unit = _ => ()): List[Int] =
    val n = 32
    val c = Channel.forProducers[Int](2, 4 * n)
    route(c)
    def from(lo: Int): Thread = Thread(() => { var i = lo; while i < lo + n do { assert(send(c, i)); i += 1 } })
    val a = from(0); a.start(); a.join()
    val b = from(n); b.start(); b.join()
    c.close()
    // a fresh consumer thread per round: its scan starts at a part of its own
    var out = List.empty[Int]
    val r = Thread(() => out = Iterator.continually(c.receiveBlocking()).takeWhile(_.isDefined).flatten.toList)
    r.start(); r.join()
    out

  test("a producer that moves threads, sending by thread, can come back out of order (why the route exists)") {
    val rounds = (1 to 200).map(_ => movedProducer((c, i) => c.offer(i)))
    val bad = rounds.count(_ != (0 until 64).toList)
    note(s"$bad of 200 rounds out of order")
    assert(bad > 0, "every round in order: this law can no longer tell a thread from a producer")
  }

  test("the same producer with a claimed route keeps its order, from any thread") {
    var r = -1
    val rounds = (1 to 200).map(_ => movedProducer((c, i) => c.offerFrom(r, i), c => r = c.claimRoute()))
    val bad = rounds.filter(_ != (0 until 64).toList)
    onFailure(s"first bad round: ${bad.headOption}")
    assertEquals(bad.size, 0)
  }
