package okay


/** the STM battery on the parking platform (specs/stm.md): atomic
 * transfers under contention, consistent snapshots, retry woken by
 * the right commit, the fast paths */
class TestStm extends munit.FunSuite {

  test("transfers between two cells under eight threads keep the sum; a reader never sees a torn pair") {
    val a = TRef(1000L)
    val b = TRef(1000L)
    val torn = java.util.concurrent.atomic.AtomicInteger(0)
    val movers = (0 until 8).map { i =>
      Thread.ofVirtual().start { () =>
        val rnd = new scala.util.Random(i)
        for _ <- 0 until 2000 do
          val amt = rnd.nextInt(50).toLong
          val tx: Unit ! Tx =
            Tx.read(a).flatMap { x =>
              Tx.read(b).flatMap { y =>
                Tx.write(a, x - amt).flatMap(_ => Tx.write(b, y + amt))
              }
            }
          Stm[Async].atomically(tx).runWith
      }
    }
    val reader = Thread.ofVirtual().start { () =>
      for _ <- 0 until 5000 do
        val pair: (Long, Long) ! Tx = Tx.read(a).flatMap(x => Tx.read(b).map(y => (x, y)))
        val (x, y) = Stm[Async].atomically(pair).runWith
        if x + y != 2000L then torn.incrementAndGet(): Unit
    }
    movers.foreach(_.join()); reader.join()
    assertEquals(a.get + b.get, 2000L)
    assertEquals(torn.get, 0, "a read-only transaction saw a torn pair")
  }

  test("retry parks the transaction; the commit that changes what it read wakes it") {
    val r = TRef(0)
    val take: Int ! Tx =
      Tx.read(r).flatMap(x => Tx.check(x > 0).flatMap(_ => Tx.write(r, x - 1).map(_ => x)))
    val waiting = Async.runAsync(Stm[Async].atomically(take))
    Thread.sleep(20)
    assert(!waiting.isCompleted, "a retry completed with nothing to take")
    // an unrelated cell changing does NOT wake it
    val other = TRef(0)
    Stm[Async].atomically(Tx.write(other, 1)).runWith
    Thread.sleep(20)
    assert(!waiting.isCompleted, "woken by a cell it never read")
    Stm[Async].atomically(Tx.write(r, 5)).runWith
    assertEquals(scala.concurrent.Await.result(waiting, scala.concurrent.duration.Duration(1, "s")), 5)
    assertEquals(r.get, 4)
  }

  test("a one-op transaction is the cell's own CAS: modify answers the old value and installs the new") {
    val r = TRef(41)
    val old = Stm[Async].atomically(Tx.modify(r)(x => (x + 1, x))).runWith
    assertEquals(old, 41)
    assertEquals(r.get, 42)
    // the cell's version moved exactly once
    assertEquals(r.version, 1L)
    // and the same through TRef.modify directly (what the Channel does)
    assertEquals(r.modify(x => (x * 2, x)), 42)
    assertEquals(r.get, 84)
  }

  test("a retry with nothing read is refused, named: nothing could wake it") {
    val e = intercept[IllegalStateException](Stm[Async].atomically(Tx.retry[Int]).runWith)
    assert(e.getMessage.contains("nothing read"), e.getMessage)
  }

  test("a thousand parked transactions hold no thread; one commit frees the ones that read that cell") {
    val gate = TRef(false)
    val futs = (0 until 1000).map { _ =>
      Async.runAsync(Stm[Async].atomically(Tx.read(gate).flatMap(g => Tx.check(g).map(_ => g))))
    }
    Thread.sleep(20)
    assert(futs.forall(!_.isCompleted))
    Stm[Async].atomically(Tx.write(gate, true)).runWith
    futs.foreach(f => assertEquals(scala.concurrent.Await.result(f, scala.concurrent.duration.Duration(1, "s")), true))
  }

  test("the channel's cell is a TRef: a transaction can read the channel's state") {
    val c = Channel[Int]()
    assert(c.offer(1)); assert(c.offer(2))
    val n = Stm[Async].atomically(Tx.read(c.cell).map(_.size)).runWith
    assertEquals(n, 2)
  }
}
