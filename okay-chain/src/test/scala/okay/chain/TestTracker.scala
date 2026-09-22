package okay.chain

import B.p

/** the follower over a PUSH source: every case scripted, no network */
class TestTracker extends munit.FunSuite:

  private def feedAll(t0: Tracker[B], os: Seq[Observed[B]]): (Tracker[B], Vector[Event[B]]) =
    os.foldLeft((t0, Vector.empty[Event[B]])) { case ((t, acc), o) =>
      val (t2, es) = t.feed(o).fold(b => fail(b.reason), identity)
      (t2, acc ++ es)
    }
  private def heights(es: Vector[Event[B]]) = es.collect { case Event.Confirmed(b) => b.height }

  test("Depth(n): a block is confirmed once n blocks sit on it, in order, once") {
    val (t, es) = feedAll(Tracker[B](Finality.Depth(2)), B.run(0, 5, "a", "g").map(Observed.Forward(_)))
    assertEquals(heights(es), Vector(0L, 1L, 2L, 3L))
    val (_, more) = feedAll(t, B.run(6, 7, "a", "a5").map(Observed.Forward(_)))
    assertEquals(heights(more), Vector(4L, 5L))
  }

  test("Finalized: only what the chain declares final, however deep the rest") {
    val (t, es) = feedAll(Tracker[B](Finality.Finalized), B.run(0, 9, "a", "g").map(Observed.Forward(_)))
    assertEquals(heights(es), Vector.empty)
    val (_, more) = feedAll(t, Seq(Observed.AtTip(Tip(p(9, "a9"), finalized = Some(p(4, "a4"))))))
    assertEquals(heights(more), Vector(0L, 1L, 2L, 3L, 4L))
  }

  test("Finalized on ANOTHER fork confirms nothing: the block held at that height is not the final one") {
    val (t, _) = feedAll(Tracker[B](Finality.Finalized), B.run(0, 9, "a", "g").map(Observed.Forward(_)))
    val (_, es) = feedAll(t, Seq(Observed.AtTip(Tip(p(9, "b9"), finalized = Some(p(4, "b4"))))))
    assertEquals(heights(es), Vector.empty)
  }

  test("Depth counts the chain followed, not the source's head: a head on a fork not yet seen confirms nothing") {
    val (t, es) = feedAll(Tracker[B](Finality.Depth(3)), B.run(0, 5, "a", "g").map(Observed.Forward(_)))
    assertEquals(heights(es), Vector(0L, 1L, 2L))
    val (_, more) = feedAll(t, Seq(Observed.AtTip(Tip(p(40, "b40")))))
    assertEquals(heights(more), Vector.empty)
  }

  test("a rollback shallower than the policy is absorbed: nothing said, the new fork followed") {
    val t0 = feedAll(Tracker[B](Finality.Depth(3)), B.run(0, 9, "a", "g").map(Observed.Forward(_)))._1
    val fork = B.run(8, 11, "b", "a7")
    val (_, es) = feedAll(t0, Observed.Backward(p(7, "a7")) +: fork.map(Observed.Forward(_)))
    assert(!es.exists { case Event.RolledBack(_, _) => true; case _ => false }, es)
    val confirmed = es.collect { case Event.Confirmed(b) => b.id }
    assertEquals(confirmed, Vector("a7", "b8"))
  }

  test("a rollback below the frontier is RolledBack(to, from), then the new fork is confirmed") {
    val t0 = feedAll(Tracker[B](Finality.Depth(0)), B.run(0, 9, "a", "g").map(Observed.Forward(_)))._1
    val (_, es) = feedAll(t0, Observed.Backward(p(5, "a5")) +: B.run(6, 11, "b", "a5").map(Observed.Forward(_)))
    assertEquals(es.head, Event.RolledBack(p(5, "a5"), p(9, "a9")))
    assertEquals(es.tail.collect { case Event.Confirmed(b) => b.id }, (6 to 11).map(h => s"b$h").toVector)
  }

  test("a block that does not extend the chain followed is Broken, naming both") {
    val t0 = feedAll(Tracker[B](Finality.Depth(0)), B.run(0, 3, "a", "g").map(Observed.Forward(_)))._1
    val r = t0.feed(Observed.Forward(B(4, "b4", "b3")))
    assert(r.left.exists(b => b.reason.contains("b3") && b.reason.contains("a3")), r)
  }

  test("a rollback to a point not on the chain followed is Broken") {
    val t0 = feedAll(Tracker[B](Finality.Depth(0)), B.run(0, 3, "a", "g").map(Observed.Forward(_)))._1
    assert(t0.feed(Observed.Backward(p(5, "zz"))).isLeft)
  }

  test("a checkpoint resumes: the first block must extend it") {
    val t0 = Tracker[B](Finality.Depth(0), from = Some(p(9, "a9")))
    assert(t0.feed(Observed.Forward(B(10, "a10", "a9"))).isRight)
    assert(t0.feed(Observed.Forward(B(10, "b10", "b9"))).isLeft)
    assertEquals(t0.nextHeight, Some(10L))
  }
