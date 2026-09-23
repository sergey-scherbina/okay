package okay.scalus

/** a message split across segments, and several in one */
class TestDemux extends munit.FunSuite:

  test("a block message cut into three segments comes out whole, once") {
    val msg = Recorded.inbound.filter(_.protocol == N2N.BlockFetch).map(_.payload).maxBy(_.length)
    val d = N2N.Demux()
    val parts = msg.grouped(msg.length / 3 + 1).toVector
    val outs = parts.map(p => d.feed(N2N.Segment(0, true, N2N.BlockFetch, p)).fold(fail(_), identity))
    assertEquals(outs.map(_.size), Vector(0, 0, 1))
    assert(outs.last.head match { case Cv.Arr(Vector(Cv.UInt(t), Cv.Tag(24, _, _))) => t == 4; case _ => false })
  }

  test("two messages in one segment come out as two") {
    val d = N2N.Demux()
    val both = N2N.requestNext ++ N2N.chainSyncDone
    assertEquals(d.feed(N2N.Segment(0, false, N2N.ChainSync, both)).map(_.size), Right(2))
  }

  test("an indefinite-length array reads; a reserved head is Bad, not Incomplete") {
    assert(Cv.read(Array(0x9f, 0x01, 0x02, 0xff).map(_.toByte)) match
      case Cv.Read.Done(Cv.Arr(v), 4) => v.size == 2
      case _ => false)
    assertEquals(Cv.read(Array(0x9f, 0x01).map(_.toByte)), Cv.Read.Incomplete)
    assert(Cv.read(Array(0x1c.toByte)) match { case Cv.Read.Bad(_) => true; case _ => false })
  }
