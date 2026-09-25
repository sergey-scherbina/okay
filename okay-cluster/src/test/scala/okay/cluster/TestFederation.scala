package okay.cluster

import okay.codec.Codecs
import okay.given
import scala.collection.mutable.ArrayBuffer

/**
 * TWO PARTIES, ONE MACHINE (specs/federation.md, stage 1).
 *
 * The spec's first claim — a job over N logs owned by N parties
 * computes what the same job over their union computes, and no
 * party's records cross to any other — asked of two processes that
 * each hold a log. One machine, because federation is about
 * OWNERSHIP and not distance: what is shown here is which process
 * read which log and what bytes left it, and none of that changes
 * when the sockets get longer.
 *
 * Three things are asserted, in-process first (where the store can
 * be asked what it handed out) and then across real JVMs:
 *
 *  1. the union's answer — equal to the fan over the whole feed;
 *  2. what crossed decodes under `Wire.wire` as accumulators and
 *     as nothing else — the decoded value re-encodes to the very
 *     bytes, so no byte is unaccounted for by the Schema;
 *  3. no party can stand in for another — a party asked for a
 *     partition it does not hold refuses, so when party B is gone
 *     the run FAILS naming B instead of computing B's share from
 *     A's log. Killing B is the test of that, and A's read count
 *     is the proof.
 */
class TestFederation extends munit.FunSuite {
  import Feeds.*

  /** the one test below that spawns real JVMs: its result depends on
   * process start-up time, which `sbt test` does not control (found by
   * ts-facade's full gate, 2026-09-23, timing out at load ~48 on 14
   * cores while the same suite alone at load 18 passed 10/10 a minute
   * later — backlog: federation-two-process-timeout). Every other test
   * here stays a plain `test`, in the default gate. */
  def liveTest(name: String)(body: => Any): Unit = test(name.tag(new munit.Tag("Live")))(body)

  Party.install()
  val feed: Feed = Feed(20000, Late - 1)

  def whole(parts: Int): Run[Sum] =
    Flows.fan(Flow.slices(events(feed), parts), PartyJob.sink(feed)).runWith

  /** a coordinator that commits epoch `at` and then dies — the same
   * shape TestSeek and TestResume use */
  final class Dying(at: Int) extends Checkpoint:
    val kept = Checkpoint.Memory()
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      kept.save(epoch, bytes)
      if epoch == at then throw Dying.Died(epoch)
    def latest: Option[(Int, Array[Byte])] = kept.latest
  object Dying:
    final case class Died(epoch: Int) extends RuntimeException(s"died at epoch $epoch")

  /** a `Serve` that keeps what crossed on the way OUT */
  def taped(s: Cluster.Serve, tape: ArrayBuffer[Array[Byte]]): Cluster.Serve = req =>
    val r = s(req)
    r match
      case Resp.Partial(bytes) => tape.synchronized { tape += bytes }: Unit
      case _ => ()
    r

  /** assertion 2: every crossed byte is a byte of an accumulator's
   * encoding under the sink's `wire` — decode, re-encode, compare */
  def accumulatorsOnly(tape: Seq[Array[Byte]]): Unit =
    val codec = Codecs.cbor(PartyJob.sink(feed).wire)
    for (bytes, i) <- tape.zipWithIndex do
      val w = codec.decode(bytes).fold(why => fail(s"partial $i does not decode as accumulators: $why"), identity)
      assert(codec.encode(w).sameElements(bytes),
        s"partial $i carries bytes the Schema does not account for")

  test("two parties in one JVM: the union's answer, and each party read only its own log") {
    for parts <- Vector(2, 4) do
      val tape = ArrayBuffer.empty[Array[Byte]]
      val parties = Vector.tabulate(parts)(n => taped(Party.as(n), tape))
      val before = Vector.tabulate(parts)(n => Party.log(n, feed, parts).records.get)
      val got = Cluster.run(PartyJob, feed, parts, parties).runWith
      val all = whole(parts)
      assertEquals(got.value, all.value, s"$parts parties")
      assertEquals(got.dropped, all.dropped, s"$parts parties")
      assertEquals(got.merged, all.merged, s"$parts parties")
      assertEquals(got.retried, 0L, "a partition moved between parties")
      // the store's own count: each party handed out exactly its slice
      // — TWICE, because a windowed sink's pre-pass (`Req.Extent`)
      // reads the partition for its bounds before the run reads it
      // for its panes. Two reads of its OWN log; none of another's.
      val n = events(feed).length
      for p <- 0 until parts do
        val slice = (n.toLong * (p + 1) / parts) - (n.toLong * p / parts)
        assertEquals(Party.log(p, feed, parts).records.get - before(p), 2 * slice, s"party $p's log")
      accumulatorsOnly(tape.toSeq)
      assertEquals(tape.length, parts)
  }

  test("what crossed, priced: accumulators against the records they stand for") {
    val tape = ArrayBuffer.empty[Array[Byte]]
    val got = Cluster.run(PartyJob, feed, 2, Vector(taped(Party.as(0), tape), taped(Party.as(1), tape))).runWith
    assertEquals(got.value, whole(2).value)
    val crossed = tape.map(_.length.toLong).sum
    val held = (0 until 2).map(p => Party.held(p, feed, 2)).sum
    println(f"%n  two parties, ${feed.n} records: $held%,d bytes held, $crossed%,d crossed (${crossed * 100.0 / held}%.2f%%)%n")
    assert(crossed < held / 10, s"more than a tenth of the records' bytes crossed: $crossed of $held")
  }

  test("and streamed, epoch by epoch, each party from its own log") {
    val got = Cluster.stream(PartyJob, feed, 2, Vector(Party.as(0), Party.as(1)), 64).runWith
    assertEquals(got.value, whole(2).value)
    assertEquals(got.dropped, whole(2).dropped)
  }

  test("A PARTY THAT RESUMES RESUMES FROM ITS OWN LOG, and seeks rather than re-reading it") {
    // The stage-1 box that was waiting on dataflow stage 11: a
    // federated stream that stops and resumes must have each party
    // open ITS log at ITS position, and no party may read another's
    // to catch up. `PartyJob`'s sink is WINDOWED, which used to mean
    // "replay from zero" — box 2b's horizon (dataflow-horizon-seek)
    // is what makes this a seek at all, and the read counts are how
    // the test tells the two apart.
    val parties = Vector(Party.as(0), Party.as(1))
    val j = Dying(3)
    val _ = intercept[Dying.Died](
      Cluster.stream(PartyJob, feed, 2, parties, 512, j).runWith)
    // the workers' sessions die with the coordinator
    val f = Codecs.cbor(summon[okay.codec.Schema[Folded]])
      .decode(j.latest.get._2).fold(fail(_), identity)
    Vector.tabulate(2)(i => f.base + i).foreach(Sessions.drop)

    val before = Vector.tabulate(2)(n => Party.log(n, feed, 2).records.get)
    val got = Cluster.stream(PartyJob, feed, 2, parties, 512, j.kept).runWith
    val read = Vector.tabulate(2)(n => Party.log(n, feed, 2).records.get - before(n))

    assertEquals(got.value, whole(2).value, "the resumed federated run answered wrong")
    assertEquals(got.dropped, whole(2).dropped)
    val own = events(feed).length / 2
    for n <- 0 until 2 do
      assert(read(n) > 0, s"party $n read nothing on the resume")
      assert(read(n) < own.toLong,
        s"party $n re-read its whole log (${read(n)} of $own) — it did not seek")
    // AND THE MARKS ARE IN THE JOURNAL, which is what it sought by
    assert(f.marks.nonEmpty, "a windowed federated run recorded no seek marks")
  }

  test("a party does not stand in for another: with B gone the run fails naming B") {
    val a = Party.log(0, feed, 2).records.get
    val b = Party.log(1, feed, 2).records.get
    val down: Cluster.Serve = _ => throw java.io.IOException("party 1 is down")
    val e = intercept[IllegalStateException](
      Cluster.run(PartyJob, feed, 2, Vector(Party.as(0), down)).runWith)
    assert(e.getMessage.contains("partition 1"), e.getMessage)
    assert(e.getMessage.contains("party 1"), e.getMessage)
    assert(e.getMessage.contains("this is party 0"), s"A should have been the one asked: ${e.getMessage}")
    // A read at most its own slice — never a record on B's behalf
    // (B's log is B's; the refusal came before any read)
    assert(Party.log(0, feed, 2).records.get - a <= events(feed).length / 2,
      "party A read more than its own log")
    assertEquals(Party.log(1, feed, 2).records.get - b, 0L, "party B's log was read in a run B was not in")
  }

  test("a process that is no party holds no partition") {
    val e = intercept[IllegalStateException](
      Cluster.run(PartyJob, feed, 2, Vector(Cluster.local)).runWith)
    assert(e.getMessage.contains("no party"), e.getMessage)
  }

  liveTest("TWO REAL PROCESSES, each its own party: the union's answer, the bytes, and B killed") {
    val procs = Workers.spawn(2, "okay.cluster.Party$", n => Seq(s"-Dokay.party=$n"))
    try
      // a deadline on the wait, failing with every party's thread dump
      // (cluster-forked-stall)
      val ports = Workers.ports(procs).map { line =>
        assert(line.contains("test.party"), s"the party did not register the job: $line")
        line.split(' ')(2).toInt
      }
      val tape = ArrayBuffer.empty[Array[Byte]]
      val wire = ports.toVector.map(p => taped(Served.connect("127.0.0.1", p), tape))

      val got = Cluster.run(PartyJob, feed, 2, wire).runWith
      val all = whole(2)
      assertEquals(got.value, all.value, "two processes, two logs")
      assertEquals(got.dropped, all.dropped)
      assertEquals(got.merged, all.merged)
      assertEquals(got.retried, 0L, "a partition moved between parties")
      accumulatorsOnly(tape.toSeq)
      assertEquals(tape.length, 2)

      // kill B; A is asked for B's partition and says no
      procs(1).destroyForcibly().waitFor(): Unit
      val e = intercept[IllegalStateException](Cluster.run(PartyJob, feed, 2, wire).runWith)
      assert(e.getMessage.contains("partition 1"), e.getMessage)
      assert(e.getMessage.contains("party 1"), s"the failure should name the missing party: ${e.getMessage}")
      assert(e.getMessage.contains("this is party 0"), s"A should have refused, not computed: ${e.getMessage}")
    finally procs.foreach(_.destroyForcibly(): Unit)
  }

  // ── stage 2: the refusal ─────────────────────────────────────────

  test("a party runs only the jobs its owner allowed, and the refusal names the list") {
    // the party allows one job and is asked for another
    val guarded = Cluster.guarded(Set("test.party"), Set("the-hospital"))("the-hospital")(Party.as(0))
    val ok = guarded(Req.Known)
    assert(ok.isInstanceOf[Resp.Names], s"a recognised coordinator asking a general question: $ok")
    val bytes = Codecs.cbor(PartyJob.params).encode(feed)
    guarded(Req.Extent("test.window", bytes, 0, 1)) match
      case Resp.Failed(why) =>
        assert(why.contains("does not run the job 'test.window'"), why)
        assert(why.contains("test.party"), s"the refusal should name what IS allowed: $why")
      case other => fail(s"a job off the list was not refused: $other")
    // and the allowed one still runs
    guarded(Req.Extent("test.party", bytes, 0, 1)) match
      case _: Resp.Extents => ()
      case other => fail(s"the allowed job was refused: $other")
  }

  test("an UNRECOGNISED coordinator is refused before the pre-pass, and learns nothing else") {
    val guarded = Cluster.guarded(Set("test.party"), Set("the-hospital"))("a-stranger")(Party.as(0))
    val bytes = Codecs.cbor(PartyJob.params).encode(feed)
    val before = Party.log(0, feed, 1).records.get
    for req <- Vector(Req.Known, Req.Extent("test.party", bytes, 0, 1),
                      Req.Run("test.party", bytes, 0, 1, Vector.empty)) do
      guarded(req) match
        case Resp.Failed(why) =>
          assert(why.contains("does not recognise the coordinator 'a-stranger'"), why)
          // THE REFUSAL TELLS A STRANGER NOTHING ELSE. A message that
          // named the allowed jobs would hand a directory of this
          // party's business to whoever knocked.
          assert(!why.contains("test.party"), s"the refusal leaked the allow-list: $why")
        case other => fail(s"a stranger was served: $other")
    // AND NOT ONE RECORD WAS READ. The refusal comes before the job,
    // which is the same rule stage 1 found for a foreign partition:
    // emptiness is not a refusal, and neither is a read that happened.
    assertEquals(Party.log(0, feed, 1).records.get, before,
      "a stranger's request reached the log")
  }

  test("a session already admitted keeps answering; a stranger's Advance does not") {
    // `Advance` and `Close` name a SESSION, not a job — the job check
    // happened when it was opened, and the coordinator check has not,
    // so it still runs on every request
    val open = Cluster.guarded(Set("test.party"), Set("the-hospital"))("the-hospital")(Party.as(0))
    val stranger = Cluster.guarded(Set("test.party"), Set("the-hospital"))("a-stranger")(Party.as(0))
    val bytes = Codecs.cbor(PartyJob.params).encode(feed)
    val session = System.nanoTime()
    open(Req.Open("test.party", bytes, 0, 1, session)) match
      case Resp.Opened(_) => ()
      case other => fail(s"the admitted coordinator could not open a session: $other")
    stranger(Req.Advance(session, 8, Vector(Bounds(Long.MinValue, Long.MinValue)), 1)) match
      case Resp.Failed(why) => assert(why.contains("does not recognise"), why)
      case other => fail(s"a stranger advanced somebody else's session: $other")
    open(Req.Advance(session, 8, Vector(Bounds(Long.MinValue, Long.MinValue)), 1)) match
      case _: Resp.Epoch => ()
      case other => fail(s"the session's own coordinator was refused: $other")
    val _ = open(Req.Close(session))
  }
}