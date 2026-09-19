package okay.cluster

import okay.codec.{Codecs, Digest, Schema}
import okay.given

/**
 * SCHEMA AT THE DOOR (specs/federation.md, stage 3).
 *
 * `Cluster.guarded` refuses a stranger before a request reaches the
 * job; this refuses a MISMATCHED SCHEMA before it reaches the job,
 * the same shape applied to the wire instead of the caller. Two
 * parties running the "same" job by name can still disagree about
 * what its partial looks like — one has rebuilt, one has not — and
 * without this box that disagreement surfaces as a raw decode
 * failure, or worse, silently wrong numbers, deep inside the
 * COORDINATOR after the party has already done the work.
 */
class TestSchemaDoor extends munit.FunSuite {
  import Feeds.*

  Party.install()
  val feed: Feed = Feed(2000, Late - 1)
  val encoded: Array[Byte] = Codecs.cbor(PartyJob.params).encode(feed)

  def digestOf(s: Schema[?]): Array[Byte] = Codecs.cbor(summon[Schema[Digest]]).encode(Digest.of(s))

  val realDigest: Array[Byte] = digestOf(PartyJob.sink(feed).wire)

  // ── the refusal, and what it names ───────────────────────────

  test("a coordinator whose digest disagrees with the party's real schema is refused — before extentAt runs") {
    // a schema the coordinator (wrongly) expects: the SAME top-level
    // shape (a windowed Handed) is not needed — any digest that
    // Compat finds incompatible in the BACKWARD direction proves the
    // point. A totally different primitive is the sharpest case.
    final case class Wrong(nope: String) derives Schema
    val bad = digestOf(summon[Schema[Wrong]])
    val checked = Cluster.schemaChecked(Party.as(0))
    checked(Req.Extent(PartyJob.name, encoded, 0, 1, bad)) match
      case Resp.Failed(why) =>
        assert(why.contains(PartyJob.name), why)
        assert(why.contains("cannot decode"), why)
      case other => fail(s"a mismatched schema was not refused: $other")
  }

  test("the SAME check refuses Req.Run and Req.Open too — every door a job's name opens") {
    final case class Wrong(nope: String) derives Schema
    val bad = digestOf(summon[Schema[Wrong]])
    val checked = Cluster.schemaChecked(Party.as(0))
    checked(Req.Run(PartyJob.name, encoded, 0, 1, Vector.empty, bad)) match
      case Resp.Failed(why) => assert(why.contains("cannot decode"), why)
      case other => fail(s"Req.Run was not refused: $other")
    checked(Req.Open(PartyJob.name, encoded, 0, 1, 999L, digest = bad)) match
      case Resp.Failed(why) => assert(why.contains("cannot decode"), why)
      case other => fail(s"Req.Open was not refused: $other")
  }

  test("NOTHING RUNS: the party's own log is never read for a refused schema") {
    // the same rule stage 1 and stage 2 both already established —
    // the refusal comes BEFORE any read, not after a wasted one
    final case class Wrong(nope: String) derives Schema
    val bad = digestOf(summon[Schema[Wrong]])
    val checked = Cluster.schemaChecked(Party.as(0))
    val before = Party.log(0, feed, 1).records.get
    val _ = checked(Req.Extent(PartyJob.name, encoded, 0, 1, bad))
    assertEquals(Party.log(0, feed, 1).records.get, before, "the log was read despite the refusal")
  }

  // ── the real digest, and no digest at all, both pass through ──

  test("a MATCHING digest is not refused — the ordinary answer comes back") {
    val checked = Cluster.schemaChecked(Party.as(0))
    checked(Req.Extent(PartyJob.name, encoded, 0, 1, realDigest)) match
      case _: Resp.Extents => ()
      case other => fail(s"a matching schema was refused: $other")
  }

  test("an EMPTY digest skips the check entirely — a coordinator built before this box is unaffected") {
    val checked = Cluster.schemaChecked(Party.as(0))
    checked(Req.Extent(PartyJob.name, encoded, 0, 1)) match
      case _: Resp.Extents => ()
      case other => fail(s"an empty digest was treated as a mismatch: $other")
  }

  // ── OPT IN, like `guarded`: unwrapped, nothing changes ────────

  test("without `schemaChecked`, a mismatched digest is simply never read — full runs are unaffected") {
    // the digest ALWAYS rides along (Cluster.run/stream attach it
    // unconditionally), and a party that never opts into checking it
    // pays nothing and refuses nothing — proven by a REAL run
    val got = Cluster.stream(PartyJob, feed, 2, Vector(Party.as(0), Party.as(1)), 64).runWith
    val reference = Flows.fan(Flow.slices(events(feed), 2), PartyJob.sink(feed)).runWith
    assertEquals(got.value, reference.value)
  }

  // ── an unknown job is left to the ordinary answer ─────────────

  test("a job not found is left to the downstream 'no job named' answer, not duplicated here") {
    val checked = Cluster.schemaChecked(Party.as(0))
    checked(Req.Extent("no.such.job", encoded, 0, 1, realDigest)) match
      case Resp.Failed(why) => assert(why.contains("no job named"), why)
      case other => fail(s"unexpected: $other")
  }

  // ── composes with `guarded`: two independent doors ────────────

  test("schemaChecked composes with guarded — a stranger is still refused first, by identity") {
    final case class Wrong(nope: String) derives Schema
    val bad = digestOf(summon[Schema[Wrong]])
    val guardedAndChecked = Cluster.guarded(Set(PartyJob.name), Set("the-hospital"))("a-stranger")(
      Cluster.schemaChecked(Party.as(0)))
    guardedAndChecked(Req.Extent(PartyJob.name, encoded, 0, 1, bad)) match
      case Resp.Failed(why) => assert(why.contains("does not recognise"), why)
      case other => fail(s"a stranger reached the schema check: $other")
  }
}
