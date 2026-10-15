package okay.cluster

import okay.Aggregator
import okay.codec.Schema

/**
 * THE LEAK, READ (specs/federation.md, stage 4) — over real partial
 * Schemas rather than invented ones, so the two flags land on shapes
 * this repository's own jobs actually produce.
 */
class TestLeak extends munit.FunSuite {
  import Feeds.*

  // ── a fold: unkeyed, one value per PARTITION ─────────────────────

  test("Wire.fold(count): one uncounted value, no key at all") {
    val r = Leak.of(Wire.fold(Aggregator.count[Ev]).wire)
    assertEquals(r.fields.length, 1)
    assertEquals(r.fields.head.role, Leak.Role.Value)
    assertEquals(r.fields.head.shape, "Long")
    assert(r.uncounted.nonEmpty, "a bare Long accumulator has nothing to say how many events made it")
    assertEquals(r.unboundedKeys, Vector.empty, "a fold has no key to flag")
  }

  test("Wire.fold with a COMPOUND accumulator is reported by name and not fragmented") {
    val agg = Aggregator[Ev, Sum, Sum](Sum(0, 0, 0))((s, e) =>
      Sum(s.n + 1, s.total + e.v, s.x))((a, b) => Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)
    val r = Leak.of(Wire.fold(agg).wire)
    // ONE field naming the shape — not `n`/`total`/`x` as three
    // separate leaks. Whether Sum's own fields answer "how many" is
    // a question about what they mean, which this tool does not
    // decide; it stops at the shape.
    assertEquals(r.fields, Vector(Leak.Field("", "Sum", Leak.Role.Value)))
    assertEquals(r.uncounted, Vector.empty, "a compound value is not flagged uncounted — only a bare scalar is")
  }

  // ── a keyed sink: the exact shape this repository ships ─────────

  test("Wire.keyed with a bare Long accumulator — the shape this codebase's own TestJobs.value uses") {
    val w = Wire.keyed((e: Ev) => e.key, value)(
      Aggregator[(Int, Long), Long, Long](0L)((s, kv) => s + kv._2)((a, b) => a + b)(identity))
    val r = Leak.of(w.wire)
    assertEquals(r.fields.length, 2)
    val keyF = r.fields.find(_.role == Leak.Role.Key).get
    assertEquals(keyF.shape, "Int")
    val valF = r.fields.find(_.role == Leak.Role.Value).get
    assertEquals(valF.shape, "Long")
    // THE FINDING: this is a REAL shape in this repository (TestJobs's
    // `value` aggregator), and it IS uncounted — a key seen once
    // hands over that record's own field, and nothing in the schema
    // says so.
    assertEquals(r.uncounted.map(_.path), Vector(valF.path),
      "a bare Long per key must be flagged: a group of one is that record's own value")
    assertEquals(r.unboundedKeys, Vector.empty, "an Int key is bounded — not flagged")
  }

  test("Wire.keyed with a STRING key is flagged unbounded; an Int key is not") {
    final case class Row(country: String, amount: Long) derives Schema
    val w = Wire.keyed((r: Row) => r.country, Aggregator.sum[Long].contramap[Row](_.amount))(
      Aggregator[(String, Long), Long, Long](0L)((s, kv) => s + kv._2)((a, b) => a + b)(identity))
    val r = Leak.of(w.wire)
    val keyF = r.fields.find(_.role == Leak.Role.Key).get
    assertEquals(keyF.shape, "String")
    assertEquals(r.unboundedKeys.map(_.path), Vector(keyF.path),
      "a String key has no bound on cardinality and must be flagged")
  }

  test("a compound accumulator crossing PER KEY is named, not fragmented, and not flagged uncounted") {
    // the accumulator per key IS `Sum` here (not the terminal `into`'s
    // accumulator) — `Wire.keyed`'s wire is built from the FIRST
    // aggregator's Acc, which is what actually crosses per partition
    // per key
    val perKey = Aggregator[Ev, Sum, Sum](Sum(0, 0, 0))((s, e) =>
      Sum(s.n + 1, s.total + e.v, s.x))((a, b) => Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)
    val into = Aggregator[(Int, Sum), Long, Long](0L)((s, _) => s + 1)((a, b) => a + b)(identity)
    val w = Wire.keyed((e: Ev) => e.key, perKey)(into)
    val r = Leak.of(w.wire)
    val valF = r.fields.find(_.role == Leak.Role.Value).get
    assertEquals(valF.shape, "Sum")
    assertEquals(r.uncounted, Vector.empty,
      "Sum(n, total, x) names its own count field — a reader can look, and is not told 'nothing to check'")
  }

  // ── a windowed sink: two groups (open boundary panes) plus a summary ──

  test("Wire.windowed: the boundary panes are a keyed group, `finished`/`late` are unkeyed and un-fragmented") {
    val w = Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
    val r = Leak.of(w.wire)
    // boundary: Vector[Triple(Long window-start, Int key, Long acc)]
    val keys = r.fields.filter(f => f.role == Leak.Role.Key && f.path.startsWith("boundary"))
    assertEquals(keys.map(_.shape).toSet, Set("Long", "Int"),
      s"the window start and the pane key should both be reported as Key: ${r.render}")
    val boundaryValue = r.fields.find(f => f.role == Leak.Role.Value && f.path.startsWith("boundary")).get
    assertEquals(boundaryValue.shape, "Long", s"the boundary's own accumulator: ${r.render}")
    // `finished` is IAcc = Sum — ONE field, not fragmented into its
    // own n/total/x
    assertEquals(r.fields.count(_.path.startsWith("finished")), 1, r.render)
    val finished = r.fields.find(_.path == "finished").get
    assertEquals(finished.role, Leak.Role.Value)
    assertEquals(finished.shape, "Sum")
    // `late` is a plain unkeyed Long
    val late = r.fields.find(_.path == "late").get
    assertEquals(late.role, Leak.Role.Value)
    assertEquals(late.shape, "Long")
    // exactly the boundary's bare accumulator and `late` are
    // uncounted — `finished` (compound) is not
    assertEquals(r.uncounted.map(_.path).toSet, Set(boundaryValue.path, "late"), r.render)
  }

  // ── the render, so an operator gets a page and not a data structure ──

  test("render prints the flags, and a clean job says so") {
    val dirty = Leak.of(Wire.fold(Aggregator.count[Ev]).wire).render
    assert(dirty.contains("UNCOUNTED"), dirty)

    val perKey = Aggregator[Ev, Sum, Sum](Sum(0, 0, 0))((s, e) =>
      Sum(s.n + 1, s.total + e.v, s.x))((a, b) => Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)
    val clean = Leak.of(Wire.keyed((e: Ev) => e.key, perKey)(
      Aggregator[(Int, Sum), Long, Long](0L)((s, _) => s + 1)((a, b) => a + b)(identity)).wire
    ).render
    assert(!clean.contains("UNCOUNTED"), clean)
    assert(!clean.contains("UNBOUNDED"), clean)
  }

  // ── the actual federation fixture, not a synthetic shape ────────

  test("Leak.of(PartyJob's own wire) reports the real finding TestFederation ships with") {
    // specs/federation.md stage 1's own job, `Party.scala` — a
    // windowed sink over a bare Long accumulator. This is not a
    // shape built to make a point; it is the one this repository
    // actually federates over, and it is uncounted for the same
    // reason `Wire.keyed`'s test above is.
    val r = Leak.of(PartyJob.sink(Feed(20000, Late - 1)).wire)
    assert(r.uncounted.exists(_.path.startsWith("boundary")),
      s"the boundary's per-key accumulator (a bare Long) should be uncounted:\n${r.render}")
  }

  // ── `and`: two sinks, two independent reports over one wire ──────

  test("and's wire reports BOTH sides' fields, under both paths") {
    val w = Wire.fold(Aggregator.count[Ev]).and(Wire.keyed((e: Ev) => e.key, value)(
      Aggregator[(Int, Long), Long, Long](0L)((s, kv) => s + kv._2)((a, b) => a + b)(identity)))
    val r = Leak.of(w.wire)
    // the fold's own scalar (`_1`), plus the keyed side's key and
    // value under `_2` — `and`'s Pair is a Wire.pair ENVELOPE, so it
    // is opened rather than reported as one opaque blob, and each
    // side is walked on its own terms
    assertEquals(r.fields.map(f => f.path -> f.role).toSet,
      Set("_1" -> Leak.Role.Value, "_2._1" -> Leak.Role.Key, "_2._2" -> Leak.Role.Value),
      r.render)
  }
}
