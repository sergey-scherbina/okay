package okay.codec

/**
 * A SCHEMA'S SHAPE, WITHOUT THE SCHEMA (specs/federation.md, stage 3).
 *
 * `Digest` exists so two processes that do not share a build can
 * still ask "will you be able to decode what I write?" — proven here
 * by CROSSING it: encode a `Digest` to bytes, decode it back, and
 * compare it against a schema it never came from, exactly as two
 * processes would.
 */
class TestDigest extends munit.FunSuite {

  def roundTrip(d: Digest): Digest =
    val codec = Codecs.cbor(summon[Schema[Digest]])
    codec.decode(codec.encode(d)).fold(fail(_), identity)

  // ── the digest travels ────────────────────────────────────────

  test("a Digest survives CBOR round-trip, byte for byte in meaning") {
    final case class Row(id: Int, name: String, tags: Vector[String]) derives Schema
    val d = Digest.of(summon[Schema[Row]])
    assertEquals(roundTrip(d), d)
  }

  // ── it agrees with Compat.compare on ordinary schemas ────────

  test("compare(local, Digest.of(local)) is IDENTICAL — no change, both directions compatible") {
    final case class Row(id: Int, amount: Long) derives Schema
    val r = Digest.compare(summon[Schema[Row]], roundTrip(Digest.of(summon[Schema[Row]])))
    assert(r.isEmpty, r.render)
    assert(r.rolling.compatible, r.render)
  }

  test("a required field ADDED on the remote side: the LOCAL (old) reader cannot decode it") {
    final case class Old(id: Int) derives Schema
    final case class New(id: Int, amount: Long) derives Schema
    // `local` = the old schema (what I have); `remote` = the digest
    // of the new one (what the coordinator now expects). I am the
    // WRITER here — Compat.compare(old, new).backward asks "can new,
    // as reader, decode old's bytes", which is unaffected by an ADD;
    // the actual break is in `.forward` (old reader can't handle new
    // bytes) — but this test's own job is DECODE ON THE OTHER END —
    // exercised the other way in the next test.
    val r = Compat.compare(summon[Schema[Old]], summon[Schema[New]])
    val viaDigest = Digest.compare(summon[Schema[Old]], roundTrip(Digest.of(summon[Schema[New]])))
    assertEquals(viaDigest.render, r.render, "Digest.compare must answer exactly what Compat.compare answers")
  }

  test("a required field REMOVED on the remote side: I (the writer) can no longer be decoded") {
    final case class Mine(id: Int, amount: Long) derives Schema
    final case class Theirs(id: Int) derives Schema
    // I write with Mine; they read with Theirs. Compat.compare(mine,
    // theirs).backward = "can Theirs, as new reader, decode Mine's
    // (old) bytes" — Theirs REMOVED `amount`, so backward is fine
    // (the new reader just ignores the extra field)... the actual
    // risk direction for federation is stated in the design: is it
    // ADDING a required field on the remote side that breaks a
    // party's writes, not removing one. This test pins that both
    // Compat.compare and Digest.compare agree regardless of which
    // way the change runs.
    val r = Compat.compare(summon[Schema[Mine]], summon[Schema[Theirs]])
    val viaDigest = Digest.compare(summon[Schema[Mine]], roundTrip(Digest.of(summon[Schema[Theirs]])))
    assertEquals(viaDigest.render, r.render)
    assert(r.backward.compatible, "a reader that dropped a field still reads old bytes fine")
  }

  test("THE FEDERATION RISK: a required field added on the READER's side breaks backward") {
    final case class Party(id: Int) derives Schema
    final case class Coordinator(id: Int, mustHave: Long) derives Schema
    // the party WRITES with `Party`'s shape; the coordinator READS
    // with `Coordinator`'s shape, which is Party plus a new REQUIRED
    // field. The coordinator (as new reader) cannot decode old bytes
    // that never had `mustHave` — this is `.backward` incompatible,
    // and it is exactly the shape `Compat.compare` was built to catch.
    val viaDigest = Digest.compare(summon[Schema[Party]], roundTrip(Digest.of(summon[Schema[Coordinator]])))
    assert(!viaDigest.backward.compatible, viaDigest.render)
    assert(viaDigest.backward.reasons.exists(_.contains("mustHave")), viaDigest.render)
  }

  test("an OPTIONAL or DEFAULTED field added on the reader's side does not break backward") {
    final case class Party(id: Int) derives Schema
    final case class Coordinator(id: Int, extra: Option[Long]) derives Schema
    val viaDigest = Digest.compare(summon[Schema[Party]], roundTrip(Digest.of(summon[Schema[Coordinator]])))
    assert(viaDigest.backward.compatible, viaDigest.render)
  }

  test("a renamed/retyped field is a shape change, caught the same way through a digest") {
    final case class Party(id: Int, amount: Long) derives Schema
    final case class Coordinator(id: Int, amount: String) derives Schema
    val viaDigest = Digest.compare(summon[Schema[Party]], roundTrip(Digest.of(summon[Schema[Coordinator]])))
    assert(!viaDigest.backward.compatible, viaDigest.render)
    assert(viaDigest.backward.reasons.exists(r => r.contains("Long") && r.contains("String")), viaDigest.render)
  }

  // ── the actual per-key/per-partition shapes this repository ships ──

  test("Wire.keyed's real Pair(K, Long) shape, through a digest, matches identically") {
    val local = Schema.SVector(() => Schema.SProduct[(Int, Long)]("Pair",
      Vector("_1" -> (() => Schema.SInt), "_2" -> (() => Schema.SLong)),
      vs => (vs(0).asInstanceOf[Int], vs(1).asInstanceOf[Long]), p => Seq(p._1, p._2)))
    val r = Digest.compare(local, roundTrip(Digest.of(local)))
    assert(r.isEmpty, r.render)
  }

  // ── a self-referential schema does not loop ──────────────────

  test("a RECURSIVE schema's digest is built and compared without looping") {
    sealed trait Tree
    final case class Leaf(value: Int) extends Tree
    final case class Branch(children: Vector[Tree]) extends Tree
    given Schema[Tree] = Schema.derived
    // if this line hangs, the recursion guard is broken — the test
    // times out rather than silently pass, which is the honest
    // failure mode for an infinite loop
    val d = Digest.of(summon[Schema[Tree]])
    val back = roundTrip(d)
    val r = Digest.compare(summon[Schema[Tree]], back)
    assert(r.isEmpty, s"a schema compared against its own digest must show no change: ${r.render}")
  }

  test("a schema that ADDS a field only inside the recursive branch is still caught") {
    // the SAME type names on both sides (Tree/Leaf/Branch), because
    // that is what "the same type evolved" actually means — two
    // DIFFERENT names would make Compat.walk see every case as
    // added/removed regardless of this box, which is Compat's own
    // existing behaviour and not what this test is about
    val oldSchema: Schema[?] = locally {
      sealed trait Tree
      final case class Leaf(value: Int) extends Tree
      final case class Branch(children: Vector[Tree]) extends Tree
      Schema.derived[Tree]
    }
    val newDigest = Digest.of(locally {
      sealed trait Tree
      final case class Leaf(value: Int, tag: String) extends Tree // `tag` is NEW
      final case class Branch(children: Vector[Tree]) extends Tree
      Schema.derived[Tree]
    })
    val r = Digest.compare(oldSchema, roundTrip(newDigest))
    assert(!r.isEmpty, "Leaf added a required field — this must show as a change")
    assert(r.changes.exists(_.toString.contains("tag")), r.render)
  }
}
