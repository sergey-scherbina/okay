package okay2.codec

sealed trait DgTree
object DgTree {
  final case class Leaf(value: Int) extends DgTree
  final case class Branch(children: Vector[DgTree]) extends DgTree
  implicit lazy val schema: Schema[DgTree] = Schema.derived
}

/** the same type names on both sides, as "the same type evolved" means:
 * `Leaf` gains a required field inside the recursive branch */
object DgOld {
  sealed trait Tree
  final case class Leaf(value: Int) extends Tree
  final case class Branch(children: Vector[Tree]) extends Tree
  implicit lazy val schema: Schema[Tree] = Schema.derived
}
object DgNew {
  sealed trait Tree
  final case class Leaf(value: Int, tag: String) extends Tree
  final case class Branch(children: Vector[Tree]) extends Tree
  implicit lazy val schema: Schema[Tree] = Schema.derived
}

/**
 * A schema's shape, without the schema (okay-codec's TestDigest): a
 * `Digest` is encoded to bytes, decoded back, and compared against a
 * schema it never came from, exactly as two processes would.
 */
class TestDigest extends munit.FunSuite {

  private def roundTrip(d: Digest): Digest = {
    val codec = Codecs.cbor(Digest.schema)
    codec.decode(codec.encode(d)).fold(fail(_), identity)
  }

  test("a Digest survives CBOR round-trip, byte for byte in meaning, and JSON too") {
    final case class Row(id: Int, name: String, tags: Vector[String])
    val d = Digest.of(Schema.derived[Row])
    assertEquals(roundTrip(d), d)
    assertEquals(Json.read[Digest](Json.write(d)), Right(d))
  }

  test("compare(local, Digest.of(local)) is IDENTICAL — no change, both directions compatible") {
    final case class Row(id: Int, amount: Long)
    val row: Schema[Row] = Schema.derived
    val r = Digest.compare(row, roundTrip(Digest.of(row)))
    assert(r.isEmpty, r.render)
    assert(r.rolling.compatible, r.render)
  }

  test("a required field ADDED on the remote side: Digest.compare answers what Compat.compare answers") {
    final case class Old(id: Int)
    final case class New(id: Int, amount: Long)
    val old: Schema[Old] = Schema.derived
    val next: Schema[New] = Schema.derived
    assertEquals(Digest.compare(old, roundTrip(Digest.of(next))).render, Compat.compare(old, next).render)
  }

  test("a required field REMOVED on the remote side: the two agree, and backward is fine") {
    final case class Mine(id: Int, amount: Long)
    final case class Theirs(id: Int)
    val mine: Schema[Mine] = Schema.derived
    val theirs: Schema[Theirs] = Schema.derived
    val r = Compat.compare(mine, theirs)
    assertEquals(Digest.compare(mine, roundTrip(Digest.of(theirs))).render, r.render)
    assert(r.backward.compatible)
  }

  test("THE FEDERATION RISK: a required field added on the READER's side breaks backward") {
    final case class Party(id: Int)
    final case class Coordinator(id: Int, mustHave: Long)
    val viaDigest = Digest.compare(Schema.derived[Party], roundTrip(Digest.of(Schema.derived[Coordinator])))
    assert(!viaDigest.backward.compatible, viaDigest.render)
    assert(viaDigest.backward.reasons.exists(_.contains("mustHave")), viaDigest.render)
  }

  test("an OPTIONAL or DEFAULTED field added on the reader's side does not break backward") {
    final case class Party(id: Int)
    final case class Coordinator(id: Int, extra: Option[Long])
    final case class Defaulted(id: Int, extra: Long = 0L)
    assert(Digest.compare(Schema.derived[Party], roundTrip(Digest.of(Schema.derived[Coordinator]))).backward.compatible)
    assert(Digest.compare(Schema.derived[Party], roundTrip(Digest.of(Schema.derived[Defaulted]))).backward.compatible)
  }

  test("a retyped field is a type change, caught the same way through a digest") {
    final case class Party(id: Int, amount: Long)
    final case class Coordinator(id: Int, amount: String)
    val viaDigest = Digest.compare(Schema.derived[Party], roundTrip(Digest.of(Schema.derived[Coordinator])))
    assert(!viaDigest.backward.compatible, viaDigest.render)
    assert(viaDigest.backward.reasons.exists(r => r.contains("Long") && r.contains("String")), viaDigest.render)
  }

  test("a hand-written Pair(K, Long) shape, through a digest, matches identically") {
    val local = Schema.SVector(() => Schema.SProduct[(Int, Long)]("Pair",
      Vector("_1" -> (() => Schema.SInt), "_2" -> (() => Schema.SLong)),
      vs => (vs(0) match { case i: Int => i; case _ => 0 }, vs(1) match { case l: Long => l; case _ => 0L }),
      p => Seq(p._1, p._2)))
    val r = Digest.compare(local, roundTrip(Digest.of(local)))
    assert(r.isEmpty, r.render)
  }

  test("a RECURSIVE schema's digest is built and compared without looping") {
    val back = roundTrip(Digest.of(DgTree.schema))
    val r = Digest.compare(DgTree.schema, back)
    assert(r.isEmpty, s"a schema compared against its own digest must show no change: ${r.render}")
  }

  test("a schema that ADDS a field only inside the recursive branch is still caught") {
    val r = Digest.compare(DgOld.schema, roundTrip(Digest.of(DgNew.schema)))
    assert(!r.isEmpty, "Leaf added a required field — this must show as a change")
    assert(r.changes.exists(_.toString.contains("tag")), r.render)
  }
}
