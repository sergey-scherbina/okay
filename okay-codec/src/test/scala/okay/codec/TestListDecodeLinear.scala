package okay.codec

/**
 * A List decodes in linear time (remote-arrow-frames, 2026-09-25): CBOR's
 * and JSON's direct decoders appended to a List with `:+`, one copy per
 * element, and 16 000 records took 2.4 s against 13.5 ms as a Vector —
 * found by okay-cluster's Remote, which sends List[A]. The ratio to the
 * Vector read, not a time: a loaded box slows both alike, and the
 * quadratic one was off by a hundredfold, not by twenty.
 */
class TestListDecodeLinear extends munit.FunSuite:
  final case class Rec(id: Long, name: String) derives Schema

  private def best(f: => Any): Long =
    (1 to 3).map { _ => val t = System.nanoTime(); val _ = f; System.nanoTime() - t }.min

  test("CBOR and JSON read a 40 000-element List about as fast as the same Vector") {
    val xs = List.tabulate(40000)(i => Rec(i.toLong, s"r$i"))
    val cl = Cbor.write(xs); val cv = Cbor.write(xs.toVector)
    val jl = Json.encode(summon[Schema[List[Rec]]])(xs); val jv = Json.encode(summon[Schema[Vector[Rec]]])(xs.toVector)
    assertEquals(Cbor.read[List[Rec]](cl), Right(xs))
    assertEquals(Json.decode(summon[Schema[List[Rec]]])(Json.parse(jl)), Right(xs))
    val cborRatio = best(Cbor.read[List[Rec]](cl)).toDouble / best(Cbor.read[Vector[Rec]](cv))
    val jsonRatio = best(Json.decode(summon[Schema[List[Rec]]])(Json.parse(jl))).toDouble /
      best(Json.decode(summon[Schema[Vector[Rec]]])(Json.parse(jv)))
    assert(cborRatio < 20, f"CBOR: a List read $cborRatio%.1fx the Vector read")
    assert(jsonRatio < 20, f"JSON: a List read $jsonRatio%.1fx the Vector read")
  }
