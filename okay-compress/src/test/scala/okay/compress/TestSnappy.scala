package okay.compress

/** stage 6: raw Snappy, ours, on every platform */
class TestSnappy extends munit.FunSuite:

  test("every sample round-trips, and the repetitive ones shrink") {
    for (name, in) <- Samples.all :+ ("big" -> Samples.big) do
      val z = Snappy.compress(in)
      assertEquals(Snappy.decompress(z).toVector, in.toVector, name)
    for name <- Vector("a run", "a short period", "text", "numbers") do
      val in = Samples.all.find(_._1 == name).get._2
      assert(Snappy.compress(in).length < in.length, s"$name did not shrink")
  }

  test("the facade's default is ours") {
    assertEquals(summon[Compression].snappy.name, "snappy")
    assert(summon[Compression].snappy eq Snappy)
  }

  test("a damaged block is refused as Corrupt, never an index error") {
    val z = Snappy.compress(Samples.big)
    val _ = intercept[Corrupt](Snappy.decompress(z.dropRight(z.length / 3)))
    // a copy reaching before the start: length 8, then a 2-byte-offset copy 5 back
    val _ = intercept[Corrupt](Snappy.decompress(Array[Byte](8, (2 | (3 << 2)).toByte, 5, 0)))
    // declares 100 bytes, holds a 3-byte literal
    val _ = intercept[Corrupt](Snappy.decompress(Array[Byte](100, 8, 1, 2, 3)))
  }
