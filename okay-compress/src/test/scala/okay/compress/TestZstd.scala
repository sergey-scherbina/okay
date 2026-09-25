package okay.compress

/** stages 2 and 3 on every platform: our frames round-trip, refusals */
class TestZstd extends munit.FunSuite:

  test("ZSTD frames round-trip every sample") {
    for (name, b) <- Samples.all do
      assertEquals(Zstd.decompress(Zstd.compress(b)).toVector, b.toVector, name)
  }

  test("a frame of many 128 KiB blocks round-trips") {
    assertEquals(java.util.Arrays.equals(Zstd.decompress(Zstd.compress(Samples.big)), Samples.big), true)
  }

  test("a frame cut short, or with a byte flipped, is refused by name") {
    val c = Zstd.compress(Samples.all.collectFirst { case ("text", b) => b }.get)
    val cut = (1 until c.length by 11).filter(n => scala.util.Try(Zstd.decompress(c.dropRight(n))).isSuccess)
    assertEquals(cut.toVector, Vector.empty)
    val flipped = (4 until c.length by 13).filter { i =>
      val d = c.clone(); d(i) = (d(i) ^ 0x41).toByte
      scala.util.Try(Zstd.decompress(d)).isSuccess
    }
    assertEquals(flipped.toVector, Vector.empty)
    assert(intercept[Corrupt](Zstd.decompress("zstd?".getBytes("UTF-8"))).getMessage.startsWith("not a ZSTD frame this reads"))
  }
