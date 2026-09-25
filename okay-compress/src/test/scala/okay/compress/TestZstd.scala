package okay.compress

/** stages 2 and 3 on every platform: our frames round-trip, refusals */
class TestZstd extends munit.FunSuite:

  test("ZSTD frames round-trip every sample") {
    for (name, b) <- Samples.all do
      assertEquals(Zstd.decompress(Zstd.compress(b)).toVector, b.toVector, name)
  }

  test("every sample round-trips at levels 1, 3, 6 and 19: double fast below 4, the chain above") {
    for (name, b) <- Samples.all :+ ("big" -> Samples.big.take(1 << 20)); level <- Vector(1, 3, 6, 19) do
      assert(java.util.Arrays.equals(Zstd.decompress(ZstdEncoder.compress(b, level)), b), s"$name at level $level")
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

  test("frames pyarrow wrote decode on this platform: ZSTD at levels 3 and 19, the LZ4 frame format") {
    for (codec, name, b64) <- Fixtures.frames do
      val frame = java.util.Base64.getDecoder.decode(b64)
      val back = if codec == "zstd" then Zstd.decompress(frame) else Lz4Frame.decompress(frame)
      assertEquals(back.toVector, Fixtures.input(name).toVector, s"$codec $name")
  }

  test("short-offset matches (a period under 8 bytes) decode exactly, at every period and length") {
    for period <- 1 to 9; len <- Vector(3, 7, 8, 9, 15, 16, 17, 100, 1000) do
      val pattern = Array.tabulate(period)(i => ('a' + i).toByte)
      val b = Array.tabulate(period * 3 + len)(i => pattern(i % period)) ++ "tail!".getBytes("UTF-8")
      assertEquals(Zstd.decompress(Zstd.compress(b)).toVector, b.toVector, s"period $period, length $len")
      assertEquals(Lz4Frame.decompress(Lz4Frame.compress(b)).toVector, b.toVector, s"LZ4, period $period, length $len")
  }


  test("a damaged frame fails as Corrupt, never as an index past the output") {
    // sequence lengths are only trusted up to the block's 128 KiB: a flip
    // that makes one huge must be named, not run off the reserved room
    // ~450 decompressions: 1 500 took 43 s on Native in a loaded whole
    // build (30 s is the limit); the mutant without the check still fails
    val c = Zstd.compress(Samples.big.take(1 << 18))
    val wrong = (4 until c.length by (c.length / 150).max(1)).flatMap { i =>
      Vector(0x01, 0x10, 0x80).flatMap { bit =>
        val d = c.clone(); d(i) = (d(i) ^ bit).toByte
        scala.util.Try(Zstd.decompress(d)).failed.toOption.filterNot(_.isInstanceOf[Corrupt]).map(e => s"$i^$bit: $e")
      }
    }
    assertEquals(wrong.take(3).toVector, Vector.empty, s"${wrong.size} damaged frames failed some other way")
  }
