package okay.compress

/** compress-crypto-facades: the library's implementation beside ours,
 * the same formats — each reads the other's output */
class TestAircompressor extends munit.FunSuite:

  private val inputs: Vector[(String, Array[Byte])] = Samples.all :+ ("big" -> Samples.big)

  test("with no import the given is ours; the import picks aircompressor; both name themselves") {
    assertEquals(summon[Compression].name, "okay")
    locally {
      import Aircompressor.given
      assertEquals(summon[Compression].name, "aircompressor")
    }
    assertEquals((Aircompressor.lz4.name, Aircompressor.zstd.name), ("lz4", "zstd"))
  }

  test("ZSTD: aircompressor round-trips, ours reads its frames, it reads ours") {
    for (name, in) <- inputs do
      val theirs = Aircompressor.zstd.compress(in)
      assertEquals(Aircompressor.zstd.decompress(theirs).toVector, in.toVector, s"$name: their round trip")
      assertEquals(Zstd.decompress(theirs).toVector, in.toVector, s"$name: ours reads theirs")
      assertEquals(Aircompressor.zstd.decompress(Zstd.compress(in)).toVector, in.toVector, s"$name: theirs reads ours")
  }

  test("LZ4: our frame over aircompressor's blocks round-trips, ours reads it, it reads ours") {
    for (name, in) <- inputs do
      val theirs = Aircompressor.lz4.compress(in)
      assertEquals(Aircompressor.lz4.decompress(theirs).toVector, in.toVector, s"$name: their round trip")
      assertEquals(Lz4Frame.decompress(theirs).toVector, in.toVector, s"$name: ours reads theirs")
      assertEquals(Aircompressor.lz4.decompress(Lz4Frame.compress(in)).toVector, in.toVector, s"$name: theirs reads ours")
  }

  test("Snappy: aircompressor round-trips, ours reads its blocks, it reads ours") {
    for (name, in) <- inputs do
      val theirs = Aircompressor.snappy.compress(in)
      assertEquals(Aircompressor.snappy.decompress(theirs).toVector, in.toVector, s"$name: their round trip")
      assertEquals(Snappy.decompress(theirs).toVector, in.toVector, s"$name: ours reads theirs")
      assertEquals(Aircompressor.snappy.decompress(Snappy.compress(in)).toVector, in.toVector, s"$name: theirs reads ours")
      // the ratio beside the library's, per sample: a greedy matcher that
      // missed most matches would still round-trip
      assert(Snappy.compress(in).length <= theirs.length * 13 / 10 + 16,
        s"$name: ours ${Snappy.compress(in).length} bytes against aircompressor's ${theirs.length}")
    locally {
      import Aircompressor.given
      assertEquals(summon[Compression].snappy.name, "snappy")
      assert(!(summon[Compression].snappy eq Snappy), "the import did not pick aircompressor's")
    }
  }

  test("pyarrow's frames read through aircompressor too") {
    val b64 = java.util.Base64.getDecoder
    assertEquals(Aircompressor.zstd.decompress(b64.decode(Fixtures.zstd_text_3)).toVector, Fixtures.input("text").toVector)
    assertEquals(Aircompressor.lz4.decompress(b64.decode(Fixtures.lz4_text)).toVector, Fixtures.input("text").toVector)
  }

  test("a cut frame is refused by name, as a Corrupt, on both codecs") {
    val z = Aircompressor.zstd.compress(Samples.big)
    val _ = intercept[Corrupt](Aircompressor.zstd.decompress(z.dropRight(z.length / 3)))
    val l = Aircompressor.lz4.compress(Samples.big)
    val _ = intercept[Corrupt](Aircompressor.lz4.decompress(l.dropRight(l.length / 3)))
  }

  test("by name, and the refusal names the jar to add") {
    assertEquals(Compressions.byName("okay").map(_.name), Right("okay"))
    assertEquals(Compressions.byName("aircompressor").map(_.name), Right("aircompressor"))
    assertEquals(Compressions.byName("zlib"), Left("unknown compression implementation 'zlib' (okay, aircompressor)"))
    assertEquals(Aircompressor.missing(), None)
    val why = Aircompressor.missing("io.airlift.compress.NoSuchClass").getOrElse(fail("a missing class went unnoticed"))
    assert(why.contains("io.airlift:aircompressor:2.0.3") && why.contains("Compression.Okay"), why)
  }
