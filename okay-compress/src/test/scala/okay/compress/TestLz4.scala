package okay.compress

/** stage 1 on every platform: xxHash's reference values, LZ4 round trips,
 * refusals */
class TestLz4 extends munit.FunSuite:

  test("xxHash32 and xxHash64: the reference values") {
    val abc = "abc".getBytes("UTF-8")
    assertEquals(XxHash.xxh32(Array.emptyByteArray, 0, 0), 0x02cc5d05)
    assertEquals(XxHash.xxh32(abc, 0, 3), 0x32d153ff)
    assertEquals(XxHash.xxh64(Array.emptyByteArray, 0, 0), 0xef46db3751d8e999L)
    assertEquals(XxHash.xxh64(abc, 0, 3), 0x44bc2cf5ad770999L)
  }

  test("LZ4 frames round-trip every sample, and compress what repeats") {
    for (name, b) <- Samples.all do
      val c = Lz4Frame.compress(b)
      assertEquals(Lz4Frame.decompress(c).toVector, b.toVector, name)
    val run = Samples.all.collectFirst { case ("a run", b) => b }.get
    assert(Lz4Frame.compress(run).length < run.length / 100, "a run of one byte must shrink a hundredfold")
  }

  test("a frame of several 4 MiB blocks round-trips") {
    val c = Lz4Frame.compress(Samples.big)
    assert(Samples.big.length > (4 << 20))
    assertEquals(java.util.Arrays.equals(Lz4Frame.decompress(c), Samples.big), true)
  }

  test("a block round-trips on its own, into a buffer of the original size") {
    for (name, b) <- Samples.all do
      // at an offset, so a position cannot pass for a length
      val dst = new Array[Byte](7 + Lz4Block.bound(b.length))
      val n = Lz4Block.compress(b, 0, b.length, dst, 7)
      val back = new Array[Byte](b.length)
      assertEquals(Lz4Block.decompress(dst, 7, n, back, 0, b.length), b.length, name)
      assertEquals(back.toVector, b.toVector, name)
  }

  test("a frame cut short, at any byte, or with a byte flipped, is refused by name") {
    val c = Lz4Frame.compress(Samples.all.collectFirst { case ("text", b) => b }.get)
    val cut = (1 until c.length by 3).filter(n => scala.util.Try(Lz4Frame.decompress(c.dropRight(n))).isSuccess)
    assertEquals(cut.toVector, Vector.empty)
    val flipped = (4 until c.length by 5).filter { i =>
      val d = c.clone(); d(i) = (d(i) ^ 0x21).toByte
      scala.util.Try(Lz4Frame.decompress(d)).isSuccess
    }
    assertEquals(flipped.toVector, Vector.empty)
    val e = intercept[Corrupt](Lz4Frame.decompress("not lz4".getBytes("UTF-8")))
    assert(e.getMessage.startsWith("not an LZ4 frame this reads"), e.getMessage)
  }
