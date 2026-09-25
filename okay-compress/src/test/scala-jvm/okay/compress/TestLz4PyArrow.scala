package okay.compress

/** stage 1 against pyarrow's LZ4 (Live: needs a python with pyarrow) */
class TestLz4PyArrow extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = PyArrow.python.isEmpty

  private val samples = Samples.all.filter(_._2.nonEmpty) :+ ("big" -> Samples.big)

  test("pyarrow's LZ4 frames decompress here, sample for sample") {
    val theirs = PyArrow("compress", "lz4", samples.map(_._2))
    samples.zip(theirs).foreach { case ((name, b), c) =>
      assert(java.util.Arrays.equals(Lz4Frame.decompress(c), b), name)
    }
  }

  test("our LZ4 frames decompress in pyarrow, sample for sample") {
    val back = PyArrow("decompress", "lz4", samples.map((_, b) => Lz4Frame.compress(b)), samples.map(_._2.length.toString))
    samples.zip(back).foreach { case ((name, b), d) => assert(java.util.Arrays.equals(d, b), name) }
  }

  test("raw LZ4 blocks cross both ways") {
    val small = samples.filter(_._2.length < (4 << 20))
    val theirs = PyArrow("compress", "lz4_raw", small.map(_._2))
    small.zip(theirs).foreach { case ((name, b), c) =>
      val out = new Array[Byte](b.length)
      assertEquals(Lz4Block.decompress(c, 0, c.length, out, 0, b.length), b.length, name)
      assert(java.util.Arrays.equals(out, b), name)
    }
    val ours = small.map { (_, b) =>
      val dst = new Array[Byte](Lz4Block.bound(b.length))
      java.util.Arrays.copyOf(dst, Lz4Block.compress(b, 0, b.length, dst, 0))
    }
    val back = PyArrow("decompress", "lz4_raw", ours, small.map(_._2.length.toString))
    small.zip(back).foreach { case ((name, b), d) => assert(java.util.Arrays.equals(d, b), name) }
  }
