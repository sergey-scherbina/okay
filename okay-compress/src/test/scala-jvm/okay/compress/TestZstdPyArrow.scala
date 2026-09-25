package okay.compress

/** stages 2 and 3 against pyarrow's ZSTD (Live) */
class TestZstdPyArrow extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = PyArrow.python.isEmpty

  private val samples = Samples.all.filter(_._2.nonEmpty) :+ ("big" -> Samples.big)

  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  for level <- Vector(1, 3, 9, 19) do
    test(s"pyarrow's ZSTD at level $level decompresses here, sample for sample") {
      // level 19 spends 13 s of pyarrow's own time on the 12 MB sample: left out there
      val these = if level >= 19 then samples.filter(_._1 != "big") else samples
      val theirs = PyArrow.zstd(these.map(_._2), level)
      these.zip(theirs).foreach { case ((name, b), c) =>
        val got = scala.util.Try(Zstd.decompress(c))
        assert(got.toOption.exists(java.util.Arrays.equals(_, b)), s"$name: ${got.failed.map(_.getMessage).getOrElse("different bytes")}")
      }
    }

  test("our ZSTD frames decompress in pyarrow, sample for sample") {
    val back = PyArrow("decompress", "zstd", samples.map((_, b) => Zstd.compress(b)), samples.map(_._2.length.toString))
    samples.zip(back).foreach { case ((name, b), d) => assert(java.util.Arrays.equals(d, b), name) }
  }
