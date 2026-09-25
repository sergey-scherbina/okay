package okay.arrow

import okay.compress.{Aircompressor, Compression}

/** compress-crypto-facades: a compressed body reads under whichever
 * `Compression` is in scope, and each implementation reads the other's */
class TestArrowCompressionChoice extends munit.FunSuite:

  test("bodies compressed by ours read under aircompressor's given, and the other way round, ZSTD and LZ4") {
    val t = Tables.everything
    for (ours, theirs) <- Vector((okay.compress.Zstd, Aircompressor.zstd), (okay.compress.Lz4Frame, Aircompressor.lz4)) do
      val byOurs = OkayArrow.write(t, Some(ours))
      val byTheirs = OkayArrow.write(t, Some(theirs))
      assertEquals(Tables.same(t, OkayArrow.read(byTheirs)), None, s"${ours.name}: theirs, read by ours")
      locally {
        import Aircompressor.given
        assertEquals(summon[Compression].name, "aircompressor")
        assertEquals(Tables.same(t, OkayArrow.read(byOurs)), None, s"${ours.name}: ours, read by theirs")
        assertEquals(Tables.same(t, OkayArrow.readFileBatch(OkayArrow.writeFile(t, Some(ours)), 0)), None, s"${ours.name}: a file")
      }
  }
