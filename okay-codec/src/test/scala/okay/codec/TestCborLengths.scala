package okay.codec

/**
 * cbor-length-wraps (specs/codecs.md "Integers that do not fit"): a
 * CBOR length or count the remaining bytes cannot hold is REFUSED — it
 * used to narrow through `n.toInt` (a byte string declared 2^32+5 long
 * read five bytes) or read negative past 2^63 (an array of 2^63+1
 * elements read as empty), desynchronising the rest of the document
 * instead of failing it. Checked on the fold and on the staged codec,
 * which call the same reader.
 */
class TestCborLengths extends munit.FunSuite:

  private def unhex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  final case class Box(n: Int)
  given Schema[Box] = Schema.derived
  private val staged = Staged.cbor[Box]

  test("a byte string declared 2^32+5 long is refused, not read as five bytes") {
    val r = Cbor.read[Array[Byte]](unhex("5b0000000100000005" + "0102030405"))
    assert(r.isLeft, s"read as ${r.map(_.toList)}")
  }

  test("a text string declared 2^32+2 long is refused") {
    assert(Cbor.read[String](unhex("7b0000000100000002" + "6869")).isLeft)
  }

  test("an array of 2^63+1 elements is refused, not read as empty") {
    val r = Cbor.read[List[Int]](unhex("9b8000000000000001" + "01"))
    assert(r.isLeft, s"read as $r")
  }

  test("a map claiming more pairs than bytes left is refused, fold and staged") {
    val bytes = unhex("bb8000000000000001" + "616e01")     // {2^63+1 pairs} "n": 1
    assert(Cbor.read[Box](bytes).isLeft)
    assert(staged.decode(bytes).isLeft)
  }

  test("SKIPPING an unknown field refuses a wrapped length too (the skip path has its own reads)") {
    // {"n": 1, "x": h'01' declared 2^32+1 long}
    val bytes = unhex("a2" + "616e" + "01" + "6178" + "5b0000000100000001" + "01")
    assert(Cbor.read[Box](bytes).isLeft, s"${Cbor.read[Box](bytes)}")
    assert(staged.decode(bytes).isLeft, s"${staged.decode(bytes)}")
  }

  test("honest lengths still read, at the boundary of the bytes left") {
    assertEquals(Cbor.read[Array[Byte]](unhex("43010203")).map(_.toList), Right(List[Byte](1, 2, 3)))
    assertEquals(Cbor.read[List[Int]](unhex("83010203")), Right(List(1, 2, 3)))
    assertEquals(Cbor.read[Box](unhex("a2616e016178420102")), Right(Box(1)))   // skips "x": h'0102'
  }
