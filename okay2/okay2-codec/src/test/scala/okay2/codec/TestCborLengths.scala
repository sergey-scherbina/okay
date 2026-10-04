package okay2.codec

/**
 * cbor-length-wraps (okay-codec's TestCborLengths, the fold half): a
 * CBOR length or count the remaining bytes cannot hold is REFUSED — not
 * narrowed through `toInt` (a byte string declared 2^32+5 long read
 * five bytes) nor read negative past 2^63 (an array of 2^63+1 elements
 * read as empty), which would desynchronise the rest of the document.
 */
class TestCborLengths extends munit.FunSuite {

  private def unhex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

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

  test("a map claiming more pairs than bytes left is refused") {
    assert(Cbor.read[IntBox](unhex("bb8000000000000001" + "616e01")).isLeft) // {2^63+1 pairs} "n": 1
  }

  test("SKIPPING an unknown field refuses a wrapped length too (the skip path has its own reads)") {
    // {"n": 1, "x": h'01' declared 2^32+1 long}
    val bytes = unhex("a2" + "616e" + "01" + "6178" + "5b0000000100000001" + "01")
    assert(Cbor.read[IntBox](bytes).isLeft, s"${Cbor.read[IntBox](bytes)}")
  }

  test("honest lengths still read, at the boundary of the bytes left") {
    assertEquals(Cbor.read[Array[Byte]](unhex("43010203")).map(_.toList), Right(List[Byte](1, 2, 3)))
    assertEquals(Cbor.read[List[Int]](unhex("83010203")), Right(List(1, 2, 3)))
    assertEquals(Cbor.read[IntBox](unhex("a2616e016178420102")), Right(IntBox(1))) // skips "x": h'0102'
  }
}
