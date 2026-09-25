package okay.crypto

/** the canonical vectors, and the one byte that is not SHA3-256 */
class TestKeccak256 extends munit.FunSuite:

  private def hex(b: Array[Byte]): String = b.map(x => "%02x".format(x & 0xff)).mkString
  private def of(s: String): String = hex(Keccak256.hash(s.getBytes("UTF-8")))

  test("the empty input, 'abc' and the quick brown fox") {
    assertEquals(of(""), "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")
    assertEquals(of("abc"), "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45")
    assertEquals(of("The quick brown fox jumps over the lazy dog"),
      "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15")
  }

  test("every length across the 136-byte rate gives 32 bytes, and each differs") {
    val digests = (0 to 300).map(n => hex(Keccak256.hash(Array.fill[Byte](n)(0x61))))
    assert(digests.forall(_.length == 64))
    assertEquals(digests.distinct.size, digests.size)
  }

  test("not SHA3-256: they differ on the empty input") {
    assertNotEquals(of(""), "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a")
  }
