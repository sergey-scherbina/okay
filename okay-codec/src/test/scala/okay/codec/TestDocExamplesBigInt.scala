package okay.codec

/**
 * The snippet in docs/modules/okay-codec.md ("Big integers"), VERBATIM:
 * a doc example that is not gated is a claim nobody checks.
 */
class TestDocExamplesBigInt extends munit.FunSuite:

  final case class Holding(policy: String, quantity: BigInt)
  given Schema[Holding] = Schema.derived

  test("docs: a uint64 quantity on both wires") {
    // ---- snippet begins
    val h = Holding("ada", (BigInt(1) << 64) - 1)

    val text  = Json.write(h)                      // {"policy":"ada","quantity":"18446744073709551615"}
    val back  = Cbor.read[Holding](Cbor.write(h))  // Right(h): 1b ffffffffffffffff on the wire
    val bare  = Json.read[Holding]("""{"policy":"ada","quantity":18446744073709551615}""")
                                                   // Left(...send it as a string of digits)
    // ---- snippet ends
    assertEquals(text, """{"policy":"ada","quantity":"18446744073709551615"}""")
    assertEquals(back, Right(h))
    assert(Cbor.write(h).map(b => f"${b & 0xFF}%02x").mkString.contains("1bffffffffffffffff"))
    assert(bare.left.exists(_.contains("send it as a string of digits")), s"$bare")
  }
