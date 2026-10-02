package okay.codec

import okay.RunCodec

/**
 * A spilling sort's element codec for every type with a `Schema`
 * (chunks-external-sort): CBOR, which is exact and self-delimiting, so a
 * case class sorts through `Chunks.sortBy` with no codec written by hand.
 * okay-stream declares `RunCodec` and cannot see okay-codec; this is the
 * one module that sees both. Import it: `import okay.codec.RunCodecs.given`
 * — an imported given wins over `RunCodec`'s own for the primitives, so
 * the two never meet as an ambiguity.
 */
object RunCodecs:
  given fromSchema[A](using s: Schema[A]): RunCodec[A] =
    RunCodec.bytes(a => Cbor.write(a)(using s),
      b => Cbor.read[A](b)(using s).fold(why => throw IllegalStateException(s"a spilled record is not a $s: $why"), identity))
