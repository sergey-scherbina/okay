## okay2-codec-cbor - CBOR, Validate, Digest/Compat and the Codecs door in okay2-codec

The first of three lanes porting the rest of okay-codec to okay2
(okay2/backlog.d/modules/okay2-codec-dialects, spec stage 54, docs/okay2.md
section 33). `Cbor` is RFC 8949 over the same Schema as JSON, with the
item primitives `Cbor.Out` and `Cbor.In`, the BigInt preferred
serialization that TestBigInt pins with RFC 8949 Appendix A vectors,
lengths refused past the bytes left, unknown fields skipped, and decode,
skip and encode trampolined past `Codecs.NativeThreshold`. `Validate`
reports every refusal at its path and reads as okay2's `Validated`.
`Compat` compares two schemas and `Digest` carries a schema's shape as
data. `Codecs` gained the provider registry and the `JsonCodec`,
`CborCodec` and `StrictJsonCodec` traits.

What differs from Scala 3: the decoders dispatch through `Schema.visit`;
`Schema.Step` gained `node` and `Kid` for Validate; `Digest`'s own Schema
is written by hand, because the derivation macro cannot expand in its own
module, with one isolated cast. The ported suites are TestCborWire (the
CBOR halves of TestCodec, TestDefaults, TestVector, TestIso,
TestEnumeration and TestIntRange), TestBigInt's CBOR half, TestCborLengths,
TestCborTrampoline, TestUnknownFields, TestValidate, TestCompat, TestDigest
and TestCodecs, on JVM, Scala.js and Scala Native.
