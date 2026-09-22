## sint-decode-truncates: a number that does not fit its field is refused

Every door decoded an `Int` with `.toInt` and a JSON `Long` with
`.toLong`, so JSON 3000000000 read as 2147483647, CBOR 2^32 as 0 and
1.5 as 1 — wrong values in a `Right`. Fifteen sites now go through one
public `okay.codec.Numbers` (public because staged code is generated in
the caller's package): the fold decoders of Cbor/Json/JsonStrict (both
paths), Validate, and the compile-time and run-time staged codecs. An
`Int` refuses non-integral and out-of-range values from JSON and CBOR;
a `Long` refuses a JSON fraction and deliberately NOT a JSON value past
2^53 (already rounded by the parser; `Long.MaxValue` round-trips by
saturation), whose exact road is `BigInt`. Tests `TestIntRange` and
`TestRuntimeStagedIntRange`, both run red first. Filed beside it:
`cbor-length-wraps` (CBOR lengths narrow through `n.toInt` too).
Operator: "исправь сразу".
