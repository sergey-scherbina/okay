## schema-bigint: an unbounded integer as a Schema primitive

`Schema.SBigInt` (`Schema[BigInt]`), with `Schema.Algebra.bigInt`
abstract so the compiler swept every algebra — stage 0 of
specs/scalus.md, because a Cardano asset quantity is a uint64 and a
Plutus datum integer is unbounded.

- CBOR: RFC 8949 preferred serialization — a plain integer across the
  whole 64-bit unsigned range, a tag 2/3 bignum past it (the ledger's
  `big_int`, byte for byte); reads every form, refuses a foreign tag.
- JSON: a string of digits (`JNum` is a Double). `Json.decode`,
  `JsonStrict` and `Validate` accept the same set from one helper
  (`BigInts`): digits, or an exact number within ±2^53.
- JSON Schema `string` + `pattern`; `Digest`/`Compat` name it
  (`Long -> BigInt` is a type change: the JSON wire changes); SQL maps
  it to `SqlType.Num`, binding digits as `decimalSchema` does; forms
  get a text input; R and okay-conf carry digits.
- FIXED on the way, test red first: CBOR `SLong` decoded a uint64 past
  2^63 as a negative (`1bffffffffffffffff` read as -1); it now refuses,
  and `Out.header` compares its argument unsigned. `SInt`'s truncation
  is the same class and is filed, not fixed (`sint-decode-truncates`).

Docs: okay-codec guide "Big integers" with the gated snippet
(`TestDocExamplesBigInt`) and RFC 8949 / RFC 8259 references; typepedia
entry; spec specs/codecs.md "Big integers".
