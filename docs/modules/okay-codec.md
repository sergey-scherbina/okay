# okay-codec

Derivations are catamorphisms over one Schema.

- `Schema[A]` reifies the shape: primitives, Option, List,
  named-field products, named-case sums; recursion is broken by
  thunked fields. `Schema.derived` builds it from a Mirror, inline
  and dependency-free — write `given Schema[T] = Schema.derived`.
- Two algebras fold it: `Json.encode` renders, `Json.decode` reads
  back with errors as `Either` (an absent OPTIONAL field is `None`;
  an absent required one is an error). More algebras plug in without
  re-deriving: CBOR, validators, Spark encoders (specs/codecs.md).
- The projection sits on the TOTAL lex+parse pipeline: a truncated
  document still decodes the fields it carries —
  `Json.read[Person]("""{"name":"x","age":1,"tags":[],"boss":null""")`
  is a `Right`.
