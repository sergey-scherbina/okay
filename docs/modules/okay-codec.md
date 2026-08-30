# okay-codec

Derivations are catamorphisms over one Schema.

- `Schema[A]` reifies the shape: primitives, Option, List,
  named-field products, named-case sums; recursion is broken by
  thunked fields. `Schema.derived` builds it from a Mirror, inline
  and dependency-free — write `given Schema[T] = Schema.derived`.
- The algebras fold it: `Json` renders text, `Cbor` renders RFC 8949
  binary (products = maps by field name, sums = one-entry maps by
  case name, `None` = null) — ONE derived Schema, two wires, equal
  semantic content; errors as `Either` on both (an absent OPTIONAL
  field is `None`; an absent required one, a truncated CBOR buffer,
  a wrong shape — all `Left`). Validators and Spark encoders plug in
  the same way without re-deriving.

```scala
given Schema[Person] = Schema.derived
Json.read[Person](Json.write(p))   // Right(p)
Cbor.read[Person](Cbor.write(p))   // Right(p), same content in binary
```

- The lossless layer: `Json.cst(text)` is the CST (trivia, duplicate
  keys, ordering, damage — all kept) and `Json.render` puts it back
  byte-for-byte; `Json.parse` is the semantic projection over it.
- `Markdown` is the REFRAMING prover (the uniml case): crossing
  emphasis `*a _b* c_` closes the inner frames tokenless, closes the
  target with its token and REOPENS the inner frames — well-nested,
  lossless, never a fault; unclosed emphasis at EOF is the builder's
  "unclosed" error node, an error as data.
- The projection sits on the TOTAL lex+parse pipeline: a truncated
  document still decodes the fields it carries —
  `Json.read[Person]("""{"name":"x","age":1,"tags":[],"boss":null""")`
  is a `Right`.
