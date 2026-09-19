# okay-codec — one Schema per datatype, every format a fold of it

`Schema[A]` reifies a datatype's shape ONCE — primitives, `Option`,
`List`, named-field products, named-case sums, recursion broken by
thunked fields — derived from Scala 3 Mirrors, inline, with no
dependencies. Every serialization concern is then a fold of that
structure with its own algebra: JSON text, CBOR binary, and, in other
modules, validators and Spark encoders. Derive once, speak many wires.

Decoding is TOTAL. A missing field, a wrong shape, a truncated
payload — each is a `Left`, never a throw, on every format.

## The pieces

| | |
|---|---|
| `Schema[A]` | the shape, derived: `given Schema[A] = Schema.derived` |
| `Json` | text: `write`, `read[A]`, `lossless` (the CST, so a citation is a byte range), `readStrict` (no tree in between) |
| `Cbor` | RFC 8949 binary, the same semantic content as the JSON algebra |
| `Staged` | the decoder specialised once and reused, for hot paths |
| `Codecs.NativeThreshold` | where a recursive schema stops costing native stack and starts trampolining |

## Round-tripping a value

```scala
import okay.codec.*

final case class Person(name: String, age: Int, nick: Option[String])
given Schema[Person] = Schema.derived

val text = Json.write(Person("Ann", 33, None))
val back = Json.read[Person](text)          // Either[String, Person]

// the same content through the binary algebra
Cbor.read[Person](Cbor.write(Person("Ann", 33, None)))
```

Both algebras render products as maps keyed by field name, sums as a
one-entry map keyed by case name, and `None` as null — so a value
written by one reads back through the other.

## Further

| | |
|---|---|
| [`docs/modules/okay-codec.md`](../docs/modules/okay-codec.md) | the guide: derivations as catamorphisms, the cross-format contract |
| [`specs/codecs.md`](../specs/codecs.md) | the design and its decisions |
| [`specs/iterative-recursive-decode.md`](../specs/iterative-recursive-decode.md) | why a deep document costs no native stack |
| [`okay-parse/`](../okay-parse) | the total lex/parse stack underneath |
