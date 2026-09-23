# okay-codec

> One reified `Schema` per datatype; every format is an algebra
> folding it — JSON text, CBOR binary, and the dialects (JSON,
> Markdown) built on the total lex/parse stack.

Depends on: `okay-parse`. Pure Scala — cross-built for JVM and JS
(the JS client of okay-cluster encodes with exactly this code).


`Policy` (specs/optics-outside.md stage 7): a projection policy over a
schema — dotted keys checked by name at construction, `touches` the
audit with no document, `project`/`redact`/`optic(key)`/`text` the
runs; the audit names exactly the fields the projection removes.

## Guide

**Derivations are catamorphisms over one Schema.** `Schema[A]`
reifies a datatype's shape once — primitives, `Option`, `List`,
named-field products, named-case sums, recursion broken by thunked
fields — via Scala 3 Mirrors, inline and dependency-free. Every
serialization concern is then a FOLD of that structure with its own
algebra: `Json` (text), `Cbor` (RFC 8949 binary), and, in their own
modules, validators or Spark encoders. Derive once, speak many wires;
this is the datatype-generic ("origami") fold over the TYPE's shape,
not the value-level `Foldable`.

**The cross-format contract.** Both shipped algebras render the same
semantic content — products as maps keyed by field names, sums as
one-entry maps keyed by case name, `None` as null — so
`Json.read(Json.write(a)) == Cbor.read(Cbor.write(a))`. Decode errors
are values (`Left`) in both: a missing required field, a wrong shape,
a truncated CBOR buffer.

**A wrapper that names its vocabulary.** `Schema.wrap`/`refine` make a
newtype invisible to every algebra; `Schema.enumeration(values, name)`
is a refine over a finite vocabulary that stays invisible on every
wire and shows in exactly one place — `JsonSchema.of` declares it as
`enum` — so a tool parameter or a `response_format` contract carries
the words a prompt used to have to state. A prompt itself renders
with `JsonSchema.of(s, vocabularies = false)`: measured, the same
words rendered into the prompt's schema cost a 4B model 1.7 macro-F1
points, deterministically, beside the prose rule that already states
them.

**Big integers.** `Schema.SBigInt` (`Schema[BigInt]`) is a primitive,
not a wrapper over a string, so every algebra sees a NUMBER: a JSON
Schema, an SQL column (`SqlType.Num`), a Spark `decimal`. It exists
because some integers do not fit a `Long` — a Cardano asset quantity is
a uint64, a Plutus datum integer has no bound — and the two wires treat
it the way their standards do:

```scala
final case class Holding(policy: String, quantity: BigInt)
given Schema[Holding] = Schema.derived

val h = Holding("ada", (BigInt(1) << 64) - 1)

val text  = Json.write(h)                      // {"policy":"ada","quantity":"18446744073709551615"}
val back  = Cbor.read[Holding](Cbor.write(h))  // Right(h): 1b ffffffffffffffff on the wire
val bare  = Json.read[Holding]("""{"policy":"ada","quantity":18446744073709551615}""")
                                               // Left(...send it as a string of digits)
```

- **CBOR** writes RFC 8949's *preferred serialization* (§3.4.3): a
  plain integer across the whole unsigned 64-bit range, and a tag 2
  (positive) or tag 3 (negative) *bignum* over the big-endian
  magnitude past it. It is the encoding the Cardano ledger's CDDL
  names `big_int`, so the bytes are the ones a node would hash. Reading
  also accepts a bignum for a small value, which a conforming encoder
  may send.
- **JSON** carries it as a STRING of digits. A JSON number is only
  interoperable up to 2⁵³ (RFC 8259 §6), and our `Json.JNum` is a
  `Double`: `18446744073709551615` would already be `…616` by the time
  anything saw it. Protocol Buffers' JSON mapping sends its 64-bit
  integers as strings for the same reason. Both JSON doors accept a
  bare number too, but only an exact one (integral, within ±2⁵³); a
  larger one is refused with the reason rather than returned rounded.

References: C. Bormann, P. Hoffman, *Concise Binary Object
Representation (CBOR)*, RFC 8949 (2020), §3.4.3 "Bignums",
doi:10.17487/RFC8949; T. Bray, *The JavaScript Object Notation (JSON)
Data Interchange Format*, RFC 8259 (2017), §6 "Numbers",
doi:10.17487/RFC8259; the Cardano ledger's Conway CDDL (`big_int`,
`big_uint = #6.2(bounded_bytes)`), IntersectMBO/cardano-ledger.

**Columns: a datatype as a table, with no engine in it.** `Columns`
folds a `Schema[A]` into column types (`Int32`, `Int64`, `Text`,
`Binary`, `Decimal`, `Json`, `Arr`, `Struct`) and rows of plain Scala
values. Every tabular decision a sum type or a recursive one forces is
made here once, so Spark (okay-spark's `SparkSchema` is a thin
translation of this), DuckDB, Parquet, Delta or an in-process
aggregator all read the same table:

```scala
val (fields, toRow) = Columns.table[Output]
// Vector(Field(id,Int32,false), Field(owner,Struct(Vector(Field(kind,Text,false),
//   Field(KeyHash,Struct(...),true), Field(ScriptHash,Struct(...),true))),false),
//   Field(lovelace,Decimal(38,0),false))
val row = toRow(Output(2, Credential.ScriptHash(Array[Byte](2)), BigInt(5_000_000)))
// Row(Vector(2, Row(Vector(ScriptHash, null, Row(Vector([2])))), 5000000))
```

A pure enum is `Text` holding the case NAME (an ordinal would be
renumbered by the next case); a sum with payloads is `kind` plus one
nullable struct per case WITH fields (a field-less case has no branch
— Parquet refuses an empty struct — and a new case is a new nullable
column old files read as null); a RECURSIVE type, a named node
reachable from itself (mutual recursion included), is
`Struct(cbor: Binary, json: Json)`; `BigInt` is `Decimal(38, 0)`,
refused past 38 digits. The same reading as spark-avro's unions and
spark-protobuf's `oneof`, with the discriminator they lack.

**Totality underneath.** `Json.parse` rides the okay-parse pipeline,
so a damaged document projects `JErr` values and a truncated one
still decodes the fields it carries — the LLM structured-output case.
An absent OPTIONAL field decodes as `None` (real payloads demand it).

**The lossless layer.** `Json.cst(text)` is the concrete syntax tree
with everything kept — trivia, odd spacing, duplicate keys, ordering,
damage — and `Json.render` puts it back byte-for-byte. Semantic
projection (`Json.parse`) and lossless round-tripping are separate
doors over the same tree.

**Indentation (the YAML dialect).** Structure in leading whitespace:
the instruction fold carries an indent stack, dedents close frames,
`- ` opens sequences, and a scalar followed by `: ` was a key (two
one-character lookaheads in the scanner settle `-5` vs `- item` and
`http://x` vs `key: v`). The projection lands in the SAME `Json`
values, so ONE decode algebra serves JSON, CBOR and YAML:
`Yaml.read[Person](doc)` decodes through the Schema you already
derived. Deliberate v1 subset: block mappings/sequences, plain and
double-quoted scalars, comments (kept — lossless); flow styles,
anchors, tags and block scalars are out of scope, degrading to error
leaves, never faults.

**Nesting by name (the XML/HTML dialect).** The case where a close
can be wrong: `Xml` closes the unclosed elements under a mismatched
`</a>` and marks them, treats a close with nothing open as an error
leaf, never opens a frame for a void element (`<br>`, `<img>`), and
lets comments and CDATA swallow markup without nesting it. Lossless
like the rest, so `Xml.render(Xml.cst(s)) == s` for every string;
`Xml.text` and `Xml.elements` are the projections.

**Reframing (the Markdown dialect).** Markdown emphasis does not
nest: `*a _b* c_` closes the star while the underscore is open. The
dialect answers with the uniml move — close the crossing inner
frames tokenless, close the target WITH its token, reopen the inner
frames — a miniature adoption agency. The tree stays well-nested,
every marker token is kept, and whatever is open at EOF becomes the
builder's "unclosed" error node.

## EDN, for Clojure and for what JSON cannot say

`Edn.write`/`Edn.read` are a third format beside JSON and CBOR, through
the same `Schema`. EDN (github.com/edn-format/edn) is what Clojure speaks,
and it keeps what JSON flattens: a keyword is not a string, an integer is
exact to 64 bits (and `123N` beyond), a character is `\c`, and a variant
is NAMED by a tag. A product is a map with keyword keys, a sum a tagged
value namespaced by the sum:

```scala
val text = Edn.write(doc)
```

gives `{:name "okay" :count 9223372036854775807 :big 123456789012345678901234567890N :initial \o …
:shape #Shape/Rect {:w 2.0 :h 3.5} …}`, which Clojure's `clojure.edn/read-string`
reads as its own data — and Clojure's `pr-str`, commas and all, reads back
as the typed value:

```scala
assertEquals(Edn.read[Doc](printed.toString), Right(doc))
```

Text is read and printed with an explicit stack and a `Schema` decoded on
JSON's two roads (native, then `Cont` past `Codecs.NativeThreshold`), so a
20 000-deep document and a 5 000-link recursive value run on the default
stack on JVM, JS and Native alike.

## The other side's types: `Stubs`

The codecs make okay's side of a value typed. `Stubs` writes the OTHER
side's declaration from the same `Schema`, so the other language's type
checker sees the type too:

- **`Stubs.python(schemas*)`** writes a module of `TypedDict`s in the
  shape okay-py's `PyCodec` sends: a product is a dict, and a sum is its
  case's dict plus `"type": Literal["Case"]`.
- **`Stubs.typescript(schemas*)`** writes a `.d.ts` in the shape `Json`
  writes. A product is an interface. A sum is `{ "Case": {...} }`. `None`
  is `null`, a `BigInt` a string of digits, and bytes a base64 string. A
  `Long` is `export type Long = number`, with a comment that a JS number
  holds it exactly only to 2^53. `Int`, `Char`, `BigIntDigits` and
  `Base64` are named the same way, so reading the declarations back is
  exact.
- **`TsTypes.scala(source, pkg)`** is the other direction: TypeScript
  declarations read into `case class`/`enum ... derives Schema`, with
  anything that is not data refused by name.

Declarations come out once each, in dependency order, and recursive types
refer to themselves by name. Both are checked by the real checkers:
TestStubsTsc runs `tsc --strict`, and okay-py's TestPyStubs runs
`mypy --strict`. R, Clojure and Frege get none. R has no types to
declare, and the JVM languages read okay's own classes. specs/schema-stubs.md.

## Tutorial

Derive and round-trip both wires:

```scala
import okay.codec.{Cbor, Json, Schema}

case class Person(name: String, age: Int, tags: List[String], boss: Option[Person])
given Schema[Person] = Schema.derived    // recursion needs the explicit given

val p = Person("ann", 41, List("a"), Some(Person("boss", 60, Nil, None)))
Json.read[Person](Json.write(p))   // Right(p)
Cbor.read[Person](Cbor.write(p))   // Right(p) — same content, binary
```

Sums go by case name, `{"CaseName": inner}` on both wires:

```scala
enum Shape:
  case Circle(r: Double)
  case Rect(w: Double, h: Double)
given Schema[Shape.Circle] = Schema.derived
given Schema[Shape.Rect] = Schema.derived
given Schema[Shape] = Schema.derived
Json.write(Shape.Circle(1.5))      // {"Circle":{"r":1.5}}
```

The truncated-answer flagship:

```scala
Json.read[Person]("""{"name":"x","age":1,"tags":[],"boss":null""")
// Right(Person("x", 1, Nil, None)) — the tree with holes projects
// the fields that are there
```

Lossless round-trip and the reframing dialect:

```scala
Json.render(Json.cst("{ \"k\":1 ,\"k\": 2 }"))  // the input, byte-for-byte

val t = Markdown.parse("*a _b* c_\n")
Cst.lexemes(t) == "*a _b* c_\n"    // every marker kept
Cst.errors(t).isEmpty               // reframed, not faulted
```

## Will the other side still read it?

`Compat.compare(old, next)` folds two `Schema` values and answers
whether a reader of one still decodes the other's bytes — no
registry, no contract-testing tool, because the schema is already a
value. Two directions: **backward** (the new reader over old bytes —
the log, and traffic in flight) and **forward** (the old reader over
new bytes — consumers not upgraded yet); `rolling` is both, which is
what a rolling deploy needs.

```scala
val r = Compat.compare(summon[Schema[OrderV1]], summon[Schema[OrderV2]])
r.backward.compatible   // false: `currency` is new and required
r.backward.reasons      // why, in words
println(r.render)       // the changes and both verdicts
```

The rules are read off this module's own decoders, and the tests
prove them by encoding with one schema and decoding with the other
rather than by assertion — on BOTH wires, which must agree.

That last clause was once false, and finding out is what this check
is for: JSON skipped an unknown field and CBOR refused it, so a
verdict used to name its wire. Nothing had chosen that (no test, no
spec, and `JsonStrict` skips unknown fields by design), so it was
fixed rather than documented — see specs/codecs.md, "Unknown fields,
on both wires".

| change | backward (new reader, old bytes) | forward (old reader, new bytes) |
|---|---|---|
| field added, required | broken — no value for it | ok — an old reader skips it |
| field added, optional or defaulted | ok — the fallback | ok |
| field removed, required | ok — the new reader skips it | broken |
| field retyped | broken | broken |
| case added | ok | broken — unknown case |
| case removed | broken | ok |

Put it in a test and a breaking change stops being a production
discovery:

```scala
test("v2 still reads what v1 wrote") {
  assert(Compat.compare(v1, v2).backward.compatible)
}
```

Nesting, collections and recursion are handled (a path names where
the change is, a wrapper is no change, a self-referential type
terminates). A RENAME reads as a remove plus an add, which is what
the wire sees.

## API reference

| member | signature | meaning |
|---|---|---|
| `Schema[A]` | `SInt/SLong/SDouble/SBool/SString/SBytes/SBigInt/SOption/SList/SProduct/SSum` | the reified shape; fields/cases are thunked for recursion |
| `Schema.SBytes` | `Schema[Array[Byte]]` | raw bytes: a CBOR byte string, base64 in JSON, `contentEncoding` in a tool schema |
| `Columns` | `fields[A]`, `row(a)`, `table[A]`, `column(s)`, `recursiveNames(s)` | a Schema as engine-free columns and rows |
| `Schema.SBigInt` | `Schema[BigInt]` | an unbounded integer: a CBOR integer to 2⁶⁴−1 then a tag 2/3 bignum, a digit string in JSON, `SqlType.Num` in SQL |
| `Base64` | `encode(Array[Byte]): String`, `decode(String): Either[String, Array[Byte]]` | RFC 4648 §4, hand-rolled and total; decoding reports rather than throws |
| `Schema.derived` | `inline given derived[A](using Mirror.Of[A]): Schema[A]` | Mirrors derivation; write `given Schema[T] = Schema.derived` |
| `Json` (data) | `JNull/JBool/JNum/JStr/JArr/JObj/JErr` | the semantic projection, damage as `JErr` |
| `Json.parse` | `String => Json` | total: any string yields a value |
| `Json.cst` / `Json.render` | `String => Cst[K]` / `Cst[K] => String` | the lossless layer |
| `Json.encode` / `Json.decode` | the two Schema algebras | render / read back (`Either`) |
| `Json.read` / `Json.write` | `String => Either[String, A]` / `A => String` | one-movers |
| `Cbor.write` / `Cbor.read` | `A => Array[Byte]` / `Array[Byte] => Either[String, A]` | RFC 8949, same content as JSON |
| `Edn.write` / `Edn.read` | `A => String` / `String => Either[String, A]` | EDN, Clojure's data notation: keyword keys, exact 64-bit integers, `123N`, `\c`, a sum as a `#Sum/Case` tag; stack-safe both ways |
| `Edn` (data) / `Edn.parse` / `Edn.show` | `ENil/EBool/ELong/EBig/EDouble/EDec/EStr/EChar/EKeyword/ESymbol/EList/EVector/EMap/ESet/ETagged` | the EDN value tree, read and printed with an explicit stack |
| `Compat.compare` | `(Schema[A], Schema[B]) => Report` | what changed, and whether each direction still decodes |
| `Cbor.In.skipItem` | `() => Either[String, Unit]` | one complete item read and discarded — what a decoder does with a field it does not declare |
| `Validate.decode` / `Validate.errors` | `Schema[A] => Json => Either[Vector[(path, msg)], A]` | `Json.decode`'s applicative twin: EVERY refusal, each at its dotted path, the typed value when there is none — same rules, read off the same decoder; a fold on `Schema.Step` |
| `Schema.fold` / `Schema.Algebra` / `Schema.Step` | the catamorphism, its algebra, the depth-aware value walk | how every algebra over `Schema` is written since schema-fold: no `match` on the GADT, no depth logic in the algebra |
| `Codecs.NativeThreshold` | `Int` (24) | native recursion below this, `Cont.defer` trampoline at/above it — every recursive door; no depth cap (`Codecs.maxDepth` removed, remove-codecs-maxdepth) |
| `Markdown.parse` | `String => Cst[Markdown.K]` | the reframing dialect (headings, paragraphs, `*`/`_` emphasis, code spans) |
| `Markdown.scan` / `Markdown.instructions` | the dialect's Scan and its instruction fold | reuse or extend |

## Gotchas

- Recursive types need the EXPLICIT given (`given Schema[Person] =
  Schema.derived`) — the thunk defers the self-reference past
  initialization; `derives` on a recursive type will loop.
- Sums need givens for every case (see Shape above) — `summonInline`
  looks them up at the use site.
- The JSON number is a `Double` in the projection — `SLong` decode
  goes through it (53-bit exactness); CBOR carries integers natively.
  A value that needs more than 53 bits in JSON wants `BigInt`, which
  travels as a digit string.
- CBOR `SLong` REFUSES an integer outside `Long` (a uint64 past 2⁶³,
  a negative past −2⁶³) — it used to wrap it silently, so
  `18446744073709551615` read as `-1`. Decode into `BigInt` instead.
- A number that does not fit its field is a `Left`, never a nearby
  value: an `Int` field refuses `3000000000` (it used to read
  `2147483647`), CBOR 2³² (it read `0`) and `1.5` (it read `1`); a
  `Long` field refuses `2.5`. The staged codecs refuse exactly what the
  fold refuses (`okay.codec.Numbers` is the one place that decides).
  Not checked: a JSON number past 2⁵³ into a `Long` — the parser has
  already rounded it; send such values as `BigInt`.
- A CBOR length or count the remaining bytes cannot hold is refused:
  a byte string declared 2³²+5 long used to read five bytes, and an
  array of 2⁶³+1 elements used to read as empty — each leaving the
  rest of the document read from the wrong place.
- `Json.write` of a `String` field escapes `"\n\t\r\\` only — exotic
  control characters pass through (the scanner keeps them lossless).

Measured (see [benchmarks](../benchmarks.md)): Json vs Cbor vs circe
on the same value — with the contract difference (total, lossless
CST underneath) stated next to the numbers.

## Which JSON door

There are two, and they differ in what they promise, not in what they
answer:

| | `Json.read` | `Json.readStrict` |
|---|---|---|
| a complete, well-formed document | the value | **the same value** (TestJsonStrict holds them equal) |
| a truncated document (a stream cut mid-value) | what arrived, decoded | `Left` |
| a damaged document | damage as data (`JErr` leaves, the projection keeps the rest) | `Left` |
| a stray or trailing character | projected around | `Left` |
| cost | scanner, CST with every trivia token, projection, fold — ~128 KB and ~10 us on the benchmark order | characters straight into the `Schema`, no tree — ~5 KB, in circe's band; **`Staged.strict[A]`** is the same read generated for the type, ~2.3 KB and 2.45x faster than circe |

Choose `read` when the contract matters — a document that may still
be arriving (the LLM case), an input you must render back byte for
byte, damage you want to see rather than refuse. Choose `readStrict`
for a complete document from a source you trust to be well-formed,
which is most decoding. Both take the same `Schema`, so the choice is
one identifier at the call site, and the wire contract does not move.
A caller who derives once and decodes many times takes the third
door: `val codec = Staged.strict[A]` at construction, `codec.decode(text)`
on the hot path — the strict read generated for `A`, the same answer
as `readStrict` (TestJsonStrictStaged holds them equal), 2.45x faster
than circe with 32% less allocation.

All three doors take a `Schema` the compiler can see. A schema that
exists only at run time (a composite from a database catalog, a
tool's declared parameters) has the interpreter, and — on the JVM,
by adding the optional [`okay-staging`](okay-staging.md) module — the
same staged codec generated from the schema as a value, switchable
off at launch. A generic door — `def put[A](a: A)(using Schema[A])`
— takes its codec from `Codecs.json(s)` / `Codecs.cbor(s)`: the
interpreter until a program installs a provider, the staged one
after `RuntimeStaged.install()`; `Json.write` and `Cbor.write` stay
the fold, verbatim.
