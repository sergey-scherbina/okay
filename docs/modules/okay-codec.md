# okay-codec

> One reified `Schema` per datatype; every format is an algebra
> folding it — JSON text, CBOR binary, and the dialects (JSON,
> Markdown) built on the total lex/parse stack.

Depends on: `okay-parse`. Pure Scala — cross-built for JVM and JS
(the JS client of okay-cluster encodes with exactly this code).

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

**Totality underneath.** `Json.parse` rides the okay-parse pipeline,
so a damaged document projects `JErr` values and a truncated one
still decodes the fields it carries — the LLM structured-output case.
An absent OPTIONAL field decodes as `None` (real payloads demand it).

**The lossless layer.** `Json.cst(text)` is the concrete syntax tree
with everything kept — trivia, odd spacing, duplicate keys, ordering,
damage — and `Json.render` puts it back byte-for-byte. Semantic
projection (`Json.parse`) and lossless round-tripping are separate
doors over the same tree.

**Reframing (the Markdown dialect).** Markdown emphasis does not
nest: `*a _b* c_` closes the star while the underscore is open. The
dialect answers with the uniml move — close the crossing inner
frames tokenless, close the target WITH its token, reopen the inner
frames — a miniature adoption agency. The tree stays well-nested,
every marker token is kept, and whatever is open at EOF becomes the
builder's "unclosed" error node.

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

## API reference

| member | signature | meaning |
|---|---|---|
| `Schema[A]` | `SInt/SLong/SDouble/SBool/SString/SOption/SList/SProduct/SSum` | the reified shape; fields/cases are thunked for recursion |
| `Schema.derived` | `inline given derived[A](using Mirror.Of[A]): Schema[A]` | Mirrors derivation; write `given Schema[T] = Schema.derived` |
| `Json` (data) | `JNull/JBool/JNum/JStr/JArr/JObj/JErr` | the semantic projection, damage as `JErr` |
| `Json.parse` | `String => Json` | total: any string yields a value |
| `Json.cst` / `Json.render` | `String => Cst[K]` / `Cst[K] => String` | the lossless layer |
| `Json.encode` / `Json.decode` | the two Schema algebras | render / read back (`Either`) |
| `Json.read` / `Json.write` | `String => Either[String, A]` / `A => String` | one-movers |
| `Cbor.write` / `Cbor.read` | `A => Array[Byte]` / `Array[Byte] => Either[String, A]` | RFC 8949, same content as JSON |
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
- `Json.write` of a `String` field escapes `"\n\t\r\\` only — exotic
  control characters pass through (the scanner keeps them lossless).

Measured (see [benchmarks](../benchmarks.md)): Json vs Cbor vs circe
on the same value — with the contract difference (total, lossless
CST underneath) stated next to the numbers.
