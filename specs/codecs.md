# okay-codec — dialects and data codecs (uniml, redesigned)

## Overview
The third module of the lex/parse/codec stack (see streaming-lex.md,
streaming-parse.md): concrete dialects and semantic projections. The
design source is scalascript's uniml (one token-to-tree model shared
by many syntaxes; a lossless CST as the universal representation, NOT
a lowest-common-denominator data model; semantic projections per
dialect) — rebuilt here idiomatically and IMMUTABLY on our machinery:
tokens are Chunks, drivers are Stages, builders are Folds, encoders
are streams back out. The uniml sketch's mutability is a defect to
design away, its direction is right (the user's assessment).

## Interface
- Dialects: JSON (the prover), XML, CBOR (binary — tokens are typed
  items, same model), Markdown, YAML. Each dialect = a Scan (lex) +
  a Driver (parse) + a projection.
- Projections: CST → semantic values (JSON AST, a document model),
  total where the dialect allows, error-carrying otherwise.
- Typeclass codecs are ALGEBRAS OVER A SCHEMA: a reified description
  of a datatype's structure (primitives, products, sums, collections,
  Option, a fixpoint node for recursion), derived once per type via
  Scala 3 Mirrors (inline, dependency-free). Every derivation is a
  CATAMORPHISM over Schema[T] with its own algebra — JSON, CBOR, XML,
  a validator, and (in okay-spark) the Spark Encoder all fold the SAME
  structure; and the fold runs in two modes, interpreted or STAGED
  (inline/Expr at compile time, emitting straight-line field access —
  the ExpressionEncoder trick, our P6 staging applied to data shapes).
  Note the vocabulary: this is a fold over the TYPE's shape functor
  (datatype-generic / origami programming), not our value-level
  Foldable — same algebra spirit, different carrier.
- Encoding is streaming too: a value renders as a token stream
  (`Chunks[Token]`), so large values stream out in constant memory.
- Role in the cross-platform policy: codecs are what client and
  server on different platforms speak to each other (with a transport
  module; see cross-platform-async.md).

## Behavior
- [x] lossless round-trip: parse then render reproduces the input
      byte-for-byte (trivia, ordering, duplicate keys, even damage) —
      Json.cst / Json.render; render is Cst.lexemes made a function
- [x] semantic round-trip on JSON: recursive products, sums by case
      name, escapes and whitespace (CBOR waits for its dialect)
- [x] streaming decode of a truncated document yields a partial value
      (via okay-parse totality: the tree with holes projects the fields
      that are there — a truncated Person decodes)
- [x] Markdown: the uniml-markdown reframing cases parse without
      faults (errors as nodes) — Markdown.scala: `*a _b* c_` closes
      the crossing inner frames tokenless, closes the target with its
      token and REOPENS the inner frames (adoption-agency in
      miniature); unclosed emphasis at EOF is the builder's "unclosed"
      error node; lossless (every marker token kept)
- [x] bytes are a PRIMITIVE of the algebra (`SBytes`), not something
      smuggled through a text or number field: CBOR writes a byte
      string (major 2), JSON base64, a tool schema
      `contentEncoding: base64`. Found by asking why an embedding
      index persisted as `List[Double]` — nine bytes and two boxed
      objects per component; measured 2.17x smaller on a real index,
      with precision unchanged and exact
- [x] cross-format: one derived codec serializes to JSON and CBOR
      with equal semantic content — Cbor.scala is the second algebra
      over the SAME Schema (RFC 8949: products = maps by field name,
      sums = one-entry maps by case name, None = null); both decode
      to equal values, decode errors are Left, truncation included
- [x] YAML: the indentation dialect (Yaml.scala) — scanner with two
      one-char lookaheads (PendingDash: `-5` vs `- item`;
      PendingColon: `http://x` vs `key: v`), instruction fold with an
      indent stack (dedents close frames, `- ` opens sequences, a
      scalar before `: ` was a key), projection into the SAME Json
      values so the one decode algebra serves a third wire
      (`Yaml.read[Person]` through the derived Schema). Lossless
      (comments and indentation are tokens), total (an orphan colon
      is an error leaf). v1 subset: block styles only — flow, anchors,
      tags, block scalars out of scope.

- [x] XML/HTML: the NESTING prover — JSON nests by punctuation, YAML
      by indentation, Markdown not at all (hence reframing), and this
      one by NAMED tags, which is where a close can be WRONG. A
      mismatched `</a>` closes the unclosed elements under it and says
      so on the error channel; a close with nothing open is an error
      leaf; void elements (`<br>`, `<img>`) never open a frame;
      comments and CDATA swallow markup without nesting it; an
      unterminated tag at end of input is still a token. Lossless and
      total under generated input, incremental reparse included.

## Out of scope
- schema languages/validation; a transport module (its own, later)

## codec-vector (2026-09-01)

Schema learns `Vector` (`SVector`) and `Char` (`SChar` — surfaced by
deriving okay-ui's Event, whose raw key is a Char), every algebra
swept: JSON, CBOR, the tool JSON-Schema, the form (by its fallback).
Recursion in derivation — the doc comment's old claim — is now a
TEST: a recursive product and a recursive sum derive and round-trip
at depth (the thunked fields and the laziness of `given` vals are
the mechanism, and they hold).

The exhibit is the type that filed the task: okay-ui's whole tree —
a recursive sum whose cases hold Vectors — derives `Schema[Ui]`,
`Schema[Event]`, `Schema[Patch]` and round-trips JSON and CBOR.
WireJson stays as the wire's own compact dialect BY CHOICE now, not
as a workaround.

## codec-defaults — decode falls back to the declaration

The reason this was filed is the design: Mirrors do not carry
defaults, so the ONE macro this library allows itself reads what the
compiler already wrote — the companion's `<init>$default$N` methods —
and nothing else. Everything downstream stays ordinary values:

- `SProduct` gains `defaults: Vector[Option[() => Any]]` (aligned
  with `fields`, empty when underived/unknown — every existing
  constructor call and type pattern survives unchanged).
- `Schema.derived` fills it via `Defaults.of[A]`. A default the
  macro cannot CALL at decode time — one that takes parameters
  (`b: Int = a`) or type parameters — is honestly None, not a guess.
- Json and Cbor decode: an absent field takes, in order, its
  declared default, then None-if-optional, then the missing-field
  refusal. A DAMAGED optional stays the absent case (and so reaches
  the default first when one is declared).
- ToolSpec: a defaulted field leaves the `required` list (an LLM may
  omit it — decode now survives that) and its default value is
  advertised as JSON Schema `default`.
- Form.decode rides Json's decode and inherits the fallback.

Behavior:
- [x] a product with defaulted fields decodes from partial JSON and
      partial CBOR: absent defaulted fields take their declarations,
      absent undefaulted fields still refuse by name
- [x] the default wins over None-if-optional: an absent
      `Option[Int] = Some(5)` decodes as Some(5)
- [x] round-trip is untouched: full wires decode exactly as before,
      and encode never writes a default-dependent shape
- [x] a computed default (referring to another parameter) is None in
      the vector — decode refuses the absent field rather than guess
- [x] the tool JSON Schema: defaulted fields are not required and
      carry `default`; optional fields stay unrequired
- [x] the macro is cross-platform: the shared suite proves it on
      JVM, JS and Native (macros run in the compiler — the platform
      only runs the values)

Derived schemas still round-trip their own output; defaults matter
for foreign, partial input — which tools and forms are made of.

## codec-iso — the newtype node

A wrapper type should travel as what it wraps: `Secret("env:PG")` is
a string on the wire, not `{"ref": ...}`. Mirrors cannot see through
a wrapper any more than they carry defaults, so the algebra itself
gains ONE node:

```scala
case SIso[A, B](under: () => Schema[B],
                to: B => Either[String, A],    // decode may REFINE
                from: A => B) extends Schema[A]

object Schema:
  /** a total wrapper (a newtype) */
  def wrap[A, B](to: B => A, from: A => B)(using Schema[B]): Schema[A]
  /** a refining wrapper — a Left is a decode error naming itself */
  def refine[A, B](to: B => Either[String, A], from: A => B)(using Schema[B]): Schema[A]
```

Every algebra folds through it: encode is `from` then under's
encode; decode is under's decode then `to`, a Left surfacing as the
same kind of error value every decoder here answers. The tool
schema, the form and the sql row all see the UNDERLYING shape —
which is the point: to every consumer, the wrapper does not exist.

Behavior:
- [x] a wrapped string round-trips JSON and CBOR as a BARE string;
      a wrapped Int as a bare number — no object anywhere
- [x] refine: a Left from `to` is a decode error carrying its
      message, positioned like any wrong-shape error, never a throw
- [x] a product holding wrapped fields derives, encodes flat, and
      partial input still falls back to defaults first (the two
      macro-adjacent features compose)
- [x] the tool schema of a wrapped field is the underlying type's
      schema (a Secret parameter declares as a string)
- [x] the sql row bridge treats a wrapped column as its underlying
      SqlValue kind, both directions
- [x] the first consumer: Schema[Secret] is the bare reference
      string; okay-conf's fixtures and round-trip tests move to the
      new wire, and toString stays the ref

Found by the sweep's exhaustivity warnings: WireJson had not learned
the keyed-diff patch trio (Remove/Reorder/Insert) — a server-driven
reorder would have MatchErrored on encode. Wired and round-tripped
here.

## Cast-free (2026-09-02, cast-free-codec)
`Schema` was a GADT from the start — `SOption[A](of) extends
Schema[Option[A]]` and the rest — and the codecs cast anyway
(`of().asInstanceOf[Schema[Any]]`, `.map(_.asInstanceOf[A])`, eighteen
times in Json and Cbor). Written by GADT matching on the schema
(`case l: Schema.SList[a]` binds the element type) they need none.
What the Mirror erases is stated ONCE, in Schema: `SProduct.eachField`
(parts is the Mirror's productIterator in field order, so the i-th
value is the i-th field's type) and `SSum.theCase` (caseOf is the
ordinal, so the value is that case's type) hand a codec each value at
its own type through a polymorphic function; sum cases are typed
`Schema[? <: A]`, the bound claimed in `derived` where the Mirror
gives the element types (the inline match on the tuple type cannot
see it). Decoding a product needs no kernel at all: each field decodes
at its own type and joins the erased parts that `fromProduct` takes.
Both codec suites unchanged, green on JVM, JS and Native. Next:
okay-sql's Typed (cast-free-typed) — its Shape mirrors Schema
untyped.

## Staged fold mode (2026-09-02, staged-codecs)

The Overview's promise, kept: the fold over Schema runs in two modes.
`Json.encode/decode` interpret the GADT per value; `Staged.json[A]`
folds the TYPE's shape at compile time and emits straight-line code —
Spark's ExpressionEncoder trick, P6's whole-stage codegen applied to a
data shape. Same algebra, one more instance of it.

### Interface
- `Staged.json[A]: JsonCodec[A]` — a macro; `JsonCodec` is
  `encode(a: A): String` and `decode(j: Json): Either[String, A]`.
  Needs a `Schema[A]` and a `Mirror.Of[A]` in scope, like `derived`.
- `Json.escape` is public: any encoder outside the fold needs the
  one escaping rule.

### Behavior
- [x] agreement: staged and interpreted are one algebra — encode
      byte-for-byte, decode Left-for-Left, over products, nested
      products, Option/List/Vector, sums (every case, unknown case, a
      sum inside a list), all the totality doors (absent with a
      declared default, absent optional, absent required, damaged
      optional, damaged elements, wrong shapes with the fold's own
      refusal words), an Iso field, a recursive type (TestStaged,
      JVM/JS/Native)
- [x] the price, step 0 first (CodecBenchmark, compare, Order = 7
      fields + nested Address + List + two Options): the interpreted
      fold was 6.0x over a hand-written encoder and 7.6x over a
      hand-written AST decoder, circe between them
- [x] the staged fold: encode 168 ns vs 820 (4.9x; 1.25x of the hand
      floor, 3.2x faster than circe), decode-from-AST 114 vs 634
      (5.6x; 1.6x of hand, 2.4x faster than circe) — history.tsv
      staged-codecs-step1
- [x] the staged path is the one TAKEN for a derived schema and not
      for a wrapped one (`Staged.productShape`/`sumShape`, tested)

### Decisions
- **Faithfulness is a construction-time SHAPE check, not
  derived-detection.** The first cut tried to read at expansion
  whether the field's `Schema` given came from `Schema.derived`;
  verified impossible — `given Schema[T] = Schema.derived` reaches
  the macro as a bare reference to the given val, and a hand-written
  given looks identical (a probe printed `derived=false` for every
  type, and the "staged" codec measured level with the fold because
  everything delegated). So each product or sum the codec meets gets
  one `val ok_T` hoisted before the codec object, comparing the
  run-time schema's field/case NAMES in order with the Mirror's, and
  each staged node is `if ok_T then <straight-line> else <the fold
  with that schema>`. One stable boolean read per node; an Iso from
  wrap/refine, a hand-written instance, and a reordered schema all
  take the fold, so a newtype travels as its underlying type in both
  modes.
- **Delegation, not expansion, for recursion** — a type met again
  inside itself folds at run time; the alternative (a generated
  recursive method per type) is a later refinement if a recursive
  hot path names it.
- **Refusal messages are the fold's own** — every cold path
  (wrong shape, wrong primitive) calls `Json.decode(schema)` for
  that node, so the words never diverge.
- **The second macro of the module** — Defaults.scala was "the one";
  Staged earns the exception the same way (reads what the compiler
  knows, hands back an ordinary value) and the codecs spec promised
  it.
- **The parser is the elephant, and not this lane's** — `Json.parse`
  of the 150-byte fixture is 14.6 µs against circe's 0.55 µs (26x):
  the lossless CST parser (trivia, totality, error-carrying) is what
  text→value pays, and decode is 0.6 µs of that 15.2. Filed as its
  own road (a fast VALUE parser beside the lossless one), separate
  from staging.

### Out of scope
- Cbor/Yaml/Xml staged algebras (the same generator, another
  emitter — when a wire names it).
- Run-time staging (`scala.quoted.staging`) for schemas that exist
  only at run time (a ToolSpec from a model, Pg composites from the
  catalog): JVM-only, a compiler dependency; only if such a workload
  appears.
