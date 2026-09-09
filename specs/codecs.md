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
defaults, so the macro here reads what the compiler already wrote —
the companion's `<init>$default$N` methods — and nothing else. (This
was "the ONE macro this library allows itself" until optics-core,
2026-09-09, whose field selector `Lens[S](_.f)` is a second of the
same kind; the policy is now stated as what both obey: A MACRO ONLY
READS, IT NEVER WRITES — it may inspect a tree the compiler built and
must generate nothing beyond a call to ordinary code. specs/optics.md
Decisions.) Everything downstream stays ordinary values:

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

## codec-enumeration — a refinement that names its vocabulary (2026-09-07, codec-jsonschema-refinement-enum)

Found by intent-structured-output: `Conf` was `Schema.refine` over a
string — three words the decoder accepts and everything else a Left —
and `JsonSchema.of` rendered it as a plain `string`, because to every
algebra the wrapper does not exist. So a `response_format` contract,
or a tool declaration, could not carry the one thing the prompt had
to state in prose. A declaration is the one consumer for which the
wrapper's VOCABULARY should exist.

```scala
case SIso[A, B](under: () => Schema[B],
                to: B => Either[String, A],
                from: A => B)
               (val vocabulary: Option[Vector[B]] = None)   // a second list

object Schema:
  /** a refinement over a FINITE vocabulary: `name(a)` on the wire,
   * an unknown spelling a decode error naming the vocabulary,
   * `"enum": [...]` in a JSON Schema */
  def enumeration[A, B](values: Vector[A], name: A => B)(using Schema[B]): Schema[A]
  /** `refine` that DECLARES its vocabulary — the decoder may accept
   * more spellings than the declaration lists */
  def vocabulary[A, B](names: Vector[B], to: B => Either[String, A], from: A => B)(using Schema[B]): Schema[A]
```

The vocabulary rides on `SIso` as a SECOND parameter list, so the
twenty `SIso(u, to, from)` patterns across the Json, CBOR, strict,
form, sql and skeleton folds match exactly as before and read
nothing new; only `JsonSchema.of` looks at it, and emits the
underlying type's schema plus `enum` with each value encoded by the
underlying schema (`["low","medium","high"]`, or `[1,2]` for an
integer vocabulary). On every wire an enumeration IS a refine.

Behavior:
- [x] the wire is unchanged: the name goes out, the name comes back,
      JSON and CBOR, alone and inside a product; an unknown name is a
      decode error naming the vocabulary
- [x] the JSON Schema of an enumeration is the underlying type plus
      `enum`; a plain `refine` stays a plain string
- [x] an integer vocabulary declares integer values
- [x] `Conf` declares its vocabulary (`Schema.vocabulary`, keeping the
      case-insensitive decode the recorded journal relies on — the
      gate's `TestEvalJournal` caught an `enumeration` that matched
      exactly): a contract or a tool declaration built from
      `Reading[I]` now says `low, medium, high`
- [x] a PROMPT does not: `JsonSchema.of(s, vocabularies = false)` is
      what `Classify.prompt`/`taxonomy` render, because the gate's
      journal fingerprint changed, the promotion rule then fired
      (Request 0.93 → 0.89), and two runs each way showed the model
      deterministic and the enum in the schema costing 1.7 macro-F1
      both times (`TestEnumPromptEffect`, Live) — the prose rule
      already states the words, and stating them twice is worse. The
      recording therefore stands unchanged.

## Compatibility between two versions (2026-09-09, schema-compat)

The microservices audit's last cheap gap. Two services share a
Schema-encoded message; one side changes its case class; nothing
here ANSWERED whether the other side can still decode. The industry
reaches for a schema registry or a contract-testing tool. Neither is
needed, because a `Schema` is a VALUE: the question is a fold over
two of them.

The rules are not invented for this check, they are READ OFF the
decoders in this module — which is what makes them trustworthy, and
what the tests assert directly (every case encodes with one schema,
decodes with the other, and the report must have predicted the
outcome):

- an absent field takes its declared default, then None-if-optional,
  then a refusal by name (codec-defaults);
- an unknown CASE is a refusal, on both wires;
- an unknown FIELD is **ignored by Json** (only declared fields are
  looked up) and **refused by Cbor** (`unknown field '<k>'`).

That last asymmetry is the one surprise, and it is why a verdict
names its wire rather than pretending there is one answer. It also
says something operational: a service pair that adds fields freely
is doing so on JSON; the same change on CBOR needs the readers
upgraded first.

Two directions, and they answer different questions:

| direction | who reads what | the question |
|---|---|---|
| BACKWARD | the new reader over old bytes | can we deploy this and still read the log, and the traffic already in flight? |
| FORWARD | the old reader over new bytes | can we deploy this before every consumer is upgraded? |

A rolling deploy needs both (`Report.rolling`); a log needs backward
for ever.

```scala
val report = Compat.compare(summon[Schema[OrderV1]], summon[Schema[OrderV2]])
report.backward(Compat.Wire.Cbor).compatible   // deploy-safe against the log?
report.render                                  // the operator's paragraph
```

Behavior:
- [x] a new REQUIRED field breaks backward on both wires; a new
      OPTIONAL or DEFAULTED one does not
- [x] a new field breaks FORWARD on Cbor and not on Json — the
      decoders' own asymmetry, asserted by decoding, not assumed
- [x] a removed required field breaks forward; on Cbor it breaks
      backward too
- [x] a retyped field breaks both directions and names both types
- [x] a new case breaks forward only; a removed case breaks backward
      only
- [x] a change inside a nested product or collection is found and
      its path names it; a wrapper (`SIso`) is no change at all;
      List and Vector are the same array
- [x] a self-referential type terminates (the name-pair guard) and
      reports its change once
- [x] every verdict agrees with what `Json.decode` and `Cbor.read`
      actually do on a value written by the other schema

Out of scope: a registry (who publishes which version — that is a
deployment fact, and the log already carries the envelope version,
specs/persist.md); a migration generator (an upcast is a function
someone writes, `Typed.step`); field RENAMES read as a remove plus
an add, which is what the wire sees.

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
  only at run time — taken up as its own module on the operator's
  ask, "Run-time staging" below (2026-09-07).

## Value parser (2026-09-02, json-value-parser)

staged-codecs step 0 found the real cost: with decode at 0.1 µs
(staged), text→value on a 150-byte object was 14.6 µs of parsing and
0.6 µs of decode — the lossless CST parser (lex, tree, project) is
what a caller who just wants the value pays for. `JsonValue` is the
other half of the promise this spec's Overview already made for
codecs ("interpreted or STAGED"): one more mode, this time for the
TEXT side.

### Interface
- `JsonValue.parse(s: String): Option[Json]` — a strict recursive
  descent over the string; `None` on anything it is not sure of.
- `Json.parseValue(s: String): Json` — the total function: the fast
  road's answer, or `Json.parse(s)` (the lossless road) when the fast
  road refused. Same values, same totality; the lossless road's
  trivia and error placement are what is not kept.

### Behavior
- [x] agreement: `JsonValue.parse` and `Json.parse` yield equal Json
      on every well-formed document (25 shapes: nesting, escapes,
      unicode, exponents, duplicate keys, whitespace) and on every
      damaged one (34 shapes: truncation, wrong punctuation, raw
      control characters, unterminated strings, `NaN`/bare
      identifiers) — the fast road refuses every damaged shape,
      never accepts a wrong answer
- [x] the strong form: a full PREFIX-TRUNCATION sweep — every
      substring of every document above, both roads, equal
      (TestJsonValue, JVM/JS/Native)
- [x] the price: 217 ns vs 13.3 µs on the fixture (61x; history.tsv
      json-value-parser), 2.0x faster than circe's own parser; end to
      end with the staged decoder, 349 ns vs circe's fused
      parse+decode at 804 (2.3x)

### Decisions
- **Refuse rather than diverge** — the fast parser is strict RFC
  8259 plus the projection's own two readings (kept ON PURPOSE so
  the roads never disagree: `A` is the four letters "u0041",
  any other unknown escape is itself; a number is whatever
  `toDouble` makes of its RFC-shaped lexeme, so `1e999` is Infinity).
  Anything else — a raw control character in a string, trailing
  content, empty input — is `None`, and `parseValue` falls through
  to the lossless parser, which already owns every damage shape and
  its wording. No damage vocabulary is duplicated.
- **No tokens, no tree** — an index into the String, a
  `StringBuilder` only for the (rare) escaped string, `substring` for
  the plain case and for a number's slice into `parseDouble`. This is
  what the interpreted road's own choice-rule from staged-tagless
  says for text: the CST is for tools (incremental reparse,
  diagnostics, round-trip), the fast pass is for speed.

### Out of scope
- A staged (macro) parser tied to a Schema shape — would still have
  to reparse arbitrary JSON structurally first; not pursued without
  a workload that reads the SAME schema from text repeatedly enough
  to amortize generating a parser for it.
- Streaming / incremental fast-path parsing (the lossless CST already
  owns incremental reparse).

## Staged CBOR (2026-09-02, staged-cbor)

The Staged fold mode's second emitter, and the first one this spec's
own words called for ("when a wire names it" — BACKLOG): CBOR sits on
okay-persist's hot path (`Wire.scala`, `WireProtocol.scala`,
`Typed.scala`, `Snapshots.scala` — every durable-log record, every
wire frame, every snapshot), so it is named now.

### Interface
- `Staged.cbor[A]: CborCodec[A]` — `encode(a: A): Array[Byte]`,
  `decode(bytes: Array[Byte]): Either[String, A]`.
- `Cbor.Out`/`Cbor.In` are public: the item-level primitives (a
  major-type header, an integer, a length-prefixed string or byte
  string) both the interpreted `write`/`read` and the staged
  generator call — one implementation of RFC 8949's varint encoding,
  not two. `Cbor.encodeItem`/`decodeItem` are the fallback door: one
  item at A's schema, on an already-open `Out`/`In`, for a node the
  staged generator delegates.

### Behavior
- [x] agreement: staged and interpreted are one algebra — encode
      item-for-item, decode Left-for-Left, over products, nested
      products, Option/List/Vector, sums, an Iso field, a recursive
      type, every wrong-shape refusal in the fold's own words
      (TestStagedCbor, JVM/JS/Native)
- [x] CBOR's own hazard, absent from JSON: a map carries no field
      ORDER guarantee. Tested directly — a hand-built map with every
      field reversed and a duplicate key decodes to the same value
      both ways.
- [x] the price (CodecBenchmark, compare, quiet box): encode 395 ns
      vs 630 (1.6x), decode 601 vs 1188 (2.0x) — history.tsv
      staged-cbor

### Decisions
- **The Reflect base, extracted** — `Staged.json`'s Mirror walk,
  labels, and the `ok_T` shape-check machinery moved into an
  abstract `Reflect` class; `JsonGen` and `CborGen` extend it and
  differ only in `emit`/`read`. One reflection, two emitters.
- **Cbor.scala refactored so there is ONE encoder** — `Out`/`In`
  were private locals inside `put`/`get`; made public classes with
  named methods (`integer`, `text`, `mapHeader`, `intItem`,
  `textItem`, …) and `put`/`get` rewritten to call them, so the
  staged generator's calls and the fold's calls are the identical
  method, not a second reimplementation of CBOR's varint header.
- **Decode reads a run-time-ordered map by NAME, not by position** —
  the first cut assumed the wire's field order equals the Mirror's
  (true only because both writers here happen to write that order),
  and would have silently miscoded a document with reordered or
  duplicate keys. Refuted before landing by a test built for exactly
  that shape. `Staged.cborProduct` is the fix: a run-time helper
  that reads `n` (key, value) pairs by name into indexed slots — one
  per-field reader SPECIALIZED at compile time, so the win survives
  — then fills absences by the fold's own rule. This is the fold's
  own Map-then-fill algorithm, verbatim, with staged readers instead
  of a Schema redispatch per field.
- **Sums need no such fix** — a CBOR sum is always a one-entry map;
  there is no order to get wrong.
- **Lists need no such fix either** — CBOR arrays are positional by
  the format itself; `Staged.cborElems`/`cborElemsV` walk the
  declared count sequentially, matching the fold, which is also
  sequential (no per-element skip-on-damage rule — CBOR has no
  sentinel error item the way JSON's JErr is one, so a malformed
  element is a real Left, not a skip).

### Out of scope
- Yaml/Xml through the same generator (no hot path names them yet).
- Streaming CBOR (a value larger than fits comfortably in memory) —
  the interpreted fold does not offer it either.

## JSON \u escapes (2026-09-03, json-unicode-escape)

`Json.unquote` had no `\uXXXX` handling. Its catch-all escape case —
written for the single-character escapes (`\"`, `\\`, `\/`) — silently
mishandled the multi-character one too: seeing `\`, it read the next
character (`u`), appended it LITERALLY, and advanced by two. `​`
therefore decoded to the five literal characters `u`, `2`, `0`, `0`,
`b`, not U+200B — and this was not a corner case. Any producer that
escapes non-ASCII as `\uXXXX` (a common, standards-legal JSON choice —
confirmed live against Telegram's own Bot API) had every non-ASCII
character in every string it sent silently turned into garbage, four
to six letters per character, with no error anywhere in the chain.

Found downstream: a chat service consuming Telegram's API decoded
Cyrillic messages into runs of literal hex digits, which explained a
day of "the bot doesn't understand Russian" reports that were never
about intent parsing at all.

The fix decodes a `\uXXXX` escape as one UTF-16 code unit. A surrogate
pair (a codepoint outside the BMP, most emoji among them) needs no
special-casing: two escapes decoding to two `Char`s that happen to form
a valid high/low surrogate pair are automatically a correct Scala
`String`, because that is what a UTF-16 string already is. A malformed
or truncated escape — fewer than four hex digits left before the
string ends, or non-hex characters — is a decode the same way any other
damage in this parser is: `unquote` cannot fail loudly (it returns a
plain `String`, not a `Json`, so there is no `JErr` for it to become),
so a bad escape is left as the literal characters it names, exactly
the wrong-but-safe behavior every other case already had. It does not
throw.

- [x] `\uXXXX` decodes to the named code point
- [x] a surrogate pair reconstructs the correct single character
- [x] the existing single-character escapes are unaffected
- [x] a truncated escape at the end of a string does not throw

### Out of scope

Re-encoding (`Json.print`/`escape`) still emits raw UTF-8 rather than
`\uXXXX` — legal JSON either way, and not the defect: the defect was
one direction only, decoding what someone else already escaped.

## Run-time staging (2026-09-07, staged-runtime)

The Staged fold mode's last out-of-scope item, taken up on the
operator's ask ("make run-time staging, and think where else it could
be useful" — with the condition that it stays optional: switchable
off, unused if not wanted). `Staged.json[A]` folds a TYPE's shape at
compile time; a schema that exists only at run time — a Postgres
composite from the catalog, a tool's parameters as an MCP server
declared them, a JSON frame from R or Python, a `Schema` built from a
JSON Schema document — has no type for the macro, and had only the
interpreter. `RuntimeStaged.json(schema)` is the same generator over
the schema as a VALUE, run through `scala.quoted.staging`: the shape
decides the code at generation time, the compiler in the running
process compiles it, the codec is cached by the schema's identity.

### Interface
- module `okay-staging` (JVM only, `dependsOn(okayCodec.jvm)`, brings
  `scala3-staging`); nothing in okay depends on it.
- `RuntimeStaged.json[A](s: Schema[A]): JsonCodec[A]` — generated
  once per schema (by identity), the interpreter when staging is off
  or the generation fails; never throws for want of staging.
- `RuntimeStaged.enabled` — `-Dokay.staging=off` / `OKAY_STAGING=off`
  (also `false`, `0`, `no`) makes every door the interpreter and no
  `Compiler` is ever made; `force(Some(false))`/`force(None)` from code.
- `RuntimeStaged.lastFailure: Option[(Schema[?], Throwable)]` — the
  last generation that fell back, and why. `isStaged(s)` — a test's
  question.
- `RuntimeStaged.interpreted(s)` — the interpreter as a `JsonCodec`.

### Behavior
- [x] agreement over the whole node vocabulary, on schemas the
      generator sees only as values: products, nesting, Option/List/
      Vector, defaults (absent with a declared default, absent
      optional, absent required, damaged optional, damaged elements),
      sums (every case, a sum in a list, an unknown case, the wrong
      shape), isos (wrap bare, refine's Left, an enumeration's names),
      recursion (delegates to the fold) — encode byte for byte, decode
      Left for Left with the fold's own refusal words (TestRuntimeStaged)
- [x] a schema BUILT at run time — an `SProduct[Seq[Any]]` over parts,
      what okay-sql builds from `pg_type` — stages like a derived one
- [x] the switch: off, the door is the interpreter and stages nothing;
      on, the same schema is generated once and the codec is `eq`
- [x] the price, on the compile-time benchmark's Order (CodecBenchmark,
      compare; `-f 1 -wi 3 -i 5`): encode 233 ns vs 842 interpreted (3.6x; 1.4x of the compile-time staged 165), decode-from-AST 140 vs 683 (4.9x; 1.2x of the compile-time 113); generation 8.7 ms per schema with the compiler warm (the first in a process pays the compiler's own warm-up on top, seconds), so a warm generation is earned back after ~15,000 values (609 ns saved per encode, 543 per decode) — history.tsv staged-runtime

### Decisions
- **Its own module, and a switch, and a fallback — optional three
  ways.** The operator's condition. A program that does not add
  `okay-staging` has no compiler on its classpath; one that does can
  turn the door off at launch without a code change; and a door that
  cannot stage answers the interpreter and says why. Not a flag inside
  okay-codec: the compiler dependency must not be reachable from a
  module that crosses to JS and Native.
- **The schema's functions reach the generated code through a table,
  not through lifting.** `scala.quoted.staging.run` compiles a closed
  expression; a `make`, a `parts`, an iso's `to` cannot be lifted into
  it. So the generated value is a FUNCTION from the schema's node table
  (`Array[Schema[?]]`, the instances the walk recorded) to the codec,
  applied once; each node's handle is a table read at the call site
  and the hot path is the straight-line code around it.
- **Every thunk forced once.** A derived schema's field thunks may
  build a fresh `Schema` instance per call (they do, for a sum's
  cases); the first cut called them twice — once in the walk, once in
  the generator — and the second instance was not in the identity
  table, so the sum's case read the ROOT's product (a
  ClassCastException in the agreement suite). The walk records each
  node's children and the generator reads only those. A walk past
  4096 nodes (a thunk that never returns the same instance) refuses
  and the door falls back.
- **Casts in one place, licensed by the node kind.** A run-time
  schema is erased: the value under an `SInt` node is `Any` to the
  generator. `RuntimeStaged.Unsafe` holds every cast (one per
  primitive, one per node kind, one at the codec's boundary), each
  emitted only under the node kind that proves it; the generator
  itself, the table walk and the tests do not cast. The rule "no cast
  without necessity" met its necessity here and isolated it.
- **Delegation for recursion and the fold's words for refusal** — as
  the compile-time generator: a node met again inside itself calls
  `Json.decode(schema)`, and every cold path (wrong shape, unknown
  case, damaged primitive) is the interpreter's own refusal, so the
  two modes never diverge in what they say.
- **Not a default anywhere.** No okay module calls `RuntimeStaged` for
  the caller; a run-time schema's codec is the interpreter until a
  measured hot path names it (below).

### Where else it could be useful — and the condition for each
The door pays for itself when three things hold at once: the schema is
a VALUE (no type for the macro), it LIVES LONG (thousands of values
per generation, or the compilation is never earned back), and the
FOLD is a measured share of the hot path (the parser, the wire or
the database usually are — staged-codecs found the fold at 0.6 µs of
a 15.2 µs text→value).
- **okay-sql typed rows** — MEASURED AND DECLINED (sql-fold-profile,
  2026-09-07, below). The condition was a profile at ≥30%; the fold
  is 24% of the fastest read there is and less of any real one.
- **JSON frames from R and Python** (specs/r.md, specs/py.md): a
  frame's column schema is a value and a frame is 10^5–10^6 rows.
  Condition: JSON is the wire. Arrow (r-arrow, py-arrow) removes the
  codec altogether and is the better road; staging is for the JSON
  path that stays.
- **A `Schema` built from a JSON Schema document** (structured output
  contracts declared by a server, MCP tool results): the only way such
  a schema can ever have a generated codec. Condition: the document's
  values are decoded at a rate that matters — a model's replies are
  not, a server's event stream might be.
- **okay-persist replay of old entry versions**: an entry's schema at
  version N is a value once the type has moved on; replay of a large
  log decodes millions of them. Condition: CBOR (a `RuntimeStaged.cbor`
  is the same generator with the CBOR emitter, not written until the
  replay profile asks) and a log ≥10^6 entries.
- **okay-script containers**: they already carry `scala3-compiler`
  for the page; adding this module costs only the staging jar, so a
  page's run-time data schemas (a form, a table from a query) could
  take the staged codec at no classpath price. Condition: a page that
  renders enough rows for the fold to show in `MeasureScript`.
- **Beyond codecs, the same shape**: a predicate over typed rows, a
  cue matcher over a run-time pattern set, a validator from a run-time
  declaration — any fold over a VALUE that runs many times against
  many inputs. The generator pattern (walk the value once, hand the
  value's functions through a table, emit straight-line code) transfers
  unchanged.

Where NOT: JS and Native (no compiler); anything decoded a few times
per schema (an MCP tool's arguments, a chat reply, a config file);
per-request schemas (each generation is the `generate` lane's price);
memory-constrained containers where the compiler's heap is the
budget; and every path where the fold is not the bottleneck, which
is most of them until measured.

- (`RuntimeStaged.strict` was that item; taken up on the operator's
  ask, "The strict door" below.)
- A generated recursive method per recursive node (delegation today,
  as in the compile-time generator).
- Automatic use by any okay module.

## Schema thunks once (2026-09-07, schema-thunks-once)

The trap staged-runtime met, fixed at its source (operator: "fix the
traps"). Every edge of a `Schema` is a thunk — that is how a
recursive type's schema terminates — but `Schema.derived` built them
as `() => summonInline[Schema[h]]`, which RE-EXPANDS the derivation
on every call for a subtype without a given of its own (a sum's
cases, always; a field type with no `given`). Two costs nobody had
measured: a fresh `SProduct` per case per value encoded (the
interpreter's `theCase`/`eachField` force the thunk each time), and
an identity that never repeats, which is what broke the staged
generator's node table.

### Interface
- `Schema.once[X](s: => Schema[X]): () => Schema[X]` — by-name in,
  `lazy val` behind the thunk: nothing forced at construction, one
  instance once forced.
- Used by `derived` (fields and cases), the `Option`/`List`/`Vector`
  givens and `wrap`/`refine`/`vocabulary`. No signature moved.

### Behavior
- [x] a sum's case thunk, a product's field thunks, the
      Option/List/Vector element thunks and an iso's under all answer
      the same instance on every call; recursion still terminates and
      the recursive edge IS the given, not a copy; a thunk that counts
      is forced exactly once and never at construction (TestSchemaOnce)
- [x] the interpreter, before/after on the Order (CodecBenchmark):
      measured by ALLOCATION per value (-prof gc; time on a loaded box is noise, bytes are not) on a sum-shaped Owner (a Pet enum, four case values): encode 10160 -> 8144 B/op (-20%), decode-from-AST 5976 -> 4088 (-32%), CBOR encode 7312 -> 5416 (-26%); the same runs' times 1072 -> 817, 830 -> 573, 1221 -> 974 ns (wide error bars); the Order, which has no sum and a given per type, 8016 -> 7968 B/op (-0.6%, the Option/List givens' re-summon) — history.tsv schema-thunks-once

### Decisions
- **Memoise at the edge, not at the door.** The alternative — every
  consumer (the interpreter, the staged generator, JsonSchema, Form)
  caching what it forced — repeats the fix per consumer and leaves the
  next one to meet the trap again. One helper, four call sites.
- **Still lazy.** `once` takes its argument by name and forces it on
  first call, so `given Schema[Tree] = Schema.derived` constructs
  without touching itself; the test holds the recursive edge `eq` to
  the given.
- **A thunk's identity is now a promise** the staged generator may
  rely on; its own walk still records children once (belt and braces:
  a hand-built schema may still hand a fresh instance per call, and
  the 4096-node cap still refuses one that never repeats).

## The codec seam (2026-09-07, staging-seam)

Run-time staging reached, not just built (operator: "add it where it
is useful — okay-script and everywhere you see it"). The observation
that made it one lane instead of twenty: every GENERIC door in the
repository — `def put[A](key, a)(using Schema[A])`, a persisted
topic's `Typed[A]`, a session's state, an HTTP body `json[A]`, a
tool's `args[A]`, a cluster frame — sees its schema as a VALUE; there
is no Mirror for `Staged.json[A]` at a generic door. So one seam,
`Codecs`, is where those doors get their codec, and one `install`
makes all of them staged.

### Interface
- `Codecs.json(s)` / `Codecs.cbor(s)` — a `JsonCodec` / `CborCodec`
  for any schema, from the installed `Provider` (cross-platform,
  okay-codec). `writeJson/readJson/writeCbor/readCbor` are the
  `Json.write/read` and `Cbor.write/read` shapes through the door.
- `Codecs.Provider` (`name`, `json`, `cbor`); `Codecs.Interpreter`
  is the default; `install(p)`, `reset()`, `provider`.
- `RuntimeStaged.cbor(s)` — the CBOR emitter over a schema value
  (`CborGen` of Staged.scala with the node table where the Mirror
  was); `RuntimeStaged.Provider`; `RuntimeStaged.install(): Boolean`
  (false, nothing installed, when the switch is off).
- `okay.codec.Staging.autoInstall(): Outcome` (JVM only, okay-codec's
  `scala-jvm`) — finds `okay.staging.RuntimeStaged` by name and
  installs it; `Installed | Absent | Refused(why)`.
- okay-script depends on okay-staging and `Serve` installs at boot,
  printing which way it went; `-Dokay.staging=off` keeps the
  interpreter.

### Behavior
- [x] the seam's default is the interpreter and its answers are the
      fold's; an installed provider is what every door answers; reset
      returns (TestCodecs, JVM/JS/Native)
- [x] the CBOR emitter agrees with `Cbor.write/read` item for item and
      Left for Left: products, defaults and an absent field, sums
      (every case, unknown case, wrong shape), iso, recursion, and the
      nodes it leaves to the fold (bytes, char) (TestRuntimeStagedCbor)
- [x] `install()` makes `Codecs.writeJson/readJson/writeCbor/readCbor`
      go through the generator (`isStaged` / `isStagedCbor` after one
      call); `Staging.autoInstall()` finds the module by name; off is
      `Refused` and the interpreter stays
- [x] the doors routed: okay-script `Application.put/value`,
      `Live.encode/decode` (session state, CBOR); okay-ui `Sessions`
      snapshots, `Form.decode`; okay-persist `Typed` (one codec per
      topic), `Snapshots`, `Configs`, `WireProtocol`/`Wire` frames,
      `RaftWire` messages, `RaftStore` ops; okay-http `Http.json`,
      `Server.json`, `as[A]`; okay-cluster `Remote` frames; okay-agent
      `ToolSpec.args`; okay-llm request bodies, stream events and
      `Structured`; okay-cache Redis values; okay-docs-mongo; okay-conf
      `read`; okay-obs spans. `Json.write`/`Cbor.write` themselves are
      untouched — a caller who wants the fold, verbatim, still has it.
- [x] the price of the seam, and of staging behind it, on the Order
      (CodecBenchmark): through the seam with the interpreter (nothing installed) encode 864 ns vs 860 direct, decode-from-AST 654 vs 598 — a volatile read and a wrapper, within the interpreter's noise; through the seam with okay-staging installed encode 235 ns and decode 164, the same as the generated codec called directly (236 / 142-164 across runs) — the seam costs nothing measurable over the codec behind it. The first seam run had the staged door at 2.7 µs: the launch switch read `sys.env` per call (fixed, see Decisions) — history.tsv staging-seam

### Decisions
- **One `@volatile` reference, no registry, no ServiceLoader.** A
  provider is installed by a call; a program that installs nothing
  pays a thin wrapper over the fold it already paid. Reflection lives
  in exactly one JVM-only object (`Staging`) for the module that
  cannot depend on the compiler; okay-script, which already carries
  the compiler, depends on okay-staging outright and installs
  without reflection.
- **The compiler's one thread.** dotty's `ContextBase` asserts the
  thread that first touched it ("illegal multithreaded access"), even
  when a lock serialises the callers — two test suites in parallel
  found it. Every generation now runs on one daemon thread
  (`okay-staging`) and the caller waits; the first generation in a
  process, and every later one, is on that thread.
- **The switch is read once.** The first seam benchmark put the
  staged door at 2.7 µs per value against 0.24 for the generated code
  and 0.86 for the interpreter: `enabled` read
  `sys.env.get("OKAY_STAGING")` per call, and `sys.env` copies the
  whole environment into a Map every time. A launch switch is a
  constant for the process; it is a `lazy val` now, with the volatile
  override kept for tests. The lesson is the benchmark's: measure the
  DOOR callers use, not only the object behind it.
- **Hot doors hold the codec once.** `Typed[A]` and the cluster's
  `Sender` take `Codecs.cbor/json(schema)` at construction; a door
  that is called with a fresh schema every time (`put[A]`) asks the
  seam per call, which is a cache lookup by identity when staged and
  a small wrapper when not.
- **Still not a default.** With nothing installed every door is the
  interpreter, on every platform; installing is one explicit call at
  a program's boot, and the container prints which way it went.

### Out of scope
- A strict-JSON emitter over a schema value (`RuntimeStaged.strict`)
  — `readStrict` doors are few and typed; when one is generic and hot.
- Per-codec hoisting of a product's decode arrays (they are built per
  call, as the compile-time generator builds them).

## The row fold's share (2026-09-07, sql-fold-profile)

The condition run-time staging set for okay-sql, measured, and the
answer is no (operator: "do the ones still open"). Two things the
guess had missed. First, `Typed.planOf` ALREADY hoists the fold's
expensive half: the column-to-field matching resolves once per
statement, against the driver's `describe`, and what runs per row is
a `decodeCell` per field into an array and one `make` — the shape a
staged generator would emit anyway, minus the per-cell match on the
field's `Shape`. Second, the remainder is small next to any driver.

`MeasureSqlFold` (okay-jdbc, Live-tagged, medians with the warmup
discarded, H2 in memory, 2000 rows of six columns — one Long, one
String, an Option[Int], a Double, a Boolean, another String):

| what (per 2000 rows) | median |
|---|---:|
| the fold alone (frames replayed from memory, typed) | 0.29 ms |
| the same replay, frames only | 0.01 ms |
| **the fold itself** | **0.27 ms** = 0.14 µs per row |
| end to end, typed | 0.69 ms |
| end to end, raw frames (the driver alone) | 0.61 ms |
| **the fold's share of a row** | **10.5%** |

> CORRECTED 2026-09-07 by `sql-plan-cells`. The first run of this
> table reported 0.45 µs per row and a 24.1% share, on 3 warmups and
> 7 samples. At 0.6 ms a run that is not warm: two runs of the SAME
> code differed by half, and raising the warmup to 50 and the samples
> to 31 settled it at 0.14 µs and 10.5%. The verdict the table
> supports is unchanged and stronger — the fold was already three
> times smaller than the number that declined run-time staging.

- [x] the fold is 0.14 µs per row at six columns, and 10.5% of a read
      whose driver is an in-memory H2 — the CHEAPEST driver that
      exists here. A Postgres read over a socket pays parsing, framing
      and the network for the same row, so the same fold is a smaller
      share of it, never a larger one; the condition cannot be met by
      the drivers this repository has.
- [x] the numbers measure real work, not an empty stream: the same
      read decodes every row and the values are the fixture's
      (a second test asserts three of them by hand)

### Decisions
- **No staged codec in okay-sql.** Not "not yet": the door would have
  to beat 0.14 µs per row to matter, against a driver that costs nine
  times that on the friendliest possible setup, and it would put the
  Scala compiler inside every process that opens a database. The
  spec's own condition, honestly applied, says no.
- **What WOULD move that number** is not staging: the per-cell match
  on `Shape` could be resolved into an array of cell decoders when
  `planOf` resolves the columns (the same hoisting, one level
  deeper), which needs no compiler and no new module. Filed as
  `sql-plan-cells`, unclaimed, with this profile as its baseline —
  and 0.14 µs per row is small enough that it stays unclaimed until
  a profile of a real workload names it. (Taken up anyway on the
  operator\'s word, measured and DECLINED — "The cell decoders that
  were not faster", below.)
- **The measurement stays.** It is the baseline any future claim
  about row-decode cost must beat, and it is Live-tagged so a loaded
  CI box never turns it into a red build.

## The strict door (2026-09-07, staged-strict)

The codec seam's last out-of-scope emitter, taken up on the operator's
ask ("do the ones still open"). `Json.readStrict` puts characters
straight into the schema with no tree; `Staged.strict[A]` generates
that read for a TYPE. `RuntimeStaged.strict(schema)` is the same
generator over the schema as a VALUE — `StrictGen` of Staged.scala
with the node table where the Mirror was — so a generic strict door
(one that has a `Schema` and no Mirror) can have it too.

### Interface
- `RuntimeStaged.strict[A](s: Schema[A]): StrictJsonCodec[A]`, cached
  by identity like `json`/`cbor`; `isStagedStrict(s)`.
- `Codecs.Provider.strict` — DEFAULTED to `JsonStrict.read`, so every
  provider written before this door existed stays correct without
  saying anything; `Codecs.strict(s)` and `Codecs.readStrict[A](text)`
  are the doors, `RuntimeStaged.Provider` overrides with the generator.

### Behavior
- [x] agreement with `JsonStrict.read` over the node vocabulary, the
      value on a well-formed document and the SAME Left on every
      refusal: wrong primitive, truncated input, a missing required
      field, the wrong shape, trailing input, an unknown field
      (skipped, as the fold skips it), an unknown sum case, a
      two-entry object where a sum wants one, an iso's own Left,
      recursion, and the nodes it leaves to the fold (Char, bytes)
      — TestRuntimeStagedStrict
- [x] the seam: `Codecs.readStrict` is the interpreted walk until
      okay-staging is installed and the generated reader after; off,
      it is the walk again
- [x] the price on the Order (CodecBenchmark): text to Order 385 ns against the interpreted strict door's 901 (2.3x) and the compile-time generated 307 (1.25x of it); circe's fused parse+decode 706 ns on the same text, so the run-time generated strict read is 1.8x faster than circe with no type known at compile time — history.tsv staged-strict

### Decisions
- **A defaulted method, not a new trait.** `strict` on `Provider`
  carries its own interpreted body, so adding a third format to the
  seam broke no implementation — the test's fake provider, written
  for two, compiles unchanged. A seam that grows should not make its
  implementers redundant work.
- **The fold's words on every cold path, again.** A node the
  generator does not know, and a type met again inside itself, call
  `Reader.get` — the same walk `readStrict` runs — so the refusals
  never diverge; that is what the agreement test checks, refusal by
  refusal.
- **Written ahead of a named workload, and said so.** The earlier
  out-of-scope line was "when a strict door is generic and hot";
  none is today. The operator asked for the open items to be done,
  so it exists and is measured. The seam's default stays the
  interpreter, so nothing pays for it until a program installs
  staging.

## The cell decoders that were not faster (2026-09-07, sql-plan-cells)

`Typed.planOf` resolves the column-to-field mapping once per
statement, and the per-row loop then walks the `Shape` ADT for every
cell. This lane compiled each field's shape into a
`SqlValue => Either[String, A]` at plan time, typed all the way down
(no new casts), so a row became an array of calls.

It is slower, consistently, and the reason is worth writing down.

| implementation | the fold itself (2000 rows, 6 columns) |
|---|---:|
| the ADT walk (shipped) | 0.22, 0.22, 0.24 ms → **0.11-0.12 µs/row** |
| compiled cell decoders | 0.26, 0.27, 0.26 ms → **0.13-0.14 µs/row** |

Three paired runs, each pair back to back on the same box, the two
implementations swapped between them.

- [x] DECLINED, and reverted. A closure call per cell is a
      megamorphic virtual call plus a tuple destructure; the match it
      replaced is a small, monomorphic-per-call-site dispatch the JIT
      already predicts and inlines. "Resolve it once" is the right
      instinct for a LOOKUP (which `planOf` already does for the
      column mapping) and the wrong one for a BRANCH the JIT is
      better at than we are.

### What the lane did leave behind
- **The instrument was wrong, and that mattered more than the
  change.** `MeasureSqlFold` ran 3 warmups and 7 samples of a 0.6 ms
  body: two runs of the SAME code differed by half, and the profile
  it produced in `sql-fold-profile` (0.45 µs per row, 24.1%) was
  three times the truth. At 50 warmups and 31 samples it settles at
  0.14 µs and 10.5%, and the corrected table is above. The verdict
  that profile supported — no staged codec at the database seam — is
  unchanged and stronger.
- **A decode suite the module did not have.** `TestRowDecode` was
  written to hold the refactor to its predecessor's answers, and it
  outlived the refactor: primitives, an Option present and absent, a
  refining wrapper that refuses, an array with a damaged element, a
  composite with the wrong arity, and NULL where it is not allowed —
  values AND refusal words, with no database.
- **A rule for the next such idea**: measure the instrument before
  trusting it, and pair the arms in one run. Both of this lane's
  surprises came from single-arm numbers taken minutes apart.


## The parser everything went through (2026-09-07, json-parse-fast-road)

`Json.parse` was `value(cst(s))` — tokenize the text into a full
lossless CST, then project a `Json` out of it. `Json.parseValue` was
one strict pass with a fallback to that road when it was not sure.
Two roads, and the default was the slow one.

Found while measuring something else. `py-arrow` is filed as "frames
via pyarrow, once the JSON-frame road hurts", so the first question
was whether it hurts. A 500k-row × 3-column frame through okay-py:

| | before |
|---|---|
| round trip | 9.7 s |
| our encode | 0.2 s |
| **`Json.parse`** | **4.9 s** |
| our own decode walk | 0.03 s |
| `Json.parseValue`, same text | **0.06 s** |

Sixty percent of the round trip was our parser, for a value the assert
says is EQUAL. The Python boundary — the thing py-arrow proposed to
replace — was a minority of the cost.

**The "79x" this section first claimed was measured badly** and is
corrected below (json-cst-batch-road): those were SINGLE timed calls
after one warm-up, which at this scale prices the JIT as much as the
code. Best-of-twelve on 9.3 MB puts the two roads 37x apart, not 79x.
The conclusion does not change and the fix does not change; the number
does, and a number in a spec is a claim.

So `Json.parse` IS the fast road now, falling back to the lossless one
whenever `JsonValue.parse` is not sure. Nothing else changed:

- **the values are the same**, and `TestJsonValue` still proves it —
  a corpus of well-formed and damaged documents plus a PREFIX SWEEP,
  every truncation of every one, both roads compared. That test now
  names `Json.lossless` explicitly, because otherwise it would have
  quietly become `parse == parse`.
- **totality is the same**: damage falls through to the CST road and
  gets `JErr` leaves exactly as before.
- **`parseValue` is gone** rather than deprecated — `parse` is that
  road, and keeping a second name for it is the drift this repository
  deletes elsewhere. Its ten callers now say `parse`.

Measured after, same frame: **round trip 9.7 s → 0.94 s**, and 100k
rows 1.3 s → 0.19 s. Every module that decodes JSON through
`Codecs.readJson` was paying the difference — okay-http, okay-agent,
okay-mcp, okay-llm, okay-conf, and the two foreign-runtime engines.

The lesson worth keeping is not about JSON. A module had two roads to
the same value, differing by 79x, and the DEFAULT was the slow one for
long enough that a separate feature got filed to work around its
symptom. `py-arrow` is re-filed with an honest number.


## The batch road (2026-09-07, json-cst-batch-road)

The operator's follow-on question to the section above: why is the
lossless road slow, and how is it made fast.

**Why.** It was not JSON, and not the tree. `Json.cst` fed the source
ONE CHARACTER AT A TIME through the effect system —
`Writer.tell(c).flatMap(...)` per char — into two transducer stages
and a `LazyList`. Just moving the characters through `Writer`, with no
lexer attached, cost 61-71 ms on 1.68 MB: four times the entire fast
value parse. The real work — lexing, instructions, building — was
about 26 ms each.

**How.** No new machinery. `Parse.full(sc, step)` is documented as
"the common case: a per-token driver with no state of its own", and
`JsonParse.instrs` says "no cross-token state" in its own comment —
they were written for each other. So a batch parse needs no driver
stage and no streaming at all:

```scala
def cst(s: String): Cst[K] = Parse.full(JsonLex.scan, JsonParse.instrs)(s).tree
```

The same `Scan` and the same `instrs`, so there is no second grammar
to keep in step. **JSON was the last codec on the per-char road** —
Xml already used `Parse.fullWith`, Yaml a hand loop.

Best of twelve, 9.3 MB:

| | old | new |
|---|---|---|
| `Json.cst` | 1129 ms | 334 ms |
| `Json.lossless` (tree + projection) | ~1196 ms | 402 ms |
| `Json.parse`, the fast value road | 32 ms | 32 ms |

**What the evidence is.** `TestJsonCst` compares the two roads' TREES
— not their values, which is a weaker claim — over the same corpus
`TestJsonValue` uses, with the same PREFIX SWEEP: every truncation of
every document, well-formed and damaged. It also asserts the lossless
law still holds (`render` puts every document back byte for byte) and
that the diagnostics are the same errors in the same order. The corpus
moved to `JsonCorpus` so both files provably mean the same documents.

**What was NOT done, and why.** Two things measured as not worth it:

- Skipping the reparse snapshots a batch `cst` throws away looked like
  8% on a single-shot run and is 1.5% — inside the noise — on best-of-
  twelve. The simpler code stands.
- `Scan.step: (S, Char) => (S, Vector[Token[K]])` allocates a tuple
  per character, which is the next wall. Fixing it means changing an
  interface four codecs and okay-rag implement, and NO main-source
  caller in this repository uses the lossless road at all: it serves
  `Json.parse`'s damage fallback, the incremental reparse story, and
  the tests. Optimising it further would be speculation, so the
  measurement is recorded and the interface is left alone.

At ~12x the fast road, the lossless one now costs about what keeping
every token, span and piece of trivia should cost.


## What was left in the lossless road (2026-09-07, json-projection-alloc)

Asked after the batch road landed: is there more. Two places, both
inside `Json.scala` — unlike the filed `scan-step-allocation`, which
would cost an interface four codecs implement.

**The projection answered a `Vector` from every node and every leaf.**
`Vector(JNull)`, `Vector(JBool(b))`, `kids.flatMap(values)` building an
intermediate at every level — one Vector per token, on a walk whose
whole job is tokens. `pairs` added `grouped(2)`, another Vector per
field. It appends into a builder now, and a field is read in one pass.

**`unquote` built a StringBuilder for every string token**, plus a
`stripPrefix` and a `stripSuffix` substring, even for a string with no
escape in it — which is nearly all of them. It now returns the
substring directly when there is no backslash.

**The number, honestly.** A/B in ONE run, because across runs the GC
state of a million-object tree swamps the difference (the first
cross-run reading said 2x and was noise):

| | |
|---|---|
| projection, old shape | 38.4 / 39.2 ms |
| projection, new | 34.1 / 34.3 ms |

About 12% on that stage and about 6% of the whole lossless call, which
now runs ~74-82 ms against ~78-92 before. The old shape in that A/B is
a simplified reconstruction — it skips `unquote` and the error text —
so 12% is a floor rather than a ceiling, and `unquote`'s own saving is
not separately measured.

Modest, and said as modest. What justifies it is that the code is also
simpler — one pass instead of `flatMap` + `grouped(2)` + `collect` —
and that the behaviour is guarded, not asserted: `TestJsonValue`'s
prefix sweep compares the fast value road against this projection over
every truncation of every document.

**This is a good place to stop.** The remaining candidates are
`Scan.step`'s per-character tuple and `JsonParse.instrs`' per-token
Vector; both are shared interfaces, and no main-source caller uses the
lossless road at all. They want a consumer before they want a change.


## The escape on the hot side (2026-09-07, json-escape-alloc)

The mirror of the projection work, on the road that is actually used.

```scala
def escape(s: String): String =
  s.flatMap { ... case c => c.toString }     // a String PER CHARACTER
```

Unlike the lossless road, this one has callers everywhere:
`Json.print`, `Staged.scala` (the compile-time encoder) and
`RuntimeStaged.scala` (the run-time one). Every string through the
staged doors this spec measures at 385 ns was paying an allocation per
character.

It is `unquote`'s shape now — look first, and answer the INPUT when
there is nothing to do. Best of twelve, 200k strings:

| | old | new |
|---|---|---|
| nothing to escape (the common string) | 17.0 ms | 6.0 ms |
| every string escapes | 37.4 ms | 13.0 ms |

**2.8x and 2.9x** — the win is in BOTH columns, which is the part worth
noticing: the old version allocated per character whether or not
anything needed escaping, so the fast path is not what earns most of
this. It is not allocating a String to hold one character.

### The five characters, and the test written first

`escape` escapes exactly `"`, `\`, `\n`, `\t` and `\r`. It does NOT
escape `\b`, `\f` or control characters — this project's own choice,
recorded in `unquote` — and `Json.scala` says the two "must agree
exactly, not just resemble". `TestJsonEscape` pins that: each of the
five, each of the ones deliberately left alone, strings with nothing
to escape, occurrences at both edges, and a round trip through both
read roads.

It was written BEFORE the rewrite, against the old implementation, and
it earned that immediately: the first version of the new `escape` used
Scala's `StringBuilder`, which has an `append(Any)` — so
`b.append(s, 0, i)` silently appended the TUPLE `(s, 0, i)` as text
rather than the prefix. A test written after the change would have
been written against that behaviour. It is `java.lang.StringBuilder`
now, whose `append(CharSequence, int, int)` is the one meant.
