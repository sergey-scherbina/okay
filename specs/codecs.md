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
