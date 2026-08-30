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

## Out of scope
- schema languages/validation; a transport module (its own, later)
