# Schema fold: the catamorphism the header promised

## Overview

`Schema.scala`'s header says every derivation "is a CATAMORPHISM
over this one structure with its own algebra". That sentence
describes the design; nothing in the file implements it. There is no
`fold`. Eight algebras exist today — `Json`, `Cbor`, `Yaml`, `Xml`,
`JsonSchema`, `Form`, `ToolSpec`, `Typed` (SQL), `Compat` — and every
one of them re-matches the GADT by hand and re-writes the recursion
by hand.

The cost of that was measured on 2026-09-10/11, not argued: the same
depth defect (native recursion on a VALUE's own nesting, unbounded
once `Codecs.maxDepth` went) turned up in five of them, and the same
fix — `Codecs.NativeThreshold`, then `Cont.defer` — was written five
times: `Cbor.get`, `Json.decode`, `Json.encode`, `Form.render`,
`Form.errorsOf` (`iterative-recursive-decode.md`,
`encode-side-depth-safety`, `form-recursive-depth-safety`). Twice the
by-hand copy was itself wrong on the first draft (`Cbor.putC`'s field
order; `Json.encode`'s quadratic string building). One missing
primitive, not five bugs.

The operator's ask, in their words: define an entity ONCE, then read
and write it to a database, to bytes, to strings, validate it, show
it in a form, edit it — uniformly, cheap at run time, strict at
compile time. `Schema[A]` already IS the "once"; this spec is the
"uniformly": the fold every algebra is written against, so a new
algebra is thirty lines with no `match` on the GADT and no knowledge
of depth.

## Design

Two primitives, because there are two recursions.

**The schema recursion** — walking the SHAPE: what fields, what
cases, what wraps what. It is finite (a type's definition), lazy at
every edge (thunks), and cyclic for a recursive type (`Tree`'s `kids`
edge leads back to `Tree`). `Schema.fold` walks it ONCE and ties the
knot: an algebra folded over a recursive schema answers a recursive
`F[A]` that refers to itself lazily, exactly as the schema does.
`JsonSchema.of`, `ToolSpec`, `Typed.shapeOf`, `Compat` are folds of
this kind alone — they never see a value.

**The value recursion** — walking a VALUE `a: A` under its schema:
this field's value, this case's payload, this list's elements. It is
as deep as the data, which after `remove-codecs-maxdepth` is as deep
as a sender chooses. This is where all five defects lived. `fold`
does not and cannot own it — the fold builds the interpreter, and the
interpreter's walk over the value is the algebra's own code. What the
spec owns instead is `Step`: the ONE depth-aware shape a value-walking
algebra composes with, so the threshold and the trampoline are written
once, here, and never again in an algebra.

Decoders that read an external STREAM (`Cbor.get`, `JsonStrict.
Reader.get`) are neither: they recurse on the schema and the bytes at
once, skip fields the schema does not declare by reading them off the
wire, and refuse mid-stream. They are not a catamorphism over a
schema; they stay hand-written (Out of scope).

### Why not one primitive

A single `fold` whose `F[A]` is "a function of the value" would hide
the value recursion inside closures the fold composed — `encoder(Tree)
= kids => encoder(Tree)(kid)` — and that closure chain is native
recursion again, one frame per level. The 2026-09-10 arc measured
exactly that shape overflowing at ~5 000 levels. Depth safety has to
be a property of how a value walk is SEQUENCED, and sequencing is the
algebra's business. So `Step` is a separate, opt-in shape, and the
spec's claim is narrower and true: an algebra that composes with
`Step` cannot have the defect; one that does not must say why.

## Interface

```scala
object Schema:

  /** a lazily folded edge at the schema's own existential type — a
    * product field's `?`, a sum case's `? <: A`. A type MEMBER, not
    * `F[?]`: applying an abstract `F` to a wildcard is unreducible in
    * Scala 3 (found at the first compile of stage 1). Forcing twice
    * folds once. */
  trait Edge[F[_], +B]:
    type X <: B
    def apply(): F[X]

  /** what an algebra answers per node; F is the algebra's carrier.
    * A PARAmorphism, not a plain cata: `product`/`sum`/`iso` get the
    * node itself beside its folded edges, because a JSON Schema
    * renders a field's DEFAULT with the field's own schema
    * (`defaultAt`), an enumeration's vocabulary with `under`'s, and a
    * value algebra needs `parts`/`make`/`caseOf` — all on the node. */
  trait Algebra[F[_]]:
    def int: F[Int]
    def long: F[Long]
    def double: F[Double]
    def bool: F[Boolean]
    def string: F[String]
    def char: F[Char]
    def bytes: F[Array[Byte]]
    def option[A](of: () => F[A]): F[Option[A]]
    def list[A](of: () => F[A]): F[List[A]]
    def vector[A](of: () => F[A]): F[Vector[A]]
    def product[A](p: SProduct[A], fields: Vector[(String, Edge[F, Any])]): F[A]
    def sum[A](su: SSum[A], cases: Vector[(String, Edge[F, A])]): F[A]
    def iso[A, B](iso: SIso[A, B], under: () => F[B]): F[A]
    /** a NAMED node met again while still being folded — the back edge
      * of a recursive type. Only a strict algebra sees it; a lazy one
      * forces the edge after the node finished and gets the memoised
      * node. A JSON Schema answers `{"$ref": "#/$defs/name"}` here. */
    def ref[A](name: String): F[A]

  /**
   * The catamorphism. Memoised by schema IDENTITY (`Schema.once`
   * makes every edge answer the same instance, a derived given is
   * evaluated once): a finished node is answered from the table, a
   * named node still on the path is handed to the algebra as `ref`.
   * Three casts, each restoring only what erasure took: the table
   * read-back, and the two edge thunks whose `?`/`? <: A` the schema
   * stored erased — the same position `derived` and `eachField` are
   * in. No value is ever cast.
   */
  def fold[A, F[_]](s: Schema[A])(alg: Algebra[F]): F[A]

  /**
   * The value walk, depth-aware, written once. `Step[A]` is "given an
   * A at this nesting level, do this algebra's work and answer R,
   * stack-safely": below `Codecs.NativeThreshold` a plain call, at or
   * past it `Cont.defer` — the split every fixed door carries today,
   * lifted out of them. An algebra whose carrier is `Step` (or built
   * on it) gets depth safety by composition:
   *   product: fields.foldLeft over `Step.child(fieldStep, fieldValue)`
   *   list:    elements the same way
   * — one node forced per iteration of `/`'s loop, in order.
   */
  opaque type Step[-A, R] = (A, Int) => R /> R
  object Step:
    def leaf[A, R](f: A => R): Step[A, R]
    /** descend ONE level: the threshold check and the defer live here */
    def child[A, R](s: () => Step[A, R], a: A, open: Int): R /> R
    def run[A, R](s: Step[A, R], a: A): R          // = reset(s(a, 0))
```

`Step`'s `R` is the trampoline's fixed answer type — the same `R`
discipline `getC`/`printIntoC` thread today (`iterative-recursive-
decode.md`, "R is the FINAL answer type ... fixed ONCE"). An algebra
that needs to combine children into a value (`Json` from fields)
returns `R = Json`; one that side-effects into a buffer
(`StringBuilder`, `Cbor.Out`) returns `R = Unit` and the buffer is
closed over — `Cbor.putC`'s ordering rule ("every side effect for one
item INSIDE that item's own defer") is then `child`'s contract, not
each algebra's discipline.

## Behavior

Stage 1 — `fold`:
- [x] `JsonSchema.of` rewritten as `fold(s)(JsonSchemaAlgebra)`
      answers byte-for-byte what it answers today, over every schema
      in the repo's test suite (the model-facing rendering is prompt
      text: `jsonschema-render-is-prompt-text` — `TestEvalJournal`
      before the gate)
- [x] a recursive schema (`Tree`, `Chain`, okay-ui's own `Ui`) folds
      to a FINITE value: the fold terminates, and the result refers
      to itself through the thunk (`fold(tree)(alg)` forced twice at
      the `kids` edge is the SAME `F[Tree]` — identity, not equality)
- [x] no VALUE cast introduced; three type-restoring casts in `fold`,
      isolated and named (the table read-back, the two erased edge
      thunks) — the same class as `derived`'s own on a sum's cases
- [x] ~~`Compat.compare` on the fold~~ — WRONG in stage 0, corrected
      in stage 1: `Compat` walks TWO schemas zipped, with its own
      cycle set, a zygomorphism over a pair. Not a fold over one
      schema; it stays as it is.
- [x] a recursive schema through `JsonSchema.of` answers `$defs`/`$ref`
      where the hand-rolled `of` recursed for ever — checked against a
      verbatim copy of the old code (StackOverflowError on `Tree`),
      not asserted from memory

Stage 2 — `Step`, and the value algebras move:
- [x] `Json.encode` as `fold` + `Step`: the `TestEncodeTrampoline`
      fixtures (100 000 levels, the two-field `Two`, the SUM `Chain`)
      pass unchanged; `Json.write(t) == old Json.write(t)` on every
      value in `TestLaws`
- [x] `Form.render` and `Form.errorsOf` the same; `TestFormDepth`
      unchanged
- [x] `Cbor.put` the same; `Cbor.putC`'s field-order regression
      witness passes — and `Step.child` carries a test that a
      side-effecting algebra sees key, value, key, value, so the rule
      is proven ONCE
- [x] MEASURED, JMH, `uptime` first, ≥3 forks (`jmh-load-not-just-
      forks`, `one-jmh-fork-lied-53-percent`): `CodecBenchmark`'s
      interpreted encode lanes within noise of today's hand-rolled
      ones. Bar: no lane worse by more than its own error bar. The
      staged doors (`Staged.json`/`cbor`) are untouched by this spec
      and must read identical.
- [x] the hand-rolled `encodeNative`/`encodeIntoC`, ~~`printIntoC`~~ (stays: `print` walks a `Json`, not a schema — not a fold),
      `renderC`/`fieldC`/`sumUiC`/`listUiC`, `errorsOfC`/`listErrorsC`
      are DELETED, not kept beside the fold

Stage 3 — the ninth algebra, validation:
- [ ] `Validate[A]: Json => Validated[Vector[(String, String)], A]` —
      applicative, ALL errors, dotted paths — as a fold; `Form.errors`
      becomes `Validate` (it already collects everything, by hand)
- [ ] `Json.decode` is NOT changed: it stays `Either` (first error,
      monadic), and the two are the same fold under two applicatives
      — a test proves `Validate(j).toEither.left.map(_.head) ==
      decode(j)` on the error cases where decode's first error is
      Validate's first

## Out of scope

- The stream decoders `Cbor.get`, `JsonStrict.Reader.get`,
  `Cbor.In.skipItem`: they recurse on the schema AND the bytes,
  skip undeclared fields by reading them, refuse mid-stream. Not a
  catamorphism over a schema. They keep their own (already
  trampolined) code.
- `Json.decode` on the fold. It could be (it walks a `Json` VALUE),
  and stage 3 shows the shape; but it is the hottest interpreted door
  and `decodeC` is fresh — move it only with its own JMH bar, as its
  own lane.
- `Staged`: compile-time specialisation is a fold evaluated at
  compile time, and `fold` is the right thing for `Staged` to become
  `inline` over — but not here. Staged's numbers are the bar this
  lane must not touch, not the thing it changes.
- Typed paths (`Schema.path[A].field("city")` → a checked lens on
  `A`). The operator's third ask. `JsonOptic.path` is most of it over
  `Json`; over `A` it needs the field NAME checked at compile time,
  which is a macro over the Mirror, a different lane with a different
  risk. Filed in BACKLOG when this spec lands.
- Any new algebra beyond `Validate`. Yaml/Xml/ToolSpec/Typed move to
  the fold when someone next touches them, not as a sweep.

## Decisions

- **Two primitives, not one** — `fold` for the shape, `Step` for the
  value. A single primitive would hide the value recursion in
  composed closures, which is the exact defect this spec exists to
  retire (Design, "Why not one primitive").
- **Thunked edges in `Algebra`'s own signature.** An algebra author
  sees `() => F[A]` per field and must be lazy there. The alternative
  — strict `F[A]` with the fold forcing edges — cannot fold a
  recursive schema at all (it would unfold forever). Laziness is the
  knot, so it is in the type.
- **Memoise by identity, and say so.** `Schema.once` exists because
  a thunk that answers a fresh instance per call broke a staged
  generator's node table (`schema-thunks-fresh-instances`). `fold`
  relies on the same thing: identity is how it knows it is back at
  `Tree`. A schema built without `once` folds to an infinite
  unfolding — a `derived` one never is, and the fold's doc says the
  rule.
- **Delete the hand-rolled twins.** Keeping `encodeIntoC` beside the
  fold "for speed" is the situation this spec ends: two copies of one
  recursion, one of them the one nobody fixes. The JMH bar decides
  whether the fold is fast enough; if it is not, THAT is the lane's
  finding and the fold is not landed for that door.
- **`Validate` is a new algebra, `decode` is not rewritten.**
  Accumulating errors changes what `Form.errors` shows a user
  (nothing — it already accumulates) and what the wire answers (a
  `Left` with one message, today, and many callers match on it).
  Same fold, two applicatives; the wire's contract is unchanged.

## Results

Stage 1 (2026-09-11, schema-fold-1): `Schema.Edge`, `Schema.Algebra`,
`Schema.fold`; `JsonSchema.of` moved. Two things the spec got wrong at
stage 0, corrected above: `F[?]` is unreducible (so `Edge` with a
type member), and `Compat` is not a fold. One thing it did not know:
the hand-rolled `JsonSchema.of` looped for ever on any recursive
schema — no caller had ever asked it one. It now answers `$defs`/
`$ref`, which is what a model or an OpenAPI document needs for a
`Tree`. `TestSchemaFold` keeps the old `of` verbatim and proves
byte-for-byte on every non-recursive shape, both vocabulary settings.

Stage 2 (2026-09-11, schema-fold-2): `Schema.Step` (`leaf`/`via`/
`adapt`/`option`/`fields`/`one`/`elems`/`node`), `Schema.Folded` (the
per-schema identity memo across calls — a fold is per schema, an
encode per value), and the four value doors moved: `Json.encode`,
`Cbor.put`, `Form.render`, `Form.errorsOf`, each's `*Native`/`*C` pair
deleted. Interface refinements the stage found: `Step` carries an
ENVIRONMENT (`E`: a `StringBuilder`, a `Cbor.Out`, a form's errors and
key) so a node allocates nothing per call; `Algebra.option`/`list`/
`vector` are paramorphic too (`Form`'s validator hands an option or a
wrapper WHOLE to the decoder, as the old `errorsOf` did, and needs the
node for that); `Step.node` is the road for a walk whose value is not
the schema's own type (a form walks a `Json`).

MEASURED, same JMH invocation per pair, the old code kept verbatim
as `LegacyJsonEncode`/`LegacyCborPut` lanes in `compare` (the
before/after-in-separate-runs shape is what `jmh-load-not-just-forks`
refuses), 5 forks, `uptime` first:

| lane (ns/op) | fold | legacy | staged control |
|---|---|---|---|
| encodeInterp (load 15) | 337.6 ± 26 | 381.3 ± 8 | 131.7 ± 1.4 |
| encodeSumInterp | 372.5 ± 41 | 496.7 ± 47 | — |
| cborEncodeInterp (load 4.6) | 489.4 ± 30 | 502.5 ± 6 | 299.2 ± 1.2 |
| cborEncodeSumInterp | 521.2 ± 59 | 696.5 ± 22 | — |

The bar was "not worse than its own error bar"; the finding is
better: the sum lanes are 25% faster with non-overlapping ranges, the
JSON product lane 11%, the CBOR product lane inside the bars (not
claimed faster, claimed not slower). The reason is what a fold IS:
the GADT dispatch and the field zip are resolved ONCE per schema at
fold time, where the interpreter re-matched the schema at every value
node — and the identity lookup `Folded` adds per call is cheaper than
that. `Staged` is untouched and reads the same.

What did NOT move: `Json.print` (walks a `Json`, not a schema — not a
fold), and `Json.decode`/`Cbor.get`/`JsonStrict` by the spec's own
scope.
