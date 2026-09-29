# okay-refine — reading anything out of anything, one pattern at a time

Status: specification and stage 1, 2026-09-29, written before the code
at the operator's word: *«на входе некий файл описывающий финансовый
дериватив в каком-нибудь формате — понять что это за формат и что за
дериватив, классифицировать его, прочитать и обработать правильно»*.
Owner lane family: `refine-*`.

## 1. Overview

A document arrives and nothing is known about it: not its format, not
what it describes, not which of the forty shapes of "an interest-rate
swap" it is. Every question about it is the same question at a
different level — *is this one of these?* — and every answer makes the
next question askable: bytes turn out to be UTF-8 text, the text turns
out to be XML, the XML turns out to be FpML, the FpML turns out to be a
swap, the swap's legs turn out to be fixed-against-floating. A parser
for a programming language does exactly this over a grammar fixed in
advance; here the grammar is not fixed, it is the sum of every pattern
anyone has registered, and it grows without touching what was there.

The operator's own account of why this failed twice before, in Java
and in Scala, at a bank: cognitive load — the patterns were written
into the code that used them, so no pattern could be read, tested or
replaced alone. The design here is the one that was believed right
then: **the patterns are the components.** A pattern is a value; the
hierarchy is their composition; the document model is what the
composition produces, not something designed beside it.

Three pieces of the library already carry this, and this module only
names their meeting:

- a step that may DECLINE is `pattern-binds` (specs/pattern-binds.md):
  under `Choose` a refused pattern kills its branch and the search goes
  on; the alternatives are `Logic`'s (specs/backtracking.md), soft cut
  included — "if this is a swap then … else …" without losing the
  other readings;
- a pattern that reads and writes back is a PRISM (specs/optics.md):
  `preview` is the partial recogniser, `review` the total constructor,
  and composition along a path reads *and* writes, so a document read
  through one branch and written through another is a conversion, for
  free and lawful;
- what a level PRODUCES is a `Schema`-typed value (specs/codecs.md):
  the reified type is data, so a new instrument is a new `Schema` and
  a new pattern, registered, and nothing existing is edited.

What the module adds is the vocabulary that holds them together — one
type, one result shape, one law — and the first level, so the shape is
proved on real material before the domain arrives.

## 2. Interface

```scala
package okay.refine

/** a pattern: from what is known (A) to what is learnt (B), or a
 *  refusal that says why; and the way back */
sealed trait Refine[A, B]:
  def name: String
  def run(a: A): Verdict[B]                  // read: may decline, says why
  def write(b: B): Either[String, A]         // total on B's image
  def andThen[C](next: Refine[B, C]): Refine[A, C]   // a path
  def <|>(alt: Refine[A, B]): Refine[A, B]           // a choice
  def map[C](name: String)(to: B => C, from: C => B): Refine[A, C]  // an iso
  def widen[C >: B](using ClassTag[B]): Refine[A, C] // into a sum

object Refine:
  def step[A, B](name: String)(read: A => Either[String, B])(write: B => A): Refine[A, B]
  def first[A, B](alts: Refine[A, B]*): Refine[A, B]   // the <|> of many

/** what a run says — never "the first won" silently */
enum Verdict[+B]:
  case Took(value: B, by: Path, declined: Vector[Refusal])
  case Unclear(candidates: Vector[(Path, B)], declined: Vector[Refusal])
  case Declined(tried: Vector[Refusal])

final case class Path(steps: Vector[String])          // the names taken
final case class Refusal(at: Path, reason: String)    // a name and why
```

A `Verdict` is the `Route`/`Support` of specs/dlm.md at the level of
documents: the value, the path of names that produced it, every
alternative that declined and its reason, and `Unclear` when more than
one branch took the input. `Declined` and `Unclear` are values, not
faults — a risk system that cannot tell what it is looking at must say
so and say what it tried.

`Refine.step` is a prism given as its two halves; `Refine.step(...).prism`
hands back the optics `Prism[A, A, B, B]` so the laws of specs/optics.md
apply unchanged. A tree of steps stays a tree (`Step`, `AndThen`, `Or`,
`Map`), so a run walks it and records the path; `<|>` runs EVERY
alternative — that is how `Unclear` can exist — and a level that wants
the first answer only says so with `Logic.cut` at stage 2.

### The first level: format

```scala
enum Doc:
  case Json(tree: Cst[okay.lex.Json.K])
  case Xml(tree: Cst[okay.codec.Xml.K])
  case Yaml(tree: Cst[okay.codec.Yaml.K])
  case Cbor(bytes: Array[Byte])

object Format:
  val text:   Refine[Array[Byte], String]       // valid UTF-8, or why not
  val json:   Refine[String, Doc.Json]
  val xml:    Refine[String, Doc.Xml]
  val yaml:   Refine[String, Doc.Yaml]
  val cbor:   Refine[Array[Byte], Doc.Cbor]
  val detect: Refine[Array[Byte], Doc] = cbor.widen <|> (text andThen (json.widen <|> xml.widen <|> yaml.widen))
```

Every text format is decided over the dialect's OWN lossless CST
(specs/codecs.md), not a second grammar: a format takes the input when
its tree has no error node and has structure (a JSON object or array, an
XML element, a YAML mapping or sequence) — a bare scalar is not a
document of any of them and is declined by all three, which is a
`Declined` verdict naming three reasons. CBOR takes bytes that are
exactly one well-formed item. `write` is the dialect's `render` (the
lossless law made a function), so `write(read(bytes)) == bytes`.

## 3. Behavior

Stage 1 — the vocabulary and the format level:
- [x] a step that reads takes: `Took(value, Path(name), Vector.empty)`
- [x] a step that refuses declines with its reason under its name
- [x] `andThen`: the path is both names; a refusal in the second step is
      reported under the composed path
- [x] `<|>`: exactly one taker is `Took` with the others in `declined`;
      two takers is `Unclear` naming both paths; none is `Declined`
      naming every reason
- [x] `write` follows the path back: `step.write ∘ read` is the input
      where the step is lossless; `Or.write` asks the alternatives in
      order and takes the first that accepts
- [x] the prism laws hold for a step's `.prism` (review-then-preview is
      identity; preview-then-review is identity where it previews)
- [x] `Format.detect` on a JSON, an XML, a YAML and a CBOR document
      answers `Took` with the right case and the path `text/json`,
      `text/xml`, `text/yaml`, `cbor`, and `write` reproduces the bytes
      (TestFormat; the XML one without a declaration — see the gap below)
- [x] a bare scalar (`hello`) is `Declined` with three reasons; a
      damaged JSON (`{"a":`) is declined by json with the tree's own
      error message, not a generic one — and so is `hello` itself
      ("unexpected 'hello' at Span(0,0,0,5)"); a bare `42`, a valid
      JSON value, is "not a JSON object or array"
- [x] bytes that are not UTF-8 decline `text` with the offset, and the
      verdict shows `cbor` was tried too
- [x] a document two formats both take is `Unclear` and names both —
      held on the vocabulary (TestRefine: 4 is `even` and `small`); NOT
      observable at the format level today, see Results: the YAML
      dialect is block-only, so `{"a": 1}` is json alone here
- [x] `<?xml version="1.0"?><a/>` is `text/xml` and writes back — a
      KNOWN GAP pinned by TestFormat for a day: the dialect had no
      processing instruction (xml-processing-instruction, 2026-09-29,
      `K.Pi` and `K.Decl`, one token each, no frame; okay2 in step)

Stage 2 — the document level and the open registry:
- [ ] `Refine.schema[A](using Schema[A]): Refine[Json, A]` — a derived
      Schema is a pattern (decode declines with the codec's message)
- [ ] a registry: `Refine[A, B]` values registered by name under a
      level, `Or`-ed in registration order, so a new format or product is
      one registration and no edit
- [ ] `Judge` seam: an ORDERING of the alternatives (ours: registration
      order; DLM-shaped: by a judge's ranking), which never adds a taker
      and never removes one — only which is tried first under `cut`
- [ ] `Logic` integration: a `Refine` as a program under `Choose`, so a
      caller writes `for case Took(swap, _, _) <- …` and `ifte`

Stage 3 — lessons:
- [ ] "this file is a swap" from a person is a journal record; the fold
      of the journal is an ordering; structure never changes

## 4. Decisions

- **The domain is not here.** FpML and ISDA CDM patterns — the ones that
  say what a swap is — are the product and live in a private repository
  that depends on okay, the way okay-watch does. This module ships
  mechanism and one public prover (the format level now; a two-document
  FpML prover — an IRS and an FX forward from ISDA's public examples —
  when stage 2 lands), enough to show `xml → fpml → swap` end to end.
- **Every alternative runs.** A `<|>` that stopped at the first taker
  could never say `Unclear`, and for a risk system a silent first-wins
  is the defect this module exists to remove. The cost is a run per
  alternative at every level; the format level pays four parses of one
  input, which is nothing beside a wrong answer. `cut` is a choice a
  caller makes, in stage 2, by name.
- **A format is decided by its own parser's tree, not by a sniff.** A
  leading `{` is not JSON (`{a: 1}` is not), a leading `<` is not XML,
  and the lossless CST already knows: it has an error node or it does
  not, it has structure or it does not. One grammar per dialect, the
  one the codec uses; a detector that disagreed with the parser would
  be a second grammar and would drift.
- **A bare scalar is no document.** `hello` is a YAML scalar and a
  YAML document by the YAML spec; here it is declined by yaml too,
  because a level whose answer is "a string" has learnt nothing about
  what to ask next. A caller that wants scalars writes the step.
- **`write` is partial on a sum.** A prism's review is total, and a
  step's is; an `Or` of steps into a sum (`Doc`) cannot know which
  alternative a `Doc.Xml` belongs to without asking, so `Or.write` asks
  in order and `Refine.write` is `Either`. The law that holds is the
  one that matters: `write(b)` for a `b` some branch produced is that
  branch's write. `.prism` is offered on a step, where it is lawful,
  not on the tree.
- **`Verdict`, not `Either`.** The refusal carries every path tried and
  its reason because that is the whole value of a hierarchy over a
  hand-written `match`: the reader sees what was considered.
  specs/dlm.md's `Support` made the same call for utterances.

## 5. Results

Stage 1 (2026-09-29, lane okay-refine), found by the first run of
TestFormat — three things the dialects said that a sniff would not have:

- **YAML claimed every JSON object.** okay-codec's YAML is the block
  dialect (specs/codecs.md: flow style out of scope); it reads
  `{"a": [1, 2]}` as a scalar `{` followed by a mapping of pairs, with
  NO error node, so the structural test alone ("has a map") let it take
  the input and `Format.detect` answered `Unclear(json, yaml)` on plain
  JSON. The tell is a scalar at the ROOT beside the structure — a block
  document has none — and `Format.yaml` declines on it. Recorded rather
  than hidden because it will reverse: when the dialect learns flow
  style, `{"a": 1}` is a YAML mapping and a JSON object and the honest
  verdict is `Unclear`, which is exactly what the design says.
- **The XML declaration is an unclosed tag.** `Xml.kindOf` knows
  comments, CDATA, close and self-close and then calls every other `<`
  an Open, so `<?xml version="1.0"?>` opens a frame nobody closes and
  every real FpML document is declined with `unclosed`. Filed as
  backlog `xml-processing-instruction` and landed the same day: `K.Pi`
  (`<?…?>`, its own scanner mode, since a PI may hold quotes and `>`)
  and `K.Decl` (`<!DOCTYPE …>`), one token each, never a frame; the
  streaming tokenizer in step with the scanner (the random-input oracle
  caught the one divergence, `<?>`: the closing `?` must not be the
  opening one). TestFormat's pinned test flipped to `Took(text/xml)`.
- **The parser's own words are better than ours.** `hello` is declined
  by json as "unexpected 'hello' at Span(0,0,0,5)" — the tree's error
  leaf — where the first test expected a generic "not a JSON object or
  array"; that phrase is now what a bare NUMBER gets, since `42` is a
  valid JSON value and the tree has no error to quote. The test was
  wrong, the design was right, and the test now says both.

Sizes: `Refine.scala` 130 lines, `Format.scala` 150 (UTF-8 validator
included, by hand so JS and Native run the same check), 18 tests JVM.

## 6. Open questions

- Corpus for stage 2: ISDA's public FpML examples (IRS, FX forward)
  and CDM's JSON samples — which versions to pin (FpML 5.12 confirmation
  view; CDM 5.x) is decided when the private repository is opened.
- Ambiguity policy above the format level: `Unclear` is the answer at
  stage 1; whether a `Judge` may resolve it, or only order it, is
  stage 2's decision — the design holds that it orders only.
