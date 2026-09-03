# Intent classification

## Overview

Routing a message to an action needs two things a bare label cannot
give: WHICH intent, and the SLOTS that intent needs to be acted on.
"Proposal" does not let you answer an email; `Proposal(when, who,
where)` does.

Both fall out of one `Schema[I]` derivation. A hierarchical sum type
IS the taxonomy, IS the frame (FrameNet's Frame Elements are the
product's required fields — the same `required` the tool-declaration
algebra already computes from "not `Option`, no default"), and IS the
parser of the model's reply. So the enumeration the model is shown and
the decoder that reads its answer are the same value, and a label
outside the enumeration is a decode error rather than a class of
parsing bug to defend against.

Two axes are kept apart, because conflating them is the standard
mistake. MULTI-INTENT ("the card was charged twice and the app
crashes") is one message carrying two intents that both need acting
on; it falls out of per-span detection. AMBIGUITY is one span whose
intent is uncertain; it is ranked alternatives within that span. A
single flat list cannot express both, and a caller that receives one
cannot tell which situation it is in.

## Interface

```scala
package okay.agent

/** categorical, not numeric: a model has no calibrated probability to
 * report, so asking for one invents precision. A NUMERIC margin
 * appears only with the vector tier, and is a different quantity with
 * a different threshold — never the same field. */
enum Conf: case Low, Medium, High

/** one candidate reading of a span */
final case class Alt[I](intent: I, conf: Conf)

/** one stretch of the message carrying one intent. `why` precedes the
 * alternatives BOTH in this declaration and on the wire — see
 * Decisions, it is worth 0.14 macro F1. */
final case class Span[I](text: String, why: String, alts: List[Alt[I]])

/** what the model returns: the message, segmented */
final case class Reading[I](spans: List[Span[I]])

object Classify:
  /** the schema of a reading over taxonomy `I` — derived, so adding a
   * case to `I` changes the prompt, the parser and the tool
   * declaration together or not at all */
  given reading[I](using Schema[I]): Schema[Reading[I]]

  /** the prompt's taxonomy section, rendered FROM the schema (never
   * hand-written beside it) */
  def taxonomy[I](using Schema[I]): String

  /** the full instruction for a message */
  def prompt[I](message: String)(using Schema[I]): String

  /** decode a reply; a label outside `I` is a Left, not a silent Other */
  def read[I](reply: String)(using Schema[Reading[I]]): Either[String, Reading[I]]

  /** the class label of a value: the case name, or for a nested
   * taxonomy the path through its groups. `depth = 1` scores the
   * matrix over GROUPS, full depth over leaves. */
  def label[I](i: I, depth: Int = Int.MaxValue)(using Schema[I]): String

  /** the SHAPE of an answer, rendered from the schema: optional
   * fields omitted, one list element, a sum tagged by its first case.
   * A shape, not a valid value — a placeholder cannot satisfy a
   * refined leaf. */
  def example[A](using Schema[A]): String

  /** the full instruction; `examples` are shown as message -> intent */
  def prompt[I](message: String, examples: List[(String, I)] = Nil)(using Schema[I]): String

  /** the in-domain gate: one binary question asked BEFORE the taxonomy */
  final case class InDomain(why: String, inDomain: Boolean)
  def inDomainPrompt[I](message: String)(using Schema[I]): String
  def readInDomain(reply: String): Either[String, InDomain]

  /** what a caller acts on: act on `High`/`Medium`, ask on `Low` */
  enum Decision[+I]:
    case Act(spans: List[(String, I)])
    case Clarify[I](span: Span[I]) extends Decision[I]
    case Empty
  def decide[I](r: Reading[I], floor: Conf = Conf.Medium): Decision[I]

object Eval:
  /** per-class counts; a Monoid, so evaluation distributes and partial
   * runs merge — the same property Postings has */
  final case class Confusion(cells: Map[(String, String), Int])
  final case class ClassScore(precision: Double, recall: Double, f1: Double)
  final case class Report(perClass: Map[String, ClassScore], macroF1: Double)

  /** one streaming pass over (gold, predicted) label pairs */
  val confusion: Aggregator[(String, String), Confusion, Report]

  /** the scores a matrix implies, without the fold */
  def report(m: Confusion): Report

  /** the promotion rule, EXECUTABLE: the classes that regressed by
   * more than `tolerance` F1 points. Empty means promotable. */
  def regressions(baseline: Report, candidate: Report,
                  tolerance: Double = 0.02): List[String]
```

## Behavior

- [x] a sum type derives a schema whose cases are the taxonomy, nested
      sums nesting as groups
- [x] a required frame slot is a non-`Option` field with no default;
      an optional slot is `Option`
- [x] a reply naming a label outside the taxonomy decodes to a `Left`,
      naming the offending label
- [x] a reply whose slot fails its own schema (a `When` that is not
      ISO-8601) decodes to a `Left`
- [x] two intents in one message produce two spans
- [x] one uncertain intent produces one span with several alts, ranked
- [x] `decide` returns `Clarify` when the best alt is `Low`, and the
      clarification carries the alternatives to choose between
- [x] `Confusion` is a lawful Monoid (associative, identity)
- [x] `Report` gives per-class precision/recall/F1 and macro F1
- [x] `regressions` is empty for an identical report and names exactly
      the classes that fell more than the tolerance
- [x] the taxonomy section of the prompt is generated from the schema:
      adding a case changes it without an edit
- [x] the example answer is rendered from the schema: a sum appears
      TAGGED, an optional field is omitted, a list shows one element
- [x] the prompt carries that example
- [x] the gate asks with an example object rather than with a schema
- [x] a gate verdict decodes, and a malformed one is a `Left`

## Out of scope

- The symbolic tier (LU dictionary over `Postings`/BM25) and the vector
  tier (class centroid, then a linear probe over frozen embeddings).
  Both are deferred behind a measured trigger, not forgotten — see
  Decisions.
- A fine-tuned encoder. Refused, see Decisions.
- Answer generation from templates attached to the intent.
- Natural-language temporal parsing ("jeudi prochain"). A slot takes
  ISO-8601 and validates through `SIso`; a Duckling-equivalent is a
  separate lane.

## Design

**One derivation, three uses.** `Schema.derived` recurses through
`thunks[MirroredElemTypes]` (`Schema.scala:123`), so a case that is
itself an enum becomes a nested `SSum` with no extra code. That is the
hierarchy the literature keeps arriving at independently — Linagora's
18 intents in 3 groups, and the two-level classifier of the agentic
article — and it buys something a flat enum cannot: an error WITHIN a
group and an error ACROSS groups are different costs, and only a
nested taxonomy can separate them in the matrix or weigh them
differently in the promotion rule.

`ToolSpec.jsonSchema` (`ToolSpec.scala:60`) already renders `SSum` as
`oneOf` tagged by case name, "the same encoding Json and Cbor use, so
decode round-trips". The prompt's taxonomy section is that value
rendered, so it cannot drift from the parser.

**One model tier.** `Structured.cut` validates the reply as it arrives
and stops generation the moment the value is complete; for a
classification the value is small, so the answer costs the answer.
NOT YET WIRED HERE: this lane builds the prompt and the decoder, and
leaves the streaming call to the caller — so the token saving is
reasoned about, not measured (backlog: intent-live-provider).

**Evaluation is a fold.** `Confusion` is a Monoid, so evaluation
distributes and partial runs merge — the property `Postings` has for
the same reason, and the property test here checks exactly that: two
partial runs merged give the same report as folding the whole.

The INTENDED fixture is a `Rerun` journal, so that a prompt change
replays deterministically and a regression points at a step number
rather than at a number that fell. That binding is not built in this
lane — `Eval` takes `(gold, predicted)` label pairs from wherever the
caller has them, and nothing here depends on where. Stated as the
intended path, not as something that exists.

## Decisions

- **`why` before the label** — chosen because it is worth 0.14 macro
  F1, measured (Results). Rejected: label first (0.479 vs 0.615),
  which `Structured.cut` would make ~130 characters cheaper per
  classification. Quality wins at this price; the cheap arm stays
  available for a caller that has measured its own trade.
- **Field order is load-bearing, and it holds** — the declaration
  order of an `SProduct` reaches the wire: 48/48 replies emitted the
  fields in the declared order across both arms. This was the lane's
  first step precisely because the decision above rests on it.
- **Two axes, two mechanisms** — spans for multi-intent, alts for
  ambiguity. Rejected: one ranked list (cannot distinguish "act on
  both" from "choose one").
- **Categorical confidence from the model, numeric margin from
  vectors** — kept as separate fields with separate thresholds.
  Rejected: one `confidence: Double` (invents calibration the model
  does not have).
- **`Other` is a case of the taxonomy, not a convention** — so it
  cannot be forgotten. But see Results: declaring it is NOT enough,
  and this is the lane's most useful negative finding.
- **A group is a one-field case whose field is a taxonomy** — that is
  how the walk knows to descend. Scala's enums encode a hierarchy as a
  case wrapping the sub-enum (`case Proposal(p: ProposalKind)`), so
  the group node is a product, not a sum, and a walk that only
  descended sums stopped at "Proposal" — found by the first run of the
  test, not by reading. A case whose single field is a plain value is
  a LEAF: its fields are slots, not a sub-taxonomy. Both kernels the
  walk uses (`theCase`, `eachField`) hand the value over at its own
  type, so the whole walk takes no cast.
- **Show an EXAMPLE, not only a schema** — measured twice in this
  lane, in opposite corners of it. A schema for the two-field gate
  answer came back as the schema itself, the verdict buried in
  `properties`. And shown only the reading's schema, the model wrote
  `"intent": "Proposal"` as a bare name where the encoding wants
  `{"Proposal": {...}}`, dropped `alts`, and merged `conf` into the
  intent object: 20 of 24 replies undecodable. A rendered example is
  derived from the schema, so it cannot drift from it.
- **The example is a SHAPE and says so** — its leaf placeholders cannot
  satisfy a refined schema (`"..."` is neither a confidence nor an
  ISO-8601 date), and nothing generic can invent a value that passes an
  arbitrary `SIso`. Rejected: pretending otherwise (the first version of
  the test asserted the example decodes, and it does not).
- **A sentinel is not a class** — the harness first fed its own
  `undecodable` marker into the confusion matrix, where it became a
  predicted-only class with F1 0 and dragged macro F1 with the DECODE
  rate rather than the classification. Two runs with identical per-class
  scores reported 0.916 and 0.748 because they differed by one
  undecodable reply. `Eval`'s rule that an invented label is still a
  class is right for a real label and wrong for a marker one made up;
  the fix is at the call site, and macro F1 is now reported over decoded
  replies WITH the decode rate beside it. Neither number means anything
  alone: the `bare` arm scores 0.733 on the four replies it managed to
  produce.
- **One tier before three** — the symbolic and vector tiers are
  deferred until measurement shows cost or latency binding. Rejected:
  building all three now (three dictionaries to keep in sync, three
  ways to be wrong, none of it yet justified by a number).
- **No fine-tuned encoder** — rejected on three independent grounds.
  The cost is the LABELS (1k-5k per class per the reference, so 18-90k
  examples for an 18-class taxonomy), not the compute. Serving one
  needs ONNX Runtime or DJL, a JVM-only native library, inside a
  library that cross-builds to JS. And the conditions under which the
  encoder wins are ">50 qps and a stable taxonomy" — the taxonomy is
  the thing that will change most. The replacement, when the fast tier
  is finally justified, is a linear probe over frozen embeddings:
  18x1024 weights is 72KB, inference is one matvec (a cosine at 1536
  components measured 1.04us in `Store.scala`, so ~18us for 18
  classes), it trains from LLM-distilled labels at 30-100 examples per
  class rather than thousands, and it needs no dependency at all.

## Results

First measurement of the lane, before any code: the local gateway
(`mlx-community:Qwen3.5-4B-MLX-4bit`, temperature 0), 24 labelled
messages over 4 classes, both field orders, JSON schema in the prompt
(no constrained decoding).

| declared order | order honoured | accuracy | macro F1 | median chars before the label |
|---|---|---|---|---|
| `why`, `intent` | 24/24 | 66.7% | **0.615** | 133 |
| `intent`, `why` | 24/24 | 58.3% | 0.479 | 1 |

Reasoning first is worth 0.136 macro F1 for ~130 characters. The
reference's claim ("dramatically improves recall on edge cases")
reproduces, and the first email in the set is the example: "Je vous
propose de faire une reunion jeudi prochain" reads as `Proposal` with
the reasoning first and as `Request` without it.

**The negative finding, which is the useful one.** `Other` collapses
in both arms — recall 0.17 with reasoning, 0.00 without. Every
`Other` message was absorbed into a positive class (charged twice ->
`Request`, birthday wishes -> `Notification`). So the advice to
"always include an `other` bucket" is necessary and NOT sufficient: a
model asked to choose among positive classes will choose one. The
matrix is what showed it; macro F1 alone reads as a mediocre score
rather than as one class being entirely absent. Treatment is a lane
item, not a prompt tweak — the candidates are an explicit
none-of-the-above instruction, `Other` examples in the prompt, and a
separate binary in-domain gate ahead of the taxonomy.

`Proposal` -> `Request` is the other confusion (3 of 6, reasoning
arm), and it is genuine overlap rather than model error: "Can we move
Thursday's sync to Friday?" is both a proposal and a request. That is
the "mutually exclusive in practice" problem, and it needs a stated
precedence rule in the taxonomy's own documentation, not a better
classifier.

Scope of these numbers, stated so nobody quotes them further than they
go: one 4B local model, n=24, no constrained decoding. They decide the
FIELD ORDER and they expose the `Other` collapse. They are not a
quality claim for the design.

**Implementation.** `Classify.scala` and `Eval.scala` in `okay-agent`,
24 tests (`TestClassify` 13, `TestEval` 11, three of them ScalaCheck
properties for the Monoid laws and for partial runs merging to the
same report as the whole). No casts: the taxonomy walk goes through
`Schema`'s existing `theCase` and `eachField` kernels.

One behaviour worth recording because the test's first expectation was
wrong about it: a single confusion damages BOTH classes it involves —
B called A costs B its recall and A its precision. A promotion rule
that named only the missed class would let half the damage through.

## Results — intent-other-collapse (2026-09-03)

The collapse was chased down in the repository rather than in a script
beside it: `TestClassifyLive` (Live-tagged, out of the default gate)
runs six arms over `IntentFixture`'s 24 messages, and `IntentFixture`
is shared so the next lane compares against the same baseline instead
of inventing one.

Same local 4B gateway, temperature as the server defaults it. Macro F1
is over DECODED replies, so it must be read together with the decode
rate — the two columns are one measurement.

| arm | decoded | macro F1 | `Other` recall |
|---|---|---|---|
| bare — the schema alone | 4/24 | 0.733 | 0.00 |
| rules — schema + written rules | 18/24 | 0.587 | 0.00 |
| shipped — rules + rendered example shape | 21/24 | 0.681 | 0.17 |
| examples — shipped + 5 labelled examples | 23/24 | 0.908 | 0.67 |
| gate — in-domain question, then shipped | 21/24 | 0.826 | 0.50 |
| **examples + gate** | **23/24** | **0.955** | **0.83** |

Read as three separate findings.

**The decode rate is a prompt property, not a model property.** 4 -> 18
-> 21 -> 23 of 24 replies decoded, from the same model, purely on how
the answer was asked for. The rendered example is the mechanical fix;
the written rules do most of the rest.

**Few-shot examples are the quality lever** (0.681 -> 0.908), and they
are drawn from OUTSIDE the fixture, so no arm is scored on its own
teaching material.

**The gate is what actually addresses the collapse.** Recall for
`Other` went 0.00 -> 0.17 with the example, 0.67 with examples, and
0.83 with the gate on top, at precision 1.00 throughout. A binary
in-domain question does not offer the model a choice among positive
classes, which is the thing it was losing to. It costs one extra call
per message — the price is visible and the caller chooses.

So the answer to the original finding stands and is now sharpened:
declaring an `Other` case is necessary and not sufficient; what
rescues it is not asking the taxonomy question at all until a separate
question has said the message belongs.

**What is still wrong.** One of six out-of-domain messages is still
absorbed even by the best arm, and the residue is not random: the
fixture's `Other` mixes "not about this at all" (a birthday wish, a
recipe) with "about a different topic in the same register" (a double
charge, a cancellation), and a taxonomy of `Proposal`/`Request`/
`Notification` carrying a bare `what: String` does not tell the model
its domain is meetings. The case NAMES carry the domain or nothing
does. Filed.

And a caution about this table: at n=24 a difference of one or two
replies is not a difference. The wording of the example line was
changed mid-lane and moved `shipped` by two replies — noise, reported
as noise. The fixture needs to grow before any of these gaps is
defended as real.

## Results — intent-fixture-too-small (2026-09-03)

The fixture grew from 24 to 120 messages, thirty per class, with the
domain stated inside it ("meeting and scheduling intents") because the
previous lane established that nothing else states it. Hard cases are
marked rather than avoided: Proposal/Request overlap, indirect
phrasing, cancellation without a proposal, and — where the bucket
actually broke — out-of-domain messages written in the register of a
request.

**The 24-message conclusion holds at 120.**

| arm | decoded | macro F1 | `Other` P / R / F1 |
|---|---|---|---|
| rules (before) | 82/120 | 0.553 | 0.00 / 0.00 / 0.00 |
| examples + gate (after) | 109/120 | 0.906 | 0.92 / 0.81 / 0.86 |

That matters more than the numbers themselves: a five-fold larger
fixture reproduced both effects, so the earlier table was measuring
something real and not the shape of twenty-four sentences. `Other` goes
from never predicted to F1 0.86, and the decode rate again moves with
the prompt (68% -> 91%).

**Language is not free**, and this is the lane's new finding. Twelve
meanings, six languages, one arm (examples + gate), so the only thing
varying between rows is the wording:

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| macro F1 | 0.914 | 0.804 | 0.792 | 0.813 | 0.727 | 0.813 |
| `Other` precision | 1.00 | 0.75 | 1.00 | 1.00 | 0.60 | 1.00 |
| `Other` recall | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 |
| undecodable | 0/12 | 1/12 | 1/12 | 0/12 | 1/12 | 0/12 |

Every language keeps `Other` RECALL at 1.00 — the gate does not stop
recognising out-of-domain messages when they stop being English. What
it loses is PRECISION: in Russian (0.60) and French (0.75) the gate
pushed genuine meeting messages OUT of the domain, and that is where
the macro F1 gap comes from. So the failure has a direction, and it is
the opposite of the English failure: English absorbed out-of-domain
messages into positive classes; non-English rejects in-domain ones.

Scope, stated so the table is not over-read: twelve messages per
language, one 4B local model, and translations written by the same hand
as the classifier — an awkward rendering is a confound these numbers
cannot separate from a model weakness. What the table supports is
"there is a language effect and it lands on gate precision", not its
size.

**Honest limitation of the whole fixture**: 120 author-written messages
are enough for stable per-class metrics and not enough to claim
coverage. They show that a change moves the needle on cases someone
thought of.
