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

  /** what a caller acts on: act on `High`/`Medium`, ask on `Low` */
  enum Decision[I]:
    case Act(spans: List[(String, I)])
    case Clarify(span: Span[I])
    case Nothing
  def decide[I](r: Reading[I], floor: Conf = Conf.Medium): Decision[I]

object Eval:
  /** per-class counts; a Monoid, so evaluation distributes and partial
   * runs merge — the same property Postings has */
  final case class Confusion(cells: Map[(String, String), Int])
  final case class ClassScore(precision: Double, recall: Double, f1: Double)
  final case class Report(perClass: Map[String, ClassScore], macroF1: Double)

  /** one streaming pass over labelled examples */
  def confusion[I]: Aggregator[(String, String), Confusion, Report]

  /** the promotion rule, EXECUTABLE: the classes that regressed by
   * more than `tolerance` F1 points. Empty means promotable. */
  def regressions(baseline: Report, candidate: Report,
                  tolerance: Double = 0.02): List[String]
```

## Behavior

- [ ] a sum type derives a schema whose cases are the taxonomy, nested
      sums nesting as groups
- [ ] a required frame slot is a non-`Option` field with no default;
      an optional slot is `Option`
- [ ] a reply naming a label outside the taxonomy decodes to a `Left`,
      naming the offending label
- [ ] a reply whose slot fails its own schema (a `When` that is not
      ISO-8601) decodes to a `Left`
- [ ] two intents in one message produce two spans
- [ ] one uncertain intent produces one span with several alts, ranked
- [ ] `decide` returns `Clarify` when the best alt is `Low`, and the
      clarification carries the alternatives to choose between
- [ ] `Confusion` is a lawful Monoid (associative, identity)
- [ ] `Report` gives per-class precision/recall/F1 and macro F1
- [ ] `regressions` is empty for an identical report and names exactly
      the classes that fell more than the tolerance
- [ ] the taxonomy section of the prompt is generated from the schema:
      adding a case changes it without an edit

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

**Evaluation is a fold.** `Confusion` is a Monoid, so evaluation
distributes and partial runs merge — the property `Postings` has for
the same reason. The fixture is a `Rerun` journal, so a prompt change
replays deterministically and a regression points at a step number
instead of a number that fell.

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
