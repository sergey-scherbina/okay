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
and stops the walk the moment the value is complete. What that is worth
here was measured in the intent-live-provider lane, and the answer is
ZERO — see its Results. The sentence this paragraph used to carry
("so the answer costs the answer") was unearned and is gone.

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
varying between rows is the wording. SUPERSEDED IN PART — this table was
taken with generic names and the gate, both of which later measurements
demoted; read it with the intent-gate-non-english Results below, which
re-take it on what is actually recommended:

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

## Results — intent-domain-in-names (2026-09-03)

The hypothesis the previous two lanes left standing: a taxonomy carries
its domain in its case NAMES or nowhere. `Proposal`/`Request`/
`Notification` with a bare `what: String` never mentions meetings, so
"please refund my card" reads as a `Request` honestly rather than
mistakenly, and every prompt-level fix for that is arguing with a type
that did not state its subject.

Four configurations, the same 120 messages, the same examples, the same
prompt. The only thing that changes is the TYPE.

| configuration | calls / message | macro F1 | `Other` P / R / F1 |
|---|---|---|---|
| generic names, no gate | 1 | 0.872 | 0.94 / 0.65 / 0.77 |
| generic names + gate | 2 | 0.906 | 0.92 / 0.81 / 0.86 |
| **domain names, no gate** | **1** | **0.907** | 0.87 / 0.96 / **0.92** |
| domain names + gate | 2 | 0.830 | 0.68 / 0.97 / 0.80 |

**The names do the gate's work, for free.** `MeetingProposal` /
`MeetingRequest` / `MeetingNotification` / `NotAboutMeetings` matched
the gated configuration's macro F1 and beat its `Other` F1, at half the
model calls. `Other` recall went 0.65 -> 0.96 with nothing changed but
the four identifiers.

**And they do not compose.** Gating an already-named taxonomy is WORSE
than either half alone (0.830): `Other` precision falls to 0.68 and
`Notification` recall to 0.68, because a second judge re-rejects what
the first accepted. Two mechanisms for one job is not twice the
safety.

So the gate is demoted from "the answer" to "the fallback", and that
now says so in its own doc comment: name the domain in the type; use
the gate when the taxonomy cannot be renamed — someone else's types, a
wire format, a taxonomy shared with a system that owns its names.

**The price of clear names, stated because it is real.** `Other`
precision falls 0.94 -> 0.87 and `Request` recall 0.92 -> 0.77:
domain-bearing names make the model readier to push a borderline
message out of the domain. Which error is cheaper is the caller's
call — a misrouted request costs a wrong action, a wrongly rejected one
costs a human's attention — and this is the trade to state in a
taxonomy's documentation rather than to settle by default.

This is the strongest form of the claim the whole feature rests on: the
taxonomy IS the type, so the type's names are not labels for humans,
they are half the classifier.

## Results — intent-name-sensitivity (2026-09-03)

The previous lane's recommendation rested on four identifiers, so this
ablates them. Four taxonomies differing ONLY in case names, with no
examples and no gate in any arm — examples would teach what the names
are supposed to say on their own, and a gate would add a second signal.

| taxonomy | macro F1 | `Other` P / R / F1 | undecodable |
|---|---|---|---|
| generic (`Proposal`...) | 0.649 | 0.83 / 0.19 / 0.30 | 10/120 |
| true domain (`Meeting`...) | 0.688 | 0.92 / 0.43 / 0.59 | 7/120 |
| wrong domain (`Shipping`...) | 0.635 | 0.72 / 0.45 / 0.55 | 2/120 |
| nonsense (`Zarnic`...) | 0.528 | 1.00 / 0.11 / 0.20 | 13/120 |

**The control did its job: the effect is not "names that look
chosen".** `Zarnic` is the WORST arm — macro F1 0.528 against generic's
0.649, `Other` recall 0.11 against 0.19, and the highest undecodable
count of the four. An uninterpretable qualifier does not merely fail to
help, it actively costs. So the previous lane's recommendation survives
the test that could have hollowed it out.

**The domain word is READ, not decorated with.** The wrong-domain arm
is the proof, and it is proof by damage: `Shipping` raises `Other`
recall to 0.45 — as high as the true domain's 0.43 — while `Proposal`
recall halves, 0.85 -> 0.45. Meeting messages are being pushed into
`NotAboutShipping`, which is the correct reading of a taxonomy that
says its subject is shipping. The model is answering the question the
names ask.

**And `Other` precision is what separates a right domain from a wrong
one**: 0.92 for `Meeting` against 0.72 for `Shipping`. Both reject at a
similar rate; only one rejects the right things. A recall column alone
would have called these two arms equivalent, which they are not.
(`Zarnic`'s 1.00 precision is on a recall of 0.11 — it is precision
over almost nothing, and reading it as a win is the trap this table
exists to avoid.)

**Scale, against the arms with examples.** Names alone move `Other`
recall 0.19 -> 0.43; names plus examples reached 0.96 in the previous
lane. So the naming is real and partial: it buys roughly a quarter of
the distance, and few-shot examples remain the larger lever. Nothing in
the shipped recommendation changes — it was measured WITH examples on
both sides — but the mechanism is now known rather than assumed.

Same scope as before: 120 author-written messages, one 4B local model,
one run per arm.

## Results — intent-live-provider (2026-09-03)

This lane exists to pay a debt: three lanes shipped while the spec said
`Structured.cut` makes a classification "cost the answer" and admitted
in the same breath that the saving was reasoned about rather than
measured. Now it is measured, and the claim does not survive.

**Against a real model, through the real streaming transport, the early
stop saves nothing — 0.0% in both regimes, for opposite reasons.**

| prompt | tokens with cut | tokens generated | saved |
|---|---|---|---|
| strict ("ONE JSON object and nothing else") | 250 | 250 | 0.0% |
| prose-inviting | 643 | 643 | 0.0% |

Under the strict prompt the accumulated text at the stop is exactly the
whole reply — 280 chars against 280, 291 against 291, message after
message. The model emits the closing brace and stops on its own, so
there is nothing after it to avoid. Under a prose-inviting prompt the
value never decodes at all (`decoded=false`), so the walk runs to the
end — the safe direction `Structured` documents, and again no saving.

**The mechanism is not broken; there is simply nothing for it to do
here.** That distinction is not an inference: `TestCutStops` runs the
walk over a synthetic stream that COUNTS how far it was pulled, in the
default gate with no model at all. A value followed by five hundred
pieces of commentary stops after the value and leaves the source
un-pulled; a stream that never completes is drained in full. Both are
asserted, not observed.

So the honest statement is: **`cut` is insurance against a model that
keeps talking after a complete value, not a saving in the normal case.**
A classification prompt that says "and nothing else" already buys what
`cut` would have bought, and buys it from the model rather than from
the client. Where `cut` still earns its place is a model or a setting
you do not control — an endpoint that appends a summary, a chat model
without a strict-output mode, a provider that ignores the instruction.

This also settles a question left open by the field-order decision.
That trade was priced in CHARACTERS of prose (~130 for reasoning
first), with a note that `cut` made the cheap arm cheaper. It does not:
both arms pay for every token the model generates. The 0.136 macro F1
that reasoning-first buys is paid for in tokens either way, and the
decision stands on its own without the discount it was credited with.

Scope: 20 messages, two streamed completions each, one 4B local model.
The comparison assumes the server answers the same request the same way
twice, which every run in this lane has supported.

## Results — intent-gate-non-english (2026-09-04)

A re-measurement, not a new hypothesis: the language table above was
taken with generic names and the gate, and the gate has since been
demoted to a fallback, so those numbers described a mechanism nobody
should reach for first. Both arms run here, on domain-bearing names,
twelve meanings per language.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| domain names | 0.881 | 0.900 | 0.813 | **0.914** | **0.652** | 0.813 |
| domain names + gate | 0.602 | 0.900 | 0.727 | 0.914 | 0.548 | 0.813 |

**The premise of this backlog item is refuted.** It was filed as "the
gate loses precision OUTSIDE English". With domain-bearing names the
gate does not pay in ANY of the six languages: it is neutral in three
and costs in three, and its worst damage is in ENGLISH (0.881 -> 0.602,
−0.28), not in Russian (−0.10). So the demotion decided by
intent-domain-in-names generalises across languages rather than being an
English-only result, and the "non-English" framing was an artifact of
having measured the gate only against generic names.

**The language gap is real and naming does not close it.** Russian is
the weakest at 0.652 with names alone and stays the weakest — this is
not a gate artifact. Spanish (0.914) and French (0.900) sit above
English (0.881), so the effect is not a simple English-first ordering
either; it lands on particular languages, and on this evidence Russian
and German are the ones to look at. Filed as its own item: the
candidates are case names in the message's language and an explicit
domain sentence, and they are worth trying separately because they cost
different things.

Scope: twelve messages per language, one 4B local model, one run per
arm. Twelve messages is a signal, not a measurement of a language —
what it supports is "the gate does not pay anywhere here" (six
languages agreeing) and "Russian is consistently weakest across two
independent runs", not the individual numbers.

## Results — intent-decode-rate-residue (2026-09-04)

Nine percent of replies were still undecodable on the best
configuration, and no lane had looked at them: the harness printed two
examples of a failure and dropped the rest, so four lanes watched the
NUMBER without ever seeing its SHAPE. Grouping the decoder's own
messages — a `groupBy` over what the harness was already collecting and
silently discarding — settled it in one run.

**The residue was not a residue.** Nine of the ten failures were one
malformation:

```json
"intent": { "MeetingRequest": { "what": "..." }, "conf": "high" }
```

The model closes the intent's object one brace too late and swallows
the sibling field. (The tenth was the last surviving `"intent":
"NotAboutMeetings"` — a bare name where the encoding wants a tagged
case.) Nothing in the residue was a hard message or a model limit; it
was one systematic shape error.

**The fix follows from the shape.** `conf` was declared after `intent`
and therefore emitted after it, right where a nested object was still
open. Declared FIRST it has nothing to fall into:

| `Alt` field order | undecodable | macro F1 |
|---|---|---|
| `(intent, conf)` | 10/120 | 0.907 |
| `(conf, intent)` | **0/120** | 0.909 |

Every reply now decodes. Accuracy is unchanged, which is the honest
reading: this was never an accuracy problem, it was ten messages that
never reached the classifier's output at all.

This is the third time in this line that FIELD ORDER turned out to be
load-bearing — first for reasoning before the label (0.136 macro F1),
then for `why` before `alts`, now for `conf` before `intent`. The
declaration order of an `SProduct` is not presentation. A test pins
this one, because it looks exactly like something a later reader would
tidy.

Scope: 120 messages, one 4B local model, one run per arm. A 10-to-0
change on a single systematic shape is stronger evidence than its n
suggests — the mechanism was named in advance and the predicted shape
is precisely what disappeared.