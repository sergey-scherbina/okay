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
- Natural-language temporal parsing in languages other than English.
  English is done (`Temporal`, see Results); a slot still takes
  ISO-8601 and validates through `SIso`, and the parser produces what
  the slot accepts.

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

The fixture IS a `Rerun` journal — built in intent-eval-on-journal,
see its Results. `Eval` still takes `(gold, predicted)` pairs from
wherever the caller has them, so nothing in it depends on the journal;
the journal is what feeds it without a model.

## Open requests from a consumer (2026-09-04)

Written from the outside, by an agent that BUILT a router on these
ideas before this module existed and now has to decide whether to
adopt it. The service is not the point and is deliberately not
described; what is worth recording is which seams a caller cannot
reach past, because every one of them is also a seam this module's own
backlog needs.

That caller's shape, only where it changes the argument: four
languages with the encoder IN PROCESS, classes and their example
phrasings authored as DATA and edited without a compiler, and an
abstention that must show a person the two candidates it could not
choose between.

Two things this module already got right and should not be talked out
of. The abstention scores the MARGIN (`s0 - s1`), not the top
probability — a high score that is not separated from the runner-up is
the dangerous case, and that lesson usually costs a production
incident. And `promise` is an `Option`: a bound that the calibration
sample cannot carry is absent rather than optimistic.

**1. One taxonomy value that both tiers read.** The model tier takes
its classes from `Schema[I]`; `NoModel.fit` infers them from the labels
present in its training rows. Nothing connects the two, so the tiers
cannot be pointed at the same taxonomy without aligning it by hand,
and — the sharper problem — a taxonomy that arrives as DATA cannot
reach the model tier at all. `intent-label-distillation` plans to
generate a large labelled corpus; if classes are a Scala enum, that
corpus can define examples but never a class. Proposed: a `Taxonomy`
holding class names plus, optionally, examples per class, with
`Taxonomy.of[I]` from `Schema` as ONE constructor and a parsed form as
another. Both tiers take it.

**2. Language as a key in the fit, not a caveat about it.** A training
row is `(text, embedding, class)`; the language it was written in has
nowhere to live, so a fit over a multilingual corpus pools every
language into one boundary. `intent-language-gap` has already MEASURED
what that costs (0.741 against English's 0.929), and
`intent-embedding-choice` is about to re-run the bake-off per language
against a second encoder — which the row shape cannot express. A
centroid averaged across languages is a worse centroid than one per
language for the same reason the gap exists at all. Proposed: rows
carry a language tag and the fit groups by it, falling back to a pooled
model where a language is too thin to fit its own. This is a grouping
key rather than new mathematics, and it turns a measured caveat into a
knob. WORTH DOING BEFORE the embedding bake-off rather than after: the
comparison it is designed to make is per language.

**3. Hand back the ranking at the abstention boundary.** DONE
2026-09-04, in three lanes and worth reading as one story.
`intent-consumer-seams-a` gave `NoModel.Verdict` its `runnerUp` and
`ranked`; `probe-ranked` exposed the distribution `Probe.score` was
drawing its verdict from, after a consumer wiring the probe into a
router found that an operator diagnostic listing every class could not
be built without re-implementing the softmax outside; and
`nomodel-real-distribution` connected them.

IT HAD TO BE THREE, because the middle one was a seam nobody could
reach for until it existed. Without it `NoModel` asked the probe for
one probability at a time and gave every non-winner the SAME
fabricated share, `(1 - p(best)) / (n - 1)` — so `ranked` was ordered
arbitrarily below rank 1, `runnerUp` was whichever class `sortBy` saw
first among the ties, and a pattern cue could promote the class the
probe ranked LAST past the one it ranked second. Both consumers named
below read exactly that fabricated part.

The lesson for the tests, not just the code: at TWO classes the
fabrication is arithmetically exact, `(1 - p) / 1`, and the whole
calibration suite was built on two classes. It went green throughout.
A property about a distribution needs three of something to be a
distribution at all.

`Probe.Verdict` carries `margin` and `runnerUp`; `NoModel.Verdict`
keeps `best` and drops them, so a caller that abstains knows only THAT
the classifier declined. Two consumers of the missing value: an
interface that offers the two candidates it could not separate, and
`intent-active-learning`, which selects the next examples to label by
uncertainty and therefore needs the distribution rather than the
winner. The value already exists one layer down; it is discarded on the
way out.

**4. A fitted model should persist as data.** `Trained` is arrays of
doubles with no codec, so fitting lives wherever loading lives. A
service that already compiles its vectors at BUILD time wants to fit
there too and load weights at boot, never carrying the training path
into the request path at all. Given `okay-codec` and Schema derivation
this is a small piece, and it is what makes "no generation on the
request path" also mean "no fitting on the request path".

**5. Slots deserve the description classes already have.** `Temporal`
parses one slot in one language; `intent-crf-slots` is filed for the
general case and ordered after the class problem. The shape that
consumer arrived at independently, and would contribute: a slot is a
NAME, a question to ask when it is unanswered (per language), and a
parser `String => Option[Value]` whose failure is a re-ask rather than
a silently stored string. Under that description `Temporal` is one
parser among several, another language is another parser rather than a
rewrite, and a learned tagger (the CRF lane) becomes an alternative
implementation of the same seam instead of a separate design. It also
gives the frame half of "a label cannot be acted on; a filled frame
can" somewhere to live, which the Overview promises and no type
currently holds.

**6. Name the dependency, not the deployment.** The bake-off's tables
read "one embed" and "needs a server", and the second half is an
assumption about how the caller is deployed rather than a property of
the tier. With an in-process encoder the same row is a tier with NO
network at all, which changes which one a reader picks — the probe at
86.7% stops being the expensive option and becomes the cheap one. The
dependency the tiers actually have is `String => Embedding`; saying
that costs nothing and stops the tables from arguing for the wrong
tier.

**7. A suspension that is waiting for a PERSON.** The first draft of
this section said conversation state — a pending question, an answer
bound to the field that asked it, an interrupt — belongs to a caller
and not here. That was two claims wearing one sentence, and only the
first survives review.

The one that holds: a CLASSIFIER stays pure. `NoModel.classify` is a
function of a message, which is what makes it testable, cacheable and
evaluable as a fold; giving it session state would cost all three.

The one that does not: that the conversation itself has no home in
this workspace. Written as a straight-line program over the effect
system, an intake IS a delimited continuation — ask, ask, ask, then
act — and an interrupt is an abort to the delimiter, which is what
delimited control is FOR. The consumer hand-rolled a small state
machine (an ADT of pending states, one case per suspension) not from
principle but because it already had a log of its own and did not
reach for the platform's. Defunctionalising a continuation by hand is
a fair trade when the state must be inspected and rebuilt, and it has
a cost that was paid in full: the language of the exchange, free as a
captured variable in a continuation, had to be pinned into the state
explicitly, and the bug that reached a user was exactly the turn where
it was not.

What `Durable` already has is the hard half. The journal is written
INTENT FIRST and the answer after, and on recovery the recorded
answers are handed back without touching the world — a program that
resumes across a restart without its stack. An `Entry` whose `answer`
is `None` is, structurally, a question that has been asked and not yet
answered.

What it does not have is that reading. Every missing answer is treated
as the crash window — an anomaly for `OnRepeat` to resolve — and there
is no state for "asked a person, waiting, and this is normal, possibly
for days". Give it one and a conversation becomes a durable program
rather than a hand-written state machine, with the pending question
already in the log where a restart can find it.

Two consequences worth stating before anyone builds it. Replay must
resume from RECORDED verdicts, not recomputed ones: a router that
re-classifies its own log rebuilds a different conversation the day
the model is refitted, which is the same reason the model tier's
turns are skipped on replay rather than re-asked. And the suspension
point takes a message that may not be the answer at all — a
correction, an unrelated request, an exact command — so the resumed
value is a choice, not a string, and the handler is what decides
which.

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

## Results — intent-eval-on-journal (2026-09-04)

Every measurement in this line has been a live run of ten to thirty
minutes, which is why several questions went four lanes without being
asked — including the one whose answer turned out to be a `groupBy`.
This makes the parts that do not involve a model cost nothing.

Nothing new had to be invented to hold the recording, because **a
recording IS a journal**: `Durable.Entry` already carries
`(seq, op, fingerprint, key, answer)`, `Rerun.Version` already groups
entries under a provenance, and `FileVersions` already stores them. The
model's reply goes in `answer`, the message in `key`, and the PROMPT's
fingerprint in `fingerprint`.

| | live | over the recording |
|---|---|---|
| whole fixture, best config | ~13 min | **0.046 s** |
| needs a model | yes | no |
| runs in the default gate | no | yes |

The replay reproduces the live report exactly — Proposal 0.952, Request
0.929, Notification 0.893, Other 0.862 — which is the evidence that the
replay path is faithful rather than merely fast.

**Two guards, both verified by breaking them on purpose**, because a
guard that cannot fail is worse than none:

- The PROMPT FINGERPRINT. A recording describes the question that was
  asked; change the prompt and it describes nothing. Adding a single
  space to the prompt fails the check with "re-record rather than
  trusting these numbers". That is the correct signal, not an obstacle
  to route around — there is no honest way to score old answers against
  a new question.
- The PROMOTION RULE. `Eval.regressions` has been executable since the
  first lane and had never guarded anything. It guards now: raising a
  baseline by four points fails the run and prints every class's F1.

So a change to the decoder, the label mapping, the gate logic or the
metrics is a second-long check in the default gate, and only a PROMPT
change still costs a live run. The recording is 54KB of JSON, committed
— the size of keeping four lanes' worth of measurement reproducible.

## Results — intent-precedence-rule (2026-09-04)

The reference literature calls overlapping classes "mutually exclusive
in practice" and prescribes a stated precedence rule. This lane asked
where such a rule LIVES, built the answer, measured it, and threw the
answer away.

**The design answer, which stands.** A doc comment cannot be read at
runtime; a prompt parameter does not travel with the type, so the next
caller reconstructs it or does without. The construction that fits this
library is a typeclass beside the schema — `Taxonomy[I]` with a
`precedence: List[String]`, its empty default one priority lower so a
stated taxonomy wins over the silent one rather than being ambiguous
with it. It travels exactly as far as the type does, which is the point
of the taxonomy BEING a type.

**The measurement, which sank it.** Two arms differing only in whether
the taxonomy declares its precedence, over the same 120 messages:

| arm | macro F1 | Proposal | Request | Notification | Other |
|---|---|---|---|---|---|
| no precedence stated | **0.909** | 0.95 | 0.93 | 0.89 | 0.86 |
| precedence stated | 0.866 | 0.92 | 0.89 | 0.84 | 0.81 |

Every class fell, by roughly the same amount. And the rules were
written to match this fixture's own labelling, so they should have
helped BY CONSTRUCTION — that was stated in the claim before the run,
precisely so this outcome could not be reinterpreted afterwards.

The uniformity is the diagnosis: two more sentences of instruction did
not sharpen the boundary they named, they diluted the whole prompt. The
second rule is the sharpest evidence — it said a cancellation with no
new time is a `MeetingNotification`, and `Notification` recall FELL
from 0.83 to 0.77. A rule aimed at a class made that class worse.

**So the mechanism is not shipped.** An API whose only measurement says
it costs 0.043 macro F1 is an unearned claim in code, and this line has
already deleted one of those from prose. The design answer is recorded
here, the four lines that implement it are in this history, and adding
them back when there is evidence costs nothing. Reverting also left the
prompt unchanged, so the recorded journal stays valid — which is
`intent-eval-on-journal` paying for itself the same day.

What to try before reaching for this again: rules rendered as EXAMPLES
of the disputed case rather than as prose (few-shot examples are the
one lever that has consistently paid in this line), and a single rule
rather than a list. Filed.

## Results — intent-tiebreak-by-example (2026-09-04)

The precedence lane's own suggestion, tested: carry a tie-break as
EXAMPLES of the disputed case rather than as prose. Same two decisions,
same 120 messages, two arms differing by exactly two added examples.

| arm | macro F1 | Proposal | Request | Notification | Other |
|---|---|---|---|---|---|
| examples as shipped | **0.909** | 0.95 | 0.93 | 0.89 | 0.86 |
| + two tie-break examples | 0.854 | 0.90 | **0.76** | 0.91 | 0.85 |
| (prose rules, previous lane) | 0.866 | 0.92 | 0.89 | 0.84 | 0.81 |

**Worse than the prose it was supposed to improve on**, and the damage
is specific rather than diffuse: `Request` RECALL collapses from 0.87
to 0.63 while `Proposal` precision falls 0.91 -> 0.81. The example did
exactly what it said — "a message that both proposes and asks is a
proposal" — and the model applied it to requests that were not disputed
at all. A tie-break shown as an instance does not stay inside the tie.

**So the overlap is not fixable from the prompt.** Two independent
channels, prose and example, both moved the boundary in the intended
direction and both paid more elsewhere than they gained. That was
written into the claim before the run, so it is a conclusion rather
than a consolation: `Proposal` and `Request` overlap because the
TAXONOMY draws them that way, and a boundary a taxonomy draws is moved
in its labels and its class definitions, not in an instruction to the
model.

**And a caution about the one lever that had always paid.** Few-shot
examples improved every arm they touched in this line — decode rate,
`Other` recall, macro F1 — and this is the first measurement where they
COST. The difference is what the example teaches: an example of a CLASS
generalises usefully, an example of a BOUNDARY generalises past the
boundary. That distinction is worth carrying into any prompt work here,
because "add an example" has otherwise been free advice.

Nothing shipped. The two tie-break examples stay in `IntentFixture` as
the evidence for the next person who reaches for this, and the
`Taxonomy[I]` typeclass refused in the precedence lane stays refused —
neither channel earned it.

## Results — intent-temporal-slots (2026-09-04)

A slot typed as ISO-8601 refuses "next thursday", so until now the
MODEL did the conversion and the schema only checked it — a model doing
arithmetic, which is the one thing it is worst at and a parser is best
at. `Temporal` does it instead.

**Not built on `okay-lex`'s `Scan`, deliberately.** That machinery
earns its keep carrying lexer state across chunk boundaries and
relexing incrementally after an edit; a five-word phrase has neither.
What a temporal parser needs is to be TOTAL and DETERMINISTIC, and that
is a function.

**Deterministic means the reference day is an argument.** "Next
Thursday" is not a value, it is a value relative to a day someone has
to name — and a parser that reads the clock cannot be tested. Every
test here is anchored to Friday 2026-09-04.

**Total means `None` rather than a guess**, and the refusals are as
much the deliverable as the parses: "soon", "end of the month", "the
14th", "later this week", "in a couple of days" are all guessable, and
each guess would be ACTED on — a meeting booked, a deadline moved.
A declined phrase is asked about. Declining is the cheap failure.

Scope, which is the list of shapes scheduling mail actually uses: an
explicit ISO date, today/tomorrow/the day after/yesterday, `in N days`
and `N days from now` and `N days ago`, a bare or qualified weekday
(`thursday`, `next thursday`, `this tuesday`, `last friday`), `next
week`, a month-and-day in either order taking the COMING year, and a
time in either spelling (`at 2pm`, `at 14:30`) riding along with any of
them.

The calendar underneath is Hinnant's civil algorithm rather than month
tables and leap-year branches, because the hand-rolled version is wrong
at exactly the dates nobody tests. Those dates are tested here anyway:
2024-02-28, 2023-02-28, 1900-02-28 (not a leap year), 2000-02-28 (but
that one is), and a year boundary. No `java.time`, so this holds on the
JS build too.

13 tests, three of them properties: epoch-day round-trip over 200,000
days, day-of-week advancing and wrapping, and totality over arbitrary
ASCII. One of those started as `forAll(...).check()` inside a `test`
block, which prints and returns and cannot fail a suite — it was
scenery, and is now a `property`.

## Results — intent-language-gap (2026-09-04)

The precondition first, as the entry demanded: the parallel set grew
from 12 meanings to 30 in all six languages, weighted toward the
out-of-domain boundary where the classifier actually breaks.

**The larger fixture refuted a claim this spec was carrying.** At n=12
Spanish (0.914) and French (0.900) sat above English (0.881), and the
gate-non-english Results concluded from that "not a simple
English-first ordering". At n=30 the ordering is: English 0.929, then
German 0.895, Spanish 0.890, Japanese 0.888, French 0.887, and Russian
0.741. The middle was noise; twelve messages could not tell those four
apart, which is exactly what the backlog entry warned when it made
growing the set a precondition.

What survives is the gap itself: **Russian sits ~0.19 below English in
two independent runs at two different fixture sizes.** That is the
finding; the ranking of the middle is not.

**Candidate one — case names in the message's own language.** Five
taxonomies (`RencontreFr`, `BesprechungDe`, `ReunionEs`, `ВстречаRu`,
`会議Ja`); Scala takes non-ASCII identifiers, so testing this cost only
typing.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| English names | 0.929 | 0.887 | 0.895 | 0.890 | 0.741 | 0.888 |
| native names | 0.929 | 0.927 | 0.788 | 0.732 | 0.791 | 0.891 |

Helps French (+0.040) and Russian (+0.050), badly hurts German (−0.107)
and Spanish (−0.158); −0.029 on average. If the name worked by being
UNDERSTOOD, the gain would be systematic and it is not. The English
pair is the harness's own guard — both arms run the same taxonomy there
and both score 0.929 exactly, so the comparison is comparing what it
claims to.

**Candidate two — say the subject out loud, in the reader's language**,
leaving the English names alone.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| plain | 0.929 | 0.887 | 0.895 | 0.890 | 0.741 | 0.888 |
| domain stated | 0.848 | 0.887 | 0.688 | 0.844 | 0.765 | 0.888 |

Also negative: −0.052 on average, German −0.207, only Russian gains
(+0.024).

**So neither candidate fixes the gap, and both cost.** That is the
fourth time in this line that adding PROSE to the prompt has cost —
after precedence rules, tie-break examples, and now a domain sentence.
The things that have ever paid here are structural: the rendered
example SHAPE, few-shot examples OF A CLASS, domain-bearing names in
English, and field order. A pattern worth stating plainly: this prompt
is at the point where more words make it worse, and the remaining
levers are in the type and the examples, not in the instructions.

One candidate remains untried and is filed rather than claimed: the
example MESSAGES stayed English throughout, on purpose, so that the
names arm moved one variable. Translating the examples themselves is
the obvious next thing and it was deliberately not confounded into
this lane.

Scope: 30 meanings per language, one 4B local model, one run per arm.
The translations are author-written, which more rows do not fix — a
gap measured against my own Russian is a gap in a joint measurement of
the model and the translator.

## Results — intent-symbolic-tier (2026-09-04)

Built on the operator's instruction rather than on its trigger, which
never fired, and therefore measured as a hypothesis rather than shipped
as a default.

**It cost one file, because the tier is a projection of machinery that
already exists.** FrameNet's "lexical units" are, here, BM25 over
labelled examples: `okay-rag`'s `Postings` is already a `Fold` and a
`Monoid`, `Keyword.search` already scores, and `Symbolic` is the
mapping from a class to the examples that carried it. That is worth
noting on its own — the retrieval stack and the classifier turned out
to want the same index.

Measured on a deterministic split: odd positions train, even positions
are scored, no message in both. An index scored against its own
examples measures nothing, because BM25 finds the identical document
and reports a perfect margin.

| margin ≥ | coverage | agreement with gold |
|---|---|---|
| 0.0 | 100.0% | 45.0% |
| 0.1 | 73.3% | 54.5% |
| 0.2 | 55.0% | 63.6% |
| 0.3 | 48.3% | 62.1% |
| 0.5 | 16.7% | 60.0% |

**Speed is not the problem: 112µs per message**, against seconds for a
model call, and the Linagora system's sub-150ms claim is clearly
reachable this way.

**The problem is that agreement does not rise with the margin.** It
plateaus at 60-64% and FALLS at 0.5. A usable filter approaches the
model's own accuracy as its threshold tightens; this one does not,
which says the margin is not a confidence signal. Without a threshold
at which the tier is safe to answer, there is no way to put it in front
of anything.

The arithmetic of shipping it anyway: at margin 0.2 it takes 55% of
traffic at 64% accuracy where the model tier is near 90% — roughly 14
points of end-to-end accuracy spent to save 55% of the calls. The
trigger for this tier was "cost or latency binding", and neither is.

**So it is not wired into `Classify`.** `Symbolic` is a working,
tested, 112µs classifier that anyone can reach for; nothing calls it,
and the reason is written here. What would change the verdict is a
better representation rather than a better threshold — which is exactly
what the vector tier tests next, and the honest reading of this table
is that it makes that lane MORE interesting, not less: paraphrase is
where BM25 is structurally weak.

## Results — intent-vector-tier (2026-09-04)

The last tier, and the first one that earns its place. Same fixture,
same odd/even split as the symbolic tier, same three numbers, so the
two tables are comparable line for line.

| margin ≥ | coverage | agreement | (symbolic, for contrast) |
|---|---|---|---|
| 0.00 | 100.0% | **80.0%** | 45.0% |
| 0.02 | 76.7% | **87.0%** | 54.5% |
| 0.05 | 45.0% | **96.3%** | 63.6% |
| 0.10 | 8.3% | 100.0% | 62.1% |

**The agreement RISES with the margin — monotonically — where the
symbolic tier's plateaued and then fell.** That is the answer to the
question the symbolic lane left open: the binding constraint was the
REPRESENTATION, not the idea of a cheap tier. BM25 matches words, and
the words that carry an intent are function words and syntax ("could
you" against "shall we"), which it either drops as stopwords or weights
by rarity rather than by role.

**And the operating point is real.** At margin 0.05 the tier answers
45% of messages at 96.3% agreement — ABOVE the model tier's ~90% macro
F1 on the same fixture. So on the slice it accepts, it is not merely
cheaper, it is more accurate; the model's remaining value is on the
half it declines, which is exactly the shape a first pass should have.

**Cost, with the number the batch hides.** 12ms for one message's
embedding round trip plus 90µs to classify, against seconds for a
generation. Production embeds one message at a time, so 12ms is the
honest figure rather than the batched one.

**What this changes about the trigger.** The tier was filed behind
"cost or latency binding", and that trigger still has not fired. It
does not need to: the tier is more accurate than the model on the
traffic it accepts, which is a better reason than saving money, and a
different one from the one the backlog anticipated.

**How to compose it** — three lines at the call site, deliberately not
hidden behind a wrapper, because a wrapper would obscure which call you
are paying for:

```scala
val v = embedOne(message)                       // your gateway, your effect
Centroid.classify(centroids, v, floor = 0.05)    // 90us, answers ~45%
  .getOrElse(askTheModel(message))              // the rest costs what it always did
```

`Centroid` never calls a gateway itself, which is why it tests on every
platform and why the embedding effect stays where the caller can see
it.

Scope: 60 train / 60 test, one split, one 4B-era embedding model at
1024 dimensions, gold labels author-written. The 100% at margin 0.10 is
five messages and means nothing on its own; the shape of the curve is
the finding, not any single cell.

## Results — intent-tier-bakeoff (2026-09-04)

**The goal changed mid-programme and the reporting changed with it.**
These tiers were built as cheap filters in FRONT of a model; the target
is now a classifier that needs no generation on the request path at
all. So the number that decides a tier is its accuracy at FULL
coverage, and the margin table is beside it for whoever wants to hand
the uncertain tail to a person rather than to a model.

Embeddings stay inside that budget: a vectoriser is 12ms and no tokens,
and labels may come from a model once, offline. The ban is on a model
being present when a message arrives.

Five tiers, one split, one table.

| tier | accuracy over ALL | per message | dependency |
|---|---|---|---|
| symbolic (BM25) | 45.0% | 147µs | none |
| patterns | 51.7% | 96µs | none |
| kNN (k=5) | 58.3% | 158µs | `String => Embedding` |
| centroid | 80.0% | 75µs | `String => Embedding` |
| **linear probe** | **86.7%** | 76µs | `String => Embedding` |
| (model tier, for scale) | ~90% | seconds | a generation |

**The probe is within a few points of the model** at 12ms plus 76µs,
with no generation, and it fits in 164ms on 60 examples. At margin 0.60
it answers 65% of messages at 97.4% — ABOVE the model — which is the
shape that makes a hand-off to a person cheap rather than embarrassing.

**Patterns confirmed the mechanism the BM25 failure implied.** Where a
cue fires it is 88.6-90.9% accurate against BM25's 63%, on the same
messages, with no network and 96µs. The cues match syntax and never a
subject: "shall we" is a proposal, "could you" a request, "FYI" at the
START a notification. What limits it is coverage — 58.3% of messages
contain no cue at all — not precision.

**kNN was my prediction, and it was wrong.** I expected nearest
neighbours to beat the centroid because `Other` is a deliberate
grab-bag whose mean resembles none of its members. It scored 58.3%
against the centroid's 80.0%. The reason is not shape but SIZE: with
fifteen examples per class, five neighbours are mostly noise, and
averaging is what rescues a small sample. The hypothesis was about
geometry and the answer was about sample size.

**A note on the "dependency" column, added after a consumer pointed
out that it was arguing for the wrong tier.** These tables used to say
"needs a server", and that is a statement about how the READER is
deployed, not about the tier. What the vector tiers actually require is
a `String => Embedding` — and where that function is in process, the
probe at 86.7% is the CHEAPEST option on the table rather than the most
expensive one, because no network is involved at all. The measurements
here were taken over HTTP because that is what this machine offers; a
caller with an in-process encoder should read every "12ms round trip"
as their own encoder's latency and re-rank accordingly.

**Ordering the tiers by what they cost.** Two of them need no network
at all and neither reaches 52%. Every tier that clears 80% needs an
embedding. So the honest statement of the no-model target is: it is
reachable, and it costs one 12ms vector call per message — not zero
infrastructure, but no generation, no tokens, and no per-call price.

What this leaves for the cascade lane: patterns answer 58% at ~89% for
free, and the probe answers everything at 86.7%. Whether running
patterns first and the probe second beats the probe alone is an
arithmetic question with a real answer, and it is measured next rather
than assumed here.

## Results — intent-no-model (2026-09-04)

The assembly the bake-off argued for, plus the two pieces it was
missing: a character n-gram tier for the zero-network path, and a
calibrated point at which the classifier declines to answer.

**Character n-grams: the property arrived, the accuracy did not.** TF-IDF
over hashed 3-5 character n-grams with the same optimiser as `Probe`,
no tokenizer, no server, no network.

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| accuracy | 53.3% | 53.3% | 53.3% | 33.3% | 46.7% | 60.0% |

Flat across languages, which is the design working — a 4-character
window does not know what alphabet it is in, and the English advantage
that every embedding tier shows is simply absent. On the English
fixture it reaches 60.0% at full coverage, above patterns (51.7%) and
BM25 (45.0%) and far below the probe (86.7%), at 92µs per message with
a 404ms fit.

(That per-message figure took a correction: the model is a lazy val and
the first version of the timing block forced it INSIDE the loop, so a
404ms fit was divided among sixty messages and reported as 6ms each.
The full gate caught it, because a "fast tier" assertion failed once
the JIT was cold — an assertion that exists precisely to notice this.) At 60 training examples a
4096-dimension hashed model is under-fitted, so this is a DATA result
rather than a verdict on the method (see `intent-label-distillation`).

**Stacking did not pay, and the default says so.** Pattern verdicts
blended into the probe's distribution, weight fitted on a calibration
split from a six-point grid: the search picked 0.8 and cost five points
on held-out data (70.0% against the probe's own 75.0%). The sweep is
monotone —

| weight | 0.0 | 0.1 | 0.3 | 0.5 | 0.8 |
|---|---|---|---|---|---|
| accuracy | 75.0% | 75.0% | 72.5% | 72.5% | 70.0% |

— so the shipped default grid is a single zero. Forty calibration rows
cannot support choosing even one number, and that sentence is in the
code beside the default.

**The abstention took three attempts, and the third one is honest.**

1. Threshold at the point where calibration accuracy still met the
   target: promised 96.2% over 65%, DELIVERED 88.9% over 45%. This is
   the classic error — choosing a threshold on a sample and quoting
   that sample's accuracy as a prediction about the next one.
2. A proper split-conformal quantile with the finite-sample rank
   `ceil((1 - alpha)(m + 1))`: promised 100% over 55%, delivered 88.2%
   over 42.5%. Better construction, same overclaim, because with ten
   calibration errors the rank runs off the end of the list and the
   bound silently degenerates.
3. **The promise became an `Option`.** A conformal bound at error rate
   `alpha` needs at least `(1 - alpha) / alpha` calibration MISTAKES to
   exist — nineteen at 95%. Below that there is no bound to report, and
   reporting the empirical number anyway is precisely the overclaim.
   The classifier now says: *no promise, 6 calibration errors, 19
   needed; the threshold still applies, the guarantee does not.*

The threshold still earns its place without the guarantee: 88.2% on the
42.5% it accepts, against 75.0% at full coverage. It is a good filter
that is honest about not being a proof.

**Where this leaves the no-generation target.** The probe answers
everything at 86.7% (60 training examples) or 75.0% (40), for one 12ms
embedding call and no tokens. The model tier is ~90%. The gap is
credibly a DATA gap rather than a method gap, and the lane that closes
it is `intent-label-distillation`: use the model once, offline, to
label a large corpus, and keep it out of the request path entirely.

## Results — intent-learning-curve (2026-09-04)

The cheapest lane in the programme, run to decide where the expensive
ones go — and it overturned the plan it was meant to confirm.

| training examples | probe | centroid | chargrams |
|---|---|---|---|
| 8 | 51.7% | 48.3% | 30.0% |
| 16 | 66.7% | 65.0% | 38.3% |
| 24 | 75.0% | 78.3% | 46.7% |
| 32 | **85.0%** | 75.0% | 53.3% |
| 40 | 81.7% | 83.3% | 55.0% |
| 48 | 85.0% | 83.3% | 55.0% |
| 56 | 86.7% | 80.0% | 65.0% |
| 60 | 85.0% | 80.0% | 60.0% |

**The probe flattens at about 32 examples.** Everything from 32 to 60 —
nearly a doubling — moves it between 81.7% and 86.7%, which is noise on
sixty held-out messages. The centroid flattens in the same place at a
slightly lower level, and it fits four vectors against the probe's 4096
weights: two models with three orders of magnitude between their
parameter counts stop improving at the same point, which is what a
SIGNAL ceiling looks like and not a capacity one.

**So the standing plan was wrong, and it was mine.** I had written that
the 86.7%-against-90% gap was "credibly a data gap rather than a method
gap" and named `intent-label-distillation` as the lane that closes it.
The curve says the labels are not the binding constraint: another
sixty of them buy nothing measurable. What is left is the
representation, so `intent-embedding-choice` moves ahead of
distillation.

**One tier is still climbing, and it is the interesting one.**
Chargrams go 30.0 → 65.0 across the same range and have not flattened.
That is the ZERO-NETWORK path — no embedding server, no per-message
round trip — sitting at 60-65% because it is starved, not because it
is finished. Distillation is not dead; it simply belongs to the tier
that can still eat, and a chargram model trained on thousands of
distilled labels is the only candidate for a classifier with no network
at all.

Read this against its size: 60 test messages, so a 3-4 point move is
noise, and the flatness of the right-hand half is the finding rather
than any single cell.

## Results — intent-embedding-choice (2026-09-04)

Promoted ahead of distillation by the learning curve, and half blocked
by the machine: the central experiment needs a second embedding model
and there is exactly one installed.

**What the gateway actually does, stated correctly this time.**
`/v1/embeddings` validates the `model` field and refuses anything that
is not an embedding model with HTTP 400; asking for it by name or
omitting it returns byte-identical vectors, and the response reports
which model answered
(`mlx-community/Qwen3-Embedding-0.6B-4bit-DWQ`, 1024 dimensions).
An earlier note in this programme said the gateway "ignores the model
field", inferred from two requests that both returned 1024 dimensions —
two different models can share a dimension, and the vectors should have
been compared instead of their shapes. Every measurement in this spec
was made with that one model, and results should be read as facts about
it rather than about embeddings in general.

**Is the ceiling the representation or the task?** The learning curve
ruled out capacity; this rules out the task. The recorded journal holds
the model tier's answer for every fixture message, so the two can be
compared with no calls at all:

| | wrong of 60 |
|---|---|
| model tier | 4 |
| probe | 8 |
| **both** | **0** |

Not one shared mistake. If the messages were inherently ambiguous the
two would stumble over the same ones; instead each has its own blind
spots, so the signal the probe misses IS present in the text and its
representation is losing it. That is the ceiling, and it is
representational.

**Framing moves the same model by 6.6 points.** One embedding model,
four ways of asking:

| framing | probe | centroid |
|---|---|---|
| bare message | 86.7% | 80.0% |
| "Classify the intent of this message: " | **88.3%** | **83.3%** |
| long e5-style task instruction | 81.7% | 65.0% |
| "Represent this message for intent classification: " | 81.7% | 78.3% |

The short classification instruction is the best of the four, and the
gain over bare text (+1.6 probe, +3.3 centroid) is at the edge of noise
on sixty messages — but the SPREAD is not: 81.7 to 88.3 from wording
alone, with both models moving together. So "choose the embedding" is
not only a question of which model, and the same rule this line has
found everywhere else applies here too — a short instruction helps, a
long one costs.

**Concatenating an orthogonal representation did not help**: embedding
86.7%, chargrams 51.7%, both together 85.0%. A weak signal glued to a
strong one is a poor test of orthogonality, so this refutes little.

**What remains blocked, and it is installation rather than code.** A
second embedding model would settle whether 88.3% is this vectoriser's
limit. Candidates that fit the constraints (local, ideally MLX,
multilingual for the Russian arm): `Qwen3-Embedding-4B/8B` as the
same-family upgrade, `BGE-M3` and `multilingual-e5-large` for
multilingual strength, `jina-embeddings-v3` for its classification
adapter, `gte-multilingual-base` for size. And for the no-network goal
specifically, static embeddings (`model2vec`/`potion`): a distilled
lookup table with no neural inference at request time, which would slot
straight into `Centroid` and `Probe` because neither cares where a
vector came from.

## Results — intent-static-embeddings (2026-09-04)

A classifier with no external gateway at request time. Rather than
downloading `model2vec`, this does what model2vec DOES: distils a
static table from the teacher already in use — embed each unit once,
offline, then tokenize, look up and pool. Nothing but array arithmetic
at request time, so it crosses to JS and Native where a native runtime
could not follow, and no foreign tokenizer has to be matched.

| table | units | sees, of an unseen message | probe | centroid |
|---|---|---|---|---|
| words, from the training half | 301 | 66.0% | 43.3% | 41.7% |
| words, full dictionary | 1019 | 100.0% | 51.7% | 43.3% |
| **words + adjacent pairs** | **1303** | — | **63.3%** | 58.3% |
| (teacher, live vectors) | — | — | 86.7% | 80.0% |

**Vocabulary was part of it and not most of it.** Going from a starved
table to complete coverage bought 8.4 points and left the method at
51.7%, below even chargrams — so the limit was not the dictionary.

**The limit was the bag of words, and this line has met it before.** A
word-only static table cannot tell "could you" from "we could": the
first requests, the second proposes, and a bag holds the same three
tokens either way. That is exactly the mechanism that sank the BM25
tier, arriving a second time by a different road. Adding adjacent PAIRS
to the vocabulary — a unit the teacher embeds like any other — is worth
11.6 points to the probe and 15.0 to the centroid, and takes the
no-network path to its best number so far.

**Where that leaves the zero-infrastructure goal.**

| option | accuracy | needs |
|---|---|---|
| patterns | 51.7% (89% where a cue fires) | nothing |
| chargrams | 60.0% | nothing |
| **static, words + pairs** | **63.3%** | a 5MB table |
| teacher | 86.7% | `String => Embedding` |

So no external gateway is reachable at 63%, and the remaining 23 points
are CONTEXT: a static table gives a unit the same vector wherever it
appears, and representing a word differently in two sentences is most
of what a transformer is for. That is the honest size of the trade, and
it is a property of the method rather than of this implementation.

Table size, since it decides whether this ships: 1303 units at 1024
dimensions is 5.2MB as float32. A production vocabulary of 30k units
would be about 120MB, or 60MB at float16 — which is the argument for
distilling into fewer dimensions as `model2vec` does with PCA, filed
rather than done.

## Results — intent-second-embedder (2026-09-04)

The experiment `intent-embedding-choice` was blocked on. A second
embedding model is now served — `Qwen3-Embedding-4B`, 2560 dimensions
against the 0.6B's 1024, genuinely different vectors — so the
vectoriser could finally be the only thing that changes.

| model | framing | probe | centroid |
|---|---|---|---|
| 0.6B | bare | 86.7% | 80.0% |
| **0.6B** | **classify instruction** | **88.3%** | **83.3%** |
| 4B | bare | 76.7% | 76.7% |
| 4B | classify instruction | 85.0% | 80.0% |

**Bigger is not better here, and the first reading of that was wrong.**
Bare, the 4B scores ten points BELOW the 0.6B, which looks like a
verdict on the model. It is not: Qwen3-Embedding is instruction-tuned,
and the larger model turns out to be far more sensitive to being told
what the vector is for — the classify instruction is worth +8.3 to it
against +1.6 to the small one. Framed properly it climbs to 85.0% and
still does not beat the framed 0.6B.

**The mechanism is the one the learning curve already found.** At 2560
dimensions the probe fits two and a half times as many weights on the
same sixty examples, and that curve showed data — not capacity — is
what binds here. A richer representation is a liability in a small-data
regime, which is the opposite of the intuition that sent me looking for
a bigger embedder. It also costs six times the wall clock: 2000ms
against 345ms for 120 messages.

**So 88.3% is this TASK at this data size, not this vectoriser.** Two
independent vectorisers, one of them four times the size, land within
three points of each other, while the model tier reaches ~90% and
shares none of the probe's errors. The remaining gap is not something
another embedding model closes.

**The per-language table is under-powered and no conclusion is drawn
from it.** Trained per language, each arm has fifteen examples — the
learning curve put the probe's stabilisation at about thirty-two — and
the numbers swing from 46.7% to 86.7% accordingly. They are recorded
for the next lane rather than interpreted:

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| 0.6B | 73.3% | 53.3% | 60.0% | 53.3% | 86.7% | 60.0% |
| 4B | 73.3% | 66.7% | 66.7% | 46.7% | 60.0% | 53.3% |

A per-language verdict needs the parallel set grown to at least thirty
examples per language, which is `intent-language-fixture-growth`.

## Results — intent-consumer-seams-a (2026-09-04)

The two smallest of the seven requests a consumer wrote into this spec
(0fc7386b), taken first because one of them was misleading readers
today. Recorded before anything else: I rebased over that commit six
times before reading it, looking only at my own Results sections rather
than at the file, and one of its notes — that the language key was
worth doing BEFORE the embedding bake-off — was advice I had already
walked past by the time I read it.

**(6) The dependency is `String => Embedding`, not "a server".** The
bake-off tables said "needs a server", which describes the reader's
deployment rather than the tier, and it argued for the wrong tier: with
an in-process encoder the probe at 86.7% is the CHEAPEST row on the
table, not the most expensive. The column is now `dependency`, the
vector tiers name the function they actually require, and a note says
that the 12ms figures are this machine's HTTP round trip rather than a
property of the method.

**(3) An abstention hands back what it could not separate.**
`Probe.Verdict` had `margin` and `runnerUp`; `NoModel.Verdict` kept
`best` and dropped both, so declining told a caller only THAT it
declined — while the two candidates it could not separate had already
been computed one layer down. `Verdict` now carries `runnerUp` and the
full `ranked` list, and `NoModel.decide` returns both the answer (or
`None`) and the verdict it CONSIDERED, from one call so the two cannot
disagree.

Two consumers of that value, and neither is hypothetical: an interface
that abstains has to show a person the choice it could not make, and
active learning selects the next examples to label by uncertainty,
which is a property of the distribution rather than of the winner.

## Results — intent-model-persistence (2026-09-04)

Request 4 of the consumer's seven, and the piece that makes "no
generation on the request path" also mean **no fitting on it**. Without
a codec, fitting lives wherever loading lives: every process start
re-fits, and re-fitting needs the teacher — so an embedding server is
dragged into the STARTUP path of a service whose request path was
carefully kept clean.

`Fitted` gives `Probe.Trained`, `Centroid.Trained`, `CharGrams.Trained`
and `Static.Table` a record apiece with a derived `Schema`, so a model
is fitted at build time and loaded at boot.

**The schemas are hand-built, and the reason turned out to be smaller
than I first wrote.** Weights are `Array[Double]`, vectors are
`ArraySeq[Float]`, and a derivation sends each as a JSON array of
numbers — which is how an embedding once travelled as `List[Double]`.
Numbers ride as bytes here instead. Measured: a two-class probe over
1024 dimensions is **21KB as bytes against 36KB as decimal literals**,
1.7x rather than the order of magnitude the first draft of the comment
implied, because base64 hands back a third of what binary saves. What
survives the number is the part worth keeping — no boxing on the way
through, and a matrix that carries its width so a reader can check it,
rather than a nested list whose rows might disagree.

**What the tests assert is the classifier, not the bytes.** Round-
tripping fields is the easy half; a caller needs the loaded model to
ANSWER what the fitted one answered. So every case compares
predictions across the trip, and the probe's case compares
probabilities to 1e-12 — identical, not merely agreeing — with a
ScalaCheck property doing the same over random fits.

**One thing a table cannot carry: its splitter.** `Static.Table` holds
a `String => Vector[String]`, and a function is not data. `load` takes
it back as an argument rather than defaulting, because passing
`Static.tokens` to a table distilled over `Static.units` is a silent
accuracy loss — pairs stop being looked up and nothing errors.

## Results — intent-taxonomy-and-language (2026-09-04)

Requests 1 and 2 of the consumer's seven, taken together because both
are about what a fit KNOWS.

**(1) One taxonomy value, two doors.** The model tier took its classes
from `Schema[I]`; a fitted tier inferred them from whatever labels its
rows happened to carry; nothing connected the two. `Taxon` is now a
value with `of[I]` reading it out of a `Schema` and `parsed` building
it from strings, and everything downstream takes the value without
caring which door it came through.

The sharper half of the request was the one about DATA: a taxonomy
that arrives as a corpus could not reach the model tier at all, and a
corpus is exactly what `intent-label-distillation` produces — it can
define examples but never a class. `Taxon` derives a `Schema`, so it
round-trips as data and can be edited without a compiler, which is what
that consumer needs and what distillation will need.

`Taxon.check` refuses a label that is not in the taxonomy rather than
letting it through. Without it a typo becomes a class, and then
`Eval`'s rule that an invented label is still a class — right for a
classifier, wrong for a misspelling — quietly scores it.

Named `Taxon` and not `Taxonomy` because the precedence lane shipped
and withdrew a `Taxonomy[I]` typeclass; a name meaning one thing in the
history and another in the code is worse than a slightly odd name.

**(2) Language as a key in the fit.** A row was `(text, embedding,
class)`, so the language had nowhere to live and a multilingual corpus
pooled every language into one boundary. `Row` now carries a `lang`,
and `ByLanguage.fit` groups by it — with the fallback that makes it
usable: a language with fewer than `minRows` examples borrows the
pooled model rather than getting one built from four rows. The
threshold defaults to 32 because that is where the learning curve put
the probe's stabilisation, so it is a policy with a measurement behind
it rather than a round number.

An untagged corpus behaves exactly as before: `Row.Any` is both "no
language" and the pooled key, so nothing changes for a caller who has
one language.

**THE MEASUREMENT IS NOT RUN, DELIBERATELY.** Fitting per language
needs at least 32 rows per language and the parallel set has 30
MESSAGES per language, so a per-language arm would train on fifteen —
below the point where the probe's numbers mean anything, which is
precisely why the previous lane's per-language table was unreadable.
Running it now would produce the same undefendable numbers with a
better excuse. The seam is built and tested; the measurement waits on
`intent-language-fixture-growth`, and that ordering is the whole
lesson of having run the bake-off before this existed.

## Results — intent-label-distillation (2026-09-04)

Reprioritised by this programme's own learning curve and scoped to what
it supports. NOT for the probe, which is flat past 32 examples. For the
tiers that were still climbing when the fixture ran out — chargrams
(30 → 65%) and the static table (63.3%) — which are the only
candidates for a classifier that needs no network at all.

The model is used ONCE, offline, in two passes: it writes messages for
a class, then a second pass classifies them back with the shipped
prompt, and only the ones where generation and classification AGREE
survive. Evaluation never touches generated data — the held-out half of
the human fixture is the only thing scored — so the number cannot be
inflated by the corpus that produced it.

| trained on | rows | accuracy on held-out HUMAN data |
|---|---|---|
| the fixture alone | 60 | 60.0% |
| the distilled corpus alone | 182 | 50.0% |
| **both** | 242 | **66.7%** |

**Distillation is a supplement, not a substitute.** Trained only on
what the model wrote, chargrams score 50.0% — ten points BELOW the
human fixture that is a third the size. The model's own writing has a
different distribution from real messages, so it adds coverage rather
than replacing evidence. Together they beat either, and 66.7% is the
best zero-network number this programme has reached, above the static
table's 63.3%.

**The filter's own number is the most interesting one here: 182 of 320
survived, 57%.** The model contradicts its own label on 43% of what it
just wrote — asked to produce a Proposal and then, moments later, asked
what that message is, it frequently says something else. Two readings,
and they are not exclusive: the classes genuinely overlap where the
fixture said they do, and a model asked to WRITE is doing a different
task from a model asked to JUDGE. Either way it is the argument for the
filter — without it, 43% of the training corpus would carry labels the
labeller disowns.

**Generation had to be made resumable, which is a lesson about the
harness rather than the method.** Thirty-two model calls do not fit in
one command's budget, and the first version lost the whole corpus when
the run was cut off. Each batch is now written the moment it arrives
and every run adds to what the last one left, with a time budget so the
exit is clean rather than a kill. 320 messages took 277 seconds across
resumable passes.

## Results — intent-distil-for-probe (2026-09-04)

The learning curve found the probe flat past 32 examples and this spec
concluded that labels are not its constraint. That was drawn on ONE
author's sentences in one register, and the distillation lane then
showed the generated corpus has a measurably different distribution.
So the flatness might have been about homogeneity rather than quantity.
The corpus was already generated; it only had to be embedded.

| trained on | rows | probe | centroid |
|---|---|---|---|
| the human fixture alone | 60 | 86.7% | 80.0% |
| **+ 40 distilled** | 100 | 86.7% | **90.0%** |
| + 80 distilled | 140 | 83.3% | 88.3% |
| + 120 distilled | 180 | 83.3% | 86.7% |
| + 320 distilled | 380 | 73.3% | 78.3% |
| distilled alone | 320 | 50.0% | 63.3% |

**A little different data is worth ten points to the centroid — and it
is now the best number in the programme.** 90.0% matches the model
tier's ~90%, from the SIMPLEST tier there is, at one embedding call and
no generation. The probe, which was the headline for two lanes, does
not move at all.

**More of it is worse, monotonically**, for both: 90.0 → 88.3 → 86.7 →
78.3 as the distilled share grows. That is distribution shift doing
exactly what it does — a mean broadened by a few diverse examples is a
better mean, and one dragged by three hundred of them is a mean of the
wrong population. The probe suffers more because it fits a boundary and
the generated labels carry noise a centroid averages away.

**So both of my earlier readings were half right.** "The probe is
data-bound" was wrong — it is register-bound, and different data does
not help it either. "Labels are not the constraint" was wrong for the
centroid, which gained ten points from forty of them. The quantity that
mattered was small and the tier that mattered was the one I had stopped
looking at.

Read against its size: 60 held-out messages, so 80.0% → 90.0% is six
messages, and the claim rests on the monotone shape of the column
rather than on the single best cell. The distilled rows here are the
UNFILTERED 320; the self-consistency filter that kept 182 of them was
not applied, and whether filtering changes the optimum is filed rather
than assumed.
