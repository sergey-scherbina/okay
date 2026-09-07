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
- Natural-language temporal parsing beyond the fixture's eight
  languages. English, and since intent-temporal-multilingual (see
  Results) fr, de, es, ru, uk, pl and ja, are done (`Temporal`); a
  slot still takes ISO-8601 and validates through `SIso`, and the
  parser produces what the slot accepts. A ninth language is a
  lexicon, not a design.

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

## Requests from a consumer (2026-09-04), and what happened to each

STATUS, added 2026-09-05 because this list was lying to the one reader
it exists for. It marked exactly ONE of seven as done — number 3 —
while six others had landed, several of them the same day the
consumer was told they were open. They said so: "request 5 remains
open, and I am still waiting for a type proposal to review with live
use rather than guessing from outside." The type had shipped, they had
already MIGRATED onto it, and nothing in this document said so.

| | request | status |
|---|---|---|
| 1 | one taxonomy both tiers read | **landed** — `Taxon` as a value (`of[I]` from a `Schema`, `parsed` from data); `Patterns.Cues` carries the taxonomy it decides (ae449ebd); every `Trained` carries the one it was fitted against and `against(taxon, rows)` refuses a stray label (90acdb51) |
| 2 | language as a key in the fit | **landed** — `Rows`/`ByLanguage.fit` groups by language and falls back to the pooled model below `minRows` |
| 3 | hand back the ranking at abstention | **landed**, and was the only one this list admitted |
| 4 | a fitted model persists as data | **landed** — `Fitted` (arrays as bytes, not digits), `Fit.save`/`Fit.grams(json)` as the door, and `Models.meeting` as a model that actually ships (d42c03ca) |
| 5 | slots deserve a description | **landed, and then twice more** — `okay.frame.Slot`/`Frame` (the module split out so both okay-intent and okay-agent can hold it, 99b3344c), typed values through `valueOf` (6a5b8e4d), extraction from the message (99af30b1), `Slot.choice` with per-VALUE wordings and `Source` provenance (60cf8f95) |
| 6 | name the dependency, not the deployment | **landed** — the tables say `String => Embedding` |
| 7 | a suspension waiting for a person | **landed** — `okay.agent.Conversation` over `okay.frame.Frame`, one slot model rather than two (99b3344c) |

**Where 5 landed DIFFERENTLY from what was proposed**, which is the
part a consumer cannot see from outside and is what they were asking
for:

- the descriptor lives in `okay-frame`, a module of its own, not in
  okay-intent — because `okay.agent.Conversation` grew a rival slot
  model the same day and neither module may depend on the other
- `Frame` carries the LANGUAGE of the exchange, and `question`,
  `answer` and `missing` take none. That was the consumer's own
  operational warning and it changed the shape: a per-call language
  makes a mid-exchange flip possible by accident, and they had
  measured one
- an answer knows where it came from (`Said` / `Found` / `Assumed`),
  which nothing in request 5 asked for and their default-value case
  demanded
- `Slot.ask` is a `Map[String, String]` rather than a function of an
  opaque language type: a language that must survive a RESTART has to
  be writable to a journal

**What of this list is still open: nothing.** The open work is
elsewhere and is in BACKLOG.md — `frame-rebind`,
`frame-language-with-grammatical-gender` (raised by the same consumer
and confirmed to exist in this repository's own fixture),
`intent-span-runaway`, and the corpus lanes that no amount of design
will close.

The requests as originally written follow, unedited.

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
Centroid.classify(centroids, v, floor = 0.05)    // 1.3us, answers ~45%
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
| symbolic (BM25) | 45.0% | 147µs† | none |
| patterns | 51.7% | 96µs† | none |
| kNN (k=5) | 58.3% | 158µs† | `String => Embedding` |
| centroid | 80.0% | 75µs† | `String => Embedding` |
| **linear probe** | **86.7%** | 76µs† | `String => Embedding` |
| (model tier, for scale) | ~90% | seconds | a generation |

†EVERY COST IN THAT COLUMN IS WRONG BY 50-70x, and they are kept only
because the accuracies beside them were measured in the same run.
Each was a `System.nanoTime` around a loop inside a test — no warmup,
no JIT accounting, one run — so what they measured was mostly the JIT.
Under JMH (`intent-jmh-row`, 2026-09-05): patterns **1.4 ±0.1µs**,
centroid **1.3 ±0.1**, probe **1.7 ±0.1**, character n-grams **13.7
±0.2**, kNN **13.9 ±1.1**. Read the JMH table below, not this column.

**The probe is within a few points of the model** at 12ms plus 1.7µs,
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
BM25 (45.0%) and far below the probe (86.7%). The 92µs per message
quoted here was a cold loop in a test; under JMH it is **13.7 ±0.2µs**
on the shipped model, and the 404ms fit is **40.1 ±0.8ms**
(`intent-jmh-row`).

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
| chargrams | 60.0% (65.0% on the grown fixture, 2026-09-07) | nothing |
| word TF-IDF + linear | 61.7% (same split and session as the 65.0%) | nothing |
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

## Results — intent-centroid-reconsidered (2026-09-04)

The centroid reached 90.0% in the previous lane and every table before
it had been read with the probe as the subject. Three conclusions
turned on the probe's number specifically, so all three were re-run.
One of them was mine from an hour earlier, and it did not survive.

**The 90.0% headline does not reproduce, and the reason is a condition
I failed to hold fixed.** The distillation lane embedded messages
BARE; this one embeds them with the classify instruction that the
embedding lane had already shown to be better. Same recipe, same
corpus, same split:

| distilled added | probe (framed) | centroid (framed) | centroid (bare, previous lane) |
|---|---|---|---|
| 0 | 88.3% | 83.3% | 80.0% |
| 20 | 88.3% | **86.7%** | — |
| 40 | 85.0% | 85.0% | **90.0%** |
| 60 | 76.7% | 83.3% | — |
| 120 | 80.0% | 75.0% | 86.7% |

Framed, the centroid's gain from distilled rows is +3.4 at its peak
rather than +10, and the peak moves from 40 rows to 20. On sixty
held-out messages +3.4 is two messages and +10 is six, so the honest
reading is that BOTH are near the noise floor and the previous lane's
headline was over-read. What survives across both conditions is the
DECLINE at larger doses — 75.0% at +120 here, 78.3% at +320 there —
which is distribution shift and is visible well outside the noise.

So the corrected claim: distilled data does not lift the centroid to
the model tier's level. It may be worth a couple of points at a small
dose, and it is reliably harmful at a large one.

**The other two re-readings leave their conclusions standing.** The 4B
embedder is worse for the centroid as well as for the probe (framed:
80.0% against the 0.6B's 83.3%), so that verdict was not an artifact of
the probe's parameter count. And the classify instruction helps the
centroid MORE than the probe (+3.3 against +1.6), which means the
default was chosen on the smaller of the two gains — it happens to be
the same default, but the reason recorded for it was the weaker half.

**What this lane is really about.** Two measurements an hour apart
disagreed because one of them silently changed the embedding's framing,
and the second only caught it because a lane existed to re-read the
first. A programme this size accumulates conditions faster than it
records them; the fix is not more care but fewer free variables — every
future arm here states its framing in the printed row, not in the prose
around it.

## Results — intent-module-split (2026-09-04)

The only one of the consumer's seven requests asking for a BOUNDARY
rather than a type, and the one they made a decision rather than a
preference by volunteering to eat the migration.

Fourteen files that turn a message into a class and a frame moved from
`okay.agent` to `okay.intent`: `Classify`, `Eval`, `Taxon`, `Rows`,
`Fitted`, `Probe`, `Centroid`, `Nearest`, `Patterns`, `CharGrams`,
`Static`, `Symbolic`, `NoModel`, `Temporal`. Nothing else moved — a
caller importing `Agent`, `Provider`, `Stepper`, `Durable`, `Rerun`,
`ToolSpec` or `Conversation` is untouched.

**One factoring turned a circular dependency into a split, and it
improves `okay-codec` on its own terms.** Only `Classify` reached back
into `okay-agent`, for `ToolSpec.jsonSchema` — and that function is the
Schema → JSON Schema algebra, the FOURTH algebra over `Schema[A]` after
Json, Cbor and YAML. It never had anything to do with agents. Moved to
`okay.codec.JsonSchema`, with `ToolSpec.jsonSchema` kept as a one-line
delegation so no caller of a tool declaration notices, `okay-intent`
depends on `okay-codec` and `okay-rag` and NOT on `okay-agent`.

The live suites keep a test-only dependency on `okay-agent` (the
journal, for replaying recorded model answers) and on `okay-llm` (a
gateway). That is named in the build rather than left implicit: main
compiles against codec and rag alone, which is the boundary the split
exists to draw.

**`Conversation` stays in `okay-agent`, on the consumer's own
argument** — it is built on `Durable`, whose journal is its state, so
moving it would have recreated exactly the circularity the
`JsonSchema` move had just removed. They accept the residual cost of
importing both modules, and their reason is the better one: a
suspension mechanism belongs with the runtime it suspends, not with
the classifier that happens to sit beside it in a caller's code.

## Results — intent-other-is-a-bin (2026-09-04)

A consumer's observation, promised in the room and owed: `Other` holds
social pleasantries and support complaints, which share nothing, so a
centroid over them is a point between two clouds and a probe's boundary
for them is wherever the mixture fell. Every per-class number in this
spec was computed with `Other` counted as a class, so the answer
reaches back through the programme. The diagnosis is right and
measured. The remedy is wrong and measured too.

**Two thirds of the probe's lead over the centroid lives in `Other`.**

| | probe | centroid | lead |
|---|---|---|---|
| all 60 test rows | 53 | 50 | +3 |
| the 15 `Other` rows | 15 | 13 | **+2** |
| the other 45 | 38 | 37 | +1 |

`Other` is a quarter of the rows and carries two thirds of the gap. The
tier comparison this programme has run for several lanes is, to that
extent, a comparison of how two models cope with one incoherent class.

**It is incoherent, and it is NOT two clusters — it is one diffuse
bin.** Mean pairwise cosine, on framed embeddings:

| | |
|---|---|
| within the pleasantries | 0.645 |
| within the complaints | 0.560 |
| **across the two halves** | **0.551** |
| within `Proposal` | 0.782 |
| within `Request` | 0.705 |
| within `Notification` | 0.684 |

Every real class is tighter than anything inside `Other`. But the
across-halves figure (0.551) is barely below within-complaints (0.560),
so the pleasantry/complaint split is not the structure either: the
complaints half is as loose as the whole. The consumer said two clouds;
the measurement says one cloud with no shape.

**And yet treating it as a REJECTION is worse than treating it as a
class.** Fitting only the three positive classes and answering `Other`
below a confidence floor:

| | best accuracy | `Other` recall there |
|---|---|---|
| probe, as abstention | 68.3% | 26.7% |
| centroid, as abstention | 68.3% | 53.3% |
| (either, with `Other` as a CLASS) | **88.3%** | — |

Twenty points worse. The reason is visible in the diffuseness that
suggested the remedy: an incoherent bin can still be LEARNED when its
members are individually distinctive — "charged twice", "the app
crashes" and "password reset link expired" have their own vocabulary,
and a class can hold them without their resembling each other.
Rejection-by-threshold discards exactly that evidence and keeps only
"none of the three fit", which is a weaker signal than the one
available.

So: `Other` is not a class in the geometric sense the other three are,
its incoherence explains most of the centroid's disadvantage, and the
fix is NOT to convert it into an abstention. If it is to be improved,
it is by splitting it into named classes that are individually
coherent — which is the consumer's other option and remains open.

One measurement defect of my own, recorded because it nearly became a
finding: the first version swept both tiers over the same floors. A
probe margin is a difference of PROBABILITIES and a centroid margin a
difference of COSINES; sharing a range made the centroid abstain on
everything above 0.1 and reported 25% accuracy as though it meant
something. Each now sweeps the range its own measure occupies.

## Results — intent-state-the-framing (2026-09-04)

Not a nice-to-have: this is the defect that produced the afternoon's
retraction. Two measurements an hour apart disagreed by ten points, the
disagreement was read as a finding and published, and a re-read then
showed the runs had embedded their messages differently — one bare, one
with a classify instruction — with neither row saying which.

A convention would not have helped, because a convention is exactly
what there was. `Conditions` makes the terms part of writing a row:

```
human + 40 distilled   probe 86.7%  centroid 90.0%
  [embedder=Qwen3-Embedding-0.6B  framing=bare  train=100 test=60
   corpus=both  distilled=40]
```

That is the retracted cell, printing the `framing=bare` whose absence
made it look comparable to a framed one. There is deliberately no
`line` overload without conditions, so a row cannot be formatted
without them.

Live-scope on purpose. A deterministic test carries its conditions in
its own source; only a measurement against a moving world — a gateway,
a model, a corpus on disk — needs to say what the world was.

**And one thing the first version got wrong, which is the same class of
error one level down.** The distilled count was DERIVED as
`rows.length - trainH.length`, which printed `distilled=260` for the
arm that has no human rows at all. A condition that lies is worse than
one that is missing, because it invites exactly the comparison it
misdescribes. Counts are passed now, not inferred.

## Results — intent-slot-descriptor (2026-09-04)

Request 5 of the consumer's seven, the last still open, proposed rather
than held: they said write the shape and they would bring real usage to
the review instead of specifying from outside a second time.

The Overview of this feature has promised since the first lane that a
label cannot be acted on and a filled FRAME can, and no type held a
frame. `Temporal` parsed one slot in one language and nothing said what
a slot IS, so a second language was a rewrite and a learned tagger
would have been a rival design.

**The shape, which is theirs.** A slot is a NAME, a QUESTION per
language, and a PARSER whose failure is a re-ask:

```scala
final case class Slot[A](name: String, ask: Map[String, String],
                         parse: String => Option[A], required: Boolean = true)
final case class Frame[I](intent: I, slots: Vector[Slot[?]],
                          filled: Map[String, String] = Map.empty)
```

Three things follow that did not before. `Temporal` becomes one
implementation of `parse` rather than a special case — `Slots.when`
is it, wearing the descriptor. Another language is another `ask` entry
and another parser, not a rewrite. And `intent-crf-slots`, when it
comes, is an alternative `parse` behind the same seam.

**`read` returns the QUESTION on failure, not an error.** The caller's
next move is to ask, so that is what it is handed; an error string
would have to be turned into a question at every call site, in every
language. `Frame.answer` returns the frame UNCHANGED when a parse
fails, which is the property the consumer asked for by name: a slot
that cannot read an answer must not store it. The alternative — keeping
the raw string and hoping — is how a field typed as a date comes to
hold "next thursday".

**`missing` is why the type exists at all.** A classifier says
`Proposal`; a caller cannot act until it knows when. The distance
between "I have a class" and "I can act" is a list of unanswered
questions in the reader's language, not a boolean.

**What it deliberately is not: a conversation.** The descriptor
describes. It holds no session state, does not know what has been
asked, and does not decide when to ask — the classifier stays a pure
function of a message, which is what keeps it testable, cacheable and
foldable, and that is worth more than the convenience of putting a
dialogue here. Suspension is `Conversation`'s, in okay-agent, on
`Durable`.

Sent for review rather than declared finished.

## Results — intent-russian-rows-fixed (2026-09-04)

The last of what the consumer's review left owed, and the one where the
defect was mine. They found all three hazards they had warned about, in
my thirty Russian rows, on the axis they said to look at.

**What was wrong, in their words and my code.**

*The person marker carried the class, and it was one letter.* "Не могли
бы ВЫ забронировать" (Request) against "Не могли бы МЫ начать"
(Proposal); "МОЖЕТЕ проверить" (Request) against "МОЖЕМ встретиться"
and "МОЖЕТ, встретимся" (both Proposal) — three spellings of one word
across two classes, and the third is not even the same part of speech.

*Template duplication.* Eight Requests in three shapes, four of them
opening "Пожалуйста, <imperative>", two of those the same sentence with
the object swapped. Plus a true near-duplicate pair inside `Other` —
the same complaint twice, so leaving one out leaves its twin in the
training half.

*Translationese.* "Переговорная изменена на B2" (a room is not "changed
to"), "С этого момента четверги удалённые" (a calque), "Подойдёт ли
пятница утром" (the natural form is "в пятницу утром"). None wrong
enough to fail a reader; all three wrong enough that the row is
evidence about my English source sentence rather than about Russian.

**What changed.** Ten Russian rows rewritten — the constructions, not
the words, because swapping "вы" for something else keeps the class on
one letter. One MEANING replaced across all six languages, since the
duplicate complaint could not be fixed in Russian alone without the
parallel set ceasing to be parallel. After: no "не могли бы" rows at
all, the two remaining "мож-" openings both inside ONE class, maximum
pairwise Jaccard 0.20 and zero pairs above 0.5.

**And the number went DOWN, which is the point.**

| | before | after |
|---|---|---|
| ru, 0.6B probe | 86.7% | **73.3%** |
| en, 0.6B probe | 73.3% | 80.0% |

Russian lost 13 points by being fixed. That is what a fixture defect
looks like from the inside: the twins and the single dominant template
made the task easier than the task is, and 86.7% was measuring my
fixture rather than the classifier. English moved too, because
replacing the duplicate meaning changed every language's arm, not only
the Russian one.

Both arms train on fifteen rows, still below the thirty-two where the
probe stabilises, so these swings carry noise — but the DIRECTION for
Russian is the one the review predicted, and a fixture edit that
changed nothing measurable would have been one nobody could check.

The provenance problem is unchanged and I will not pretend otherwise: I
rewrote my own rows, so the fixture is still one hand's Russian. What
the review bought is that the defects are gone; what it cannot buy is a
second author.

## Results — intent-end-to-end (2026-09-04)

Twenty lanes measured these tiers and nothing used them: inside okay
there was no path where a message arrives and a decision leaves. A
consumer had their own router; `okay-intent` had no caller of its own,
and a library with no callers has the wrong API and cannot find out.

`okay.demo.IntentRouter` is the caller. It is deliberately a ROUTER
rather than a demonstration of a classifier, because the interesting
part is what happens AFTER the class: the frame that class needs, the
question it is still missing, and the decision to ask a person instead
of guessing. Its tier order is the one the measurements argued for —
pattern cues first, since they cost nothing and are 89% accurate where
they fire; the vector tier for the rest; and below its margin nobody
guesses, a person sees the candidates.

It works, in the default gate, with no model and no network. What
matters is **the three frictions it exposed**, none of which any test
had found.

**1. A filled frame hands back TEXT, not the parsed value.** The router
knows the meeting is on the 10th — `Temporal` parsed "next thursday" to
prove the answer was acceptable — and `Frame.filled` can only return
the string the user typed. To act, the caller parses it AGAIN, with the
same reference day, and nothing in the type says so. This was named in
the slot lane as a suspected hole; here it is demonstrated from the
outside, with a test that shows the second parse.

**2. The pattern tier speaks canonical names, so a caller with a
domain-bearing taxonomy writes a mapping.** `Patterns.meeting` hardcodes
`Proposal`/`Request`/`Notification`/`Other`, and the router's taxonomy
is `MeetingProposal`/... — measurements having shown domain-bearing
names are worth keeping. So `IntentRouter` carries a private
`canonicalToTaxonomy`, which every other caller will now write too.
`Cue.cls` is a `String` and could carry any names; what is missing is a
way to say "these cues, against MY taxonomy".

**3. `Taxon` is not connected to the tiers that classify.** The router
holds a taxonomy AND a pattern set AND a centroid, and nothing checks
that they agree — it calls `taxonomy.has` by hand after the fact.
Request 1 asked for one taxonomy both tiers read, and this lane
delivered one taxonomy that neither tier reads: `Classify` takes a
`Schema[I]`, `Patterns` takes cues, `Centroid` takes whatever labels it
was fitted on. The value exists and the wiring does not.

None of the three is fixed here. The lane was to find out what a caller
has to work around, and quietly repairing them would have hidden the
answer — they are filed, and the router keeps its workarounds visible
so the next reader can see the shape of what is missing.

## Results — intent-frame-typed-values (2026-09-04)

The blocker the first caller found, and the one that made "a label
cannot be acted on; a filled FRAME can" untrue in the code that
promised it. `Slot[A]` knew its type, parsed an answer to prove it was
acceptable, and then `Frame` stored the raw TEXT — so `IntentRouter`,
having just established that "next thursday" was a date, got the string
back and parsed it a second time, with the same reference day, which
nothing in the type told it to remember.

`Frame` now keeps `Answered`: the slot, the text a person typed, and
the VALUE it parsed to. `valueOf` takes the SLOT rather than a name,
and that is the mechanism rather than a convenience — the slot is the
evidence that this answer has type `A`, so there is no way to ask for a
type the slot never had.

**On the one cast.** `valueOf` contains an `asInstanceOf`, and the
repository's rule is explicit that a cast needs a real necessity. This
is the shape the rule itself names as the exception — a heterogeneous
map keyed by identity — and it is isolated in one function whose guard
is what makes it true: the value is returned only when `a.slot eq s`,
so it was produced by THIS slot's own parser and by no other. That
guard is tested, not asserted: a second slot with the SAME NAME and a
different type is handed nothing.

`filled` survives as the text view, because a frame shown back to a
person should show what they typed rather than what it parsed to.

The caller's test that recorded this defect now pins the property
instead of the workaround — which is the shape a fixed friction should
leave behind.

## Spec — intent-cues-for-a-taxonomy (2026-09-04)

The second friction the first caller exposed. `Patterns.meeting` is a
`Vector[Cue]` whose `cls` fields are bare strings, so nothing connects
a cue set to the taxonomy it is supposed to decide. Two consequences,
both visible in `IntentRouter`:

1. A caller whose taxonomy is domain-bearing (`MeetingProposal`, not
   `Proposal`) writes a translation by hand.
2. That translation ends in `case _ =>`, so a cue class the author
   forgot — or a class added to the cue set later — is routed to
   whatever the fallthrough names, silently and forever.

The second is the real defect. The first is friction; the second is a
wrong answer that no test can see, because a total function over
strings has no hole to trip on.

### Interface

- [x] `Cues(taxon: Taxon, all: Vector[Cue])` — a cue set is a cue set
      TOGETHER WITH the taxonomy it decides. Constructed only through
      `Cues.of`, which returns `Left` naming every cue whose class the
      taxonomy does not hold.
- [x] `Cues.silent: Vector[String]` — the classes no cue can ever
      produce. Not an error: a tier that cannot reach a class is a
      fact worth being able to read, and for the canonical set it is
      empty.
- [x] `Cues.renamed(onto: Taxon, mapping: Map[String, String])` —
      `Either[String, Cues]`, and TOTAL in both directions: every
      class the cues use must appear as a key, and every value must be
      a class `onto` holds. Missing keys and unknown targets are both
      `Left`, which is precisely the `case _ =>` the router had.
- [x] `Patterns.score` / `Patterns.classify` take a `Cues`. So does
      `NoModel`.
- [x] `Patterns.canonical: Taxon` — the four names the shipped cue set
      speaks — and `Patterns.meeting: Cues` stated against it.

### Behavior

- [x] A cue naming a class outside the taxonomy fails construction,
      and the message names the class.
- [x] A rename that omits a source class fails, and the message names
      the omitted class — the router's silent fallthrough, turned into
      an error.
- [x] A rename whose target is not in the destination taxonomy fails.
- [x] A successful rename decides the same messages as the original,
      under the new names: same winner, same margin.
- [x] `IntentRouter` drops `canonicalToTaxonomy` and holds a renamed
      `Cues` instead, so its `taxonomy.has` filter has nothing left to
      catch.

### Decision — why not a type parameter

`Cues[I]` with the classes as a sum type was the first draft and is
wrong for the same reason `Taxon` is a value: the taxonomy a service
edits arrives as DATA, from a distilled corpus or a config file, and
cannot be a type. The check therefore happens at CONSTRUCTION, once,
and every use downstream is total — which is the same bargain
`Schema`-derived code makes and the one this module already took when
`Taxonomy[I]` was withdrawn.

## Results — intent-cues-for-a-taxonomy (2026-09-04)

Landed as specified, and the interesting part is what the change
DELETED rather than what it added.

`IntentRouter` lost two lines it should never have needed: the
`canonicalToTaxonomy` match and the `.filter(taxonomy.has)` that stood
downstream of it. The filter was there because the translation could
produce anything; the translation ended in `case _ =>` because a match
over strings has to. Neither is needed once the cue set carries the
taxonomy it decides — every class `Patterns.classify` can return is
one the taxonomy holds, by construction, and there is nothing left for
a filter to catch.

`renamed` is total in both directions, which is the difference from
what the router had. A `Map` passed where a `match` used to be would
have bought nothing: `mapping.getOrElse(cls, fallback)` is the same
silent fallthrough with different syntax. What makes it safe is the
requirement that every class the cues USE appears as a key — so the
mistake becomes a `Left` at construction, and the test that pins it
deletes exactly one entry from the router's own map and reads the
class name back out of the error.

Two smaller facts fell out of stating a set against a taxonomy:

- `silent` — the classes no cue can reach. Empty for the shipped set,
  which is worth having MEASURED rather than assumed; a cue set that
  cannot produce one of its own classes has a recall ceiling nobody
  would find by reading it.
- `Cues.unsafe`, used once, by `Patterns.meeting` itself. A set built
  in the same file as the taxonomy it is checked against cannot fail
  for a caller's reason, so an `Either` there would be an `Either`
  every caller unwraps to reach a constant. The check still runs, at
  class initialisation.

Gate: clean compile 0 warnings; okayIntent JVM+JS, okayDemo,
okayAgent — 296 tests, 0 failures.

The two SIGTERMs this lane hit were a sibling's sbt, not the repo's
own broken matrix: pid 56948 at `-Xmx6g` with load average 27, and the
same command passed at load 17 with nothing else running. Consistent
with the standing note that gates run on a quiet box, and no evidence
either way about the 1449-test kill on master.

## Spec — intent-slot-extraction (2026-09-04)

The end-to-end extractors, asked for by the operator, and the hole
between the classifier and the act.

A frame today can only be ASKED. `Frame.answer` takes a reply to a
question, so the only way a slot gets filled is that somebody asks
one — and the router therefore classifies "Are you free Wednesday
afternoon?" as a proposal and then asks "When would you like to
meet?" of the person who just said. Every part needed to do better
already exists: `Temporal.parse` scans a whole word list and finds the
date wherever in the sentence it sits. Nothing wires it to a slot.

So a slot may carry an EXTRACTOR, and a frame may be filled from the
message it arrived in.

### Interface

- [x] `Found[A](text: String, value: A)` — a value together with the
      SPAN of the message it came from. The span is not decoration: a
      value a person did not type has to be echoable ("Thursday 10
      Sep — right?"), and the whole message is not an echo.
- [x] `Slot[A].extract: String => Option[Found[A]]`, defaulting to
      "nothing found". A slot that cannot extract is the normal case
      and stays a two-line value.
- [x] `Frame.fillFrom(message)` — runs the extractor of every
      UNANSWERED slot and stores what it finds. Never overwrites an
      answer: a person's own reply outranks a guess about their
      earlier sentence.
- [x] `Temporal.find(message, today): Option[Found[When]]` — the value
      is `parse`'s own answer over the whole message, unchanged; the
      span is the SHORTEST window of words that yields that same
      value. Minimal evidence for the answer the parser already gave,
      rather than a second, differently-behaved parser.
- [x] `Slots.text(..., fromMessage = true)` — a text slot whose
      evidence is the whole message, for the frames where the request
      IS the message.
- [x] `IntentRouter` fills before it asks.

### Behavior

- [x] "Are you free Wednesday afternoon?" fills `when` and the router
      ACTS instead of asking.
- [x] The evidence span is the date phrase, not the sentence.
- [x] "Shall we meet?" still asks — nothing was said, nothing is
      invented.
- [x] A slot already answered by a person is not overwritten by
      extraction.
- [x] Extraction and asking agree: a value extracted from a message
      equals the value obtained by asking and being told the same
      phrase.

### Decision — the value is `parse`'s, the span is the minimum

Two ways to find a span, and only one of them keeps the semantics.
Sliding a window and taking the FIRST or LONGEST window that parses
makes a new parser with new answers — a window can parse to something
the whole sentence would not. Taking `parse`'s answer over the whole
message first, and then searching for the shortest window that
reproduces it, cannot change any answer: the extractor agrees with the
parser by construction, and only the evidence is searched for. The
cost is O(n²) parses of short strings on a message that is already
being classified, which is not the expensive thing on this path.

### Known limit, filed rather than hidden

`Temporal` is English. Extraction over the six-language fixture will
therefore fill English rows and decline every other language, and the
router degrades to asking — in the reader's own language, which it
already does. The number goes in the Results, and the lane to fix it
is `intent-temporal-multilingual`.

## Results — intent-slot-extraction (2026-09-04)

The router now ACTS on messages it used to ask about, and the two
rewritten tests are the whole result: "Could you send me the agenda?"
used to end in "What would you like done?" — asked of someone who had
just said — and "Shall we meet on Tuesday?" used to end in "When would
you like to meet?". Both are actions now, and the second carries
`2026-09-08` as a `Temporal.When`, with `Tuesday` kept beside it as
the words it rests on.

**The minimal span is shorter than the phrase a person would quote,
and that is correct.** "Shall we meet next thursday at 2pm about the
roadmap?" yields `thursday at 2pm`, not `next thursday at 2pm`,
because a bare weekday already resolves to the coming one — the
shorter window reproduces the same `When`, so it is the minimum
evidence for the answer. The test asserts the short form and says why.

**A design fault this lane exposed in the previous one.** `valueOf`
identifies a slot by IDENTITY, which is what makes its cast true — and
`IntentRouter` built `Slots.when(today)` INSIDE `frameFor`, where no
caller could reach it. The typed value was therefore unreachable
through the very door the last lane opened: `Frame.slots` hands back
`Slot[?]`, and a wildcard cannot be asked for a type. The fix is
caller-side and is the pattern to document: slots are HELD AS VALUES,
in a `Meeting(today)` the caller keeps, and `route` takes that value
instead of a bare date. Found by writing a test with `private def when`
instead of `private val when` and watching it return `None`.

### Coverage, per language

Five of the thirty parallel meanings carry a date in their English
reading. Extraction finds all five in English and none in the other
five languages:

| lang | found | of |
|------|-------|----|
| en   | 5     | 5  |
| fr   | 0     | 5  |
| de   | 0     | 5  |
| es   | 0     | 5  |
| ru   | 0     | 5  |
| ja   | 0     | 5  |

That is `Temporal` being English, stated as a measured number in a
test that prints it rather than as a comment claiming it. The router
degrades the right way — it asks, in the reader's own language, which
it already did — and `intent-temporal-multilingual` is the lane.

### Decision — no `lang` on `fillFrom`

`missing` takes a language because asking is addressed to a reader.
`fillFrom` does not, because extraction reads what is in front of it
and today's only extractor is English. Adding the parameter now would
put an argument in the signature that every implementation ignores,
which is how a signature starts lying; when `Temporal` learns a second
language the parameter arrives with a reader.

## Results — conversation-over-frame (2026-09-04)

`Slot`, `Found`, `Answered` and `Frame` moved out of this module into
`okay-frame`, and the reason is a consumer's: `okay.agent.Conversation`
had grown its own slot model on the same day, and two in one
repository is one too many. See `specs/conversation.md` for the merge.

What stays here is what is about INTENTS rather than about forms:
`Slots` (the parsers this programme has — `when` is `Temporal` wearing
the descriptor), and every tier. `okay-intent` depends on `okay-frame`;
`Frame` is imported rather than defined.

Two changes to the shape this spec described:

- `Frame` carries the LANGUAGE of the exchange. `missing`, `complete`
  and `answer` no longer take one. The router therefore takes the
  language where it takes the day — `Meeting(today, lang)` — which is
  the right place for it: a language is a property of a conversation,
  not of a call.
- `Frame.take(name, text)` is the dialogue's door: it answers the
  named slot and lets the same sentence fill whatever else it can.
  `answer` remains for a caller that wants the strict `Either`.

## Results — intent-fitted-model-ships (2026-09-04)

Nine tiers measured, none shipped. `Models.meeting` is the first
fitted model in this repository that a caller can load, and `Fit` is
the door from a corpus to a model and back.

**Which tier can ship, and why only one.** The vector tiers need an
embedder — a gateway on the network, or a distilled table somebody has
to build first — so shipping one ships a dependency, not a model. The
cue tier ships already but is not fitted. `CharGrams` is the tier in
between: hashed character n-grams over the text, no embedder at all.

**The dimension was chosen by measurement, against the module's own
default.** `CharGrams.train` defaults to 4096; the shipped model uses
1024. On held-out English: 61.7% at 1024, 63.3% at 4096, 58.3% at
8192 — more dimensions stop helping at this corpus size, and 1024
serialises to 42KB against 170KB. `Fit.grams` carries 1024 as its
default and says why. (Superseded 2026-09-07 by the operator's call,
intent-shipped-model-4096 below: 4096 buckets and a 2–3 window, once
the window sweep had measured what that buys.)

**What it delivers, which is the number that justifies the artifact.**
Alone it is 61.7% and would not be worth shipping. Behind the cue
tier, at FULL COVERAGE on 60 held-out English messages: **76.7%**, with
no network, no gateway and no fitting on the startup path. The cues
answer the 53% they fire on at 90.6%; the model answers the remaining
28 messages at 61%. Both halves of that split are asserted in the
test, so the doc comment cannot drift from the code.

**What I decided NOT to ship, and why.** A fit over all six languages
of the fixture. Per-language held-out, fifteen rows each: fr 53-67%,
de 40-47%, es 33-53%, ru 33-40%, ja 53-60% — noise-dominated at that
size, and it costs English three points (73.3% composite against
76.7%). `CharGrams` is language-agnostic BY CONSTRUCTION and this
fixture cannot demonstrate it; `intent-language-fixture-growth` is the
lane that would.

**The artifact is reproducible, not a blob.** `MakeModel` writes it,
and a test asserts that what is committed is byte-for-byte what the
generator produces from the same corpus. `CharGrams.train` is
deterministic — weights start at zero, rows are walked in order — so
that test is a real check rather than a hope. If the fixture moves,
the test fails and names the fix.

**A generated SOURCE rather than a classpath resource**, because
`okay-intent` is cross-built and a resource is a JVM-only way to load
a model into a module whose whole claim is that it needs nothing at
runtime.

**And a gate correction found while proving that.** `TestModelsCross`
was written to run on JS, so I ran `okayIntentJS/test` to check — it
finished in one second having run ZERO tests. This module's
`.jsSettings` point the JS test scope at `src/test/scala-cross`, which
did not exist ("there is none yet", said the comment), so every gate
today that reported "okayIntent JVM+JS" was reporting a JS COMPILE and
no JS tests. The changelog entries for `intent-frame-typed-values`,
`intent-cues-for-a-taxonomy`, `intent-slot-extraction` and
`conversation-over-frame` say JVM+JS and meant it about okay-agent,
which does have cross suites, but overstated it about okay-intent.
`scala-cross` is now non-empty and on both platforms' test paths:
`TestModelsCross`, four tests, passing on JS.

**One door removed on review.** The first draft of `Fit` had
`centroid(rows)` and `probe(rows)` wrappers for fitting. They renamed
`Centroid.train` and `Probe.train` without adding anything, so they
are gone; `Fit` keeps only what was actually missing — the measured
default for `grams`, and writing a model down.

## Results — intent-one-entry-point (2026-09-04)

The composition lived in `okay.demo.IntentRouter` and now lives in
`okay.intent.Router`, with the demo as its caller. The demo file got
SHORTER, which is the shape a correct extraction leaves behind: what
stayed there is what a caller actually owns — its taxonomy, the names
it uses, the frames its classes need, the day the conversation is
happening.

`Router.of` checks that every tier speaks the taxonomy. `Action` keeps
the demo's four outcomes, with one addition that came from the other
consumer's ask the same day: `Ask` carries how many questions remain.

### The floor I chose twice, and the second measurement that decided it

First measurement, on held-out English: raising the last tier's floor
from 0.0 to 0.5 lifts precision among answered messages from 76.7% to
83.7% while coverage falls from 60/60 to 43/60 — four tenths of a
point per abstention. That argued for a floor of 0, and I set it.

That broke a demo test which had asserted that "zzz qqq xxx" escalates
to a person, and the break was the useful part: the held-out set is
all IN-DOMAIN, so it cannot measure the case a floor exists for. So I
measured that case.

| | median margin | range |
|---|---|---|
| real held-out English | 0.434 | 0.008 - 0.994 |
| nonsense ("asdf", "qwerty uiop", "zzz qqq xxx") | 0.437 | 0.131 - 0.893 |

The model is exactly as confident about garbage as about English. NO
THRESHOLD SEPARATES THEM, so a non-zero floor buys the look of caution
and none of it. The default stays 0 and the doc comment now says why
with both numbers.

What replaces the property: a caller chooses whether to load the last
tier at all. With it, coverage; without it, the tier below is a
person. The demo's test now pins BOTH — one that nonsense gets a class
when the model is loaded, named so nobody mistakes it for an
endorsement, and one that a router built without the model still
escalates.

Calibrated abstention already exists and is not a margin: `NoModel`'s
conformal threshold, with a promise attached that is `None` when the
sample cannot support it.

### CharGrams.renamed

The demo's taxonomy is domain-bearing and the shipped model speaks
canonical names, so without this the reference caller could not use
the model it ships. It obeys `Cues.renamed`'s rule exactly — total in
both directions, a partial map is an error rather than a silent
bucket — and touches only the labels, not the weights.

## Spec — intent-second-author (2026-09-04)

Every accuracy in this file was measured on a corpus written by one
hand. Until this evening that was a caveat; now it is load-bearing,
because `Models.meeting` SHIPS and quotes 76.7% in a doc comment, a
module page and a changelog entry.

A consumer demonstrated the failure four times today, most sharply
this evening: leave-one-out said 98.4%, and three of twelve real
answers written AFTER their corpus were taken by the wrong class —
while LOO did not move when they fixed it (98.4 to 98.2). Their
sentence is the one this lane is about: *if your bake-off corpora are
authored the same way, the number they report about themselves is not
the number that decides anything.*

I cannot fix that by writing more messages. I am the same hand, and a
second corpus by the same author measures the same thing twice. What I
can do is measure the GAP, in two ways that do not need new data.

### 1. Where does the score live?

- [x] For each held-out message, its nearest neighbour in the training
      half by character-trigram Jaccard.
- [x] Accuracy stratified by that distance. If the composite's 76.7%
      is carried by the near half and collapses on the far half, the
      corpus is scoring itself and the honest number for an unseen
      message is the far-half one.

### 2. What does a register shift cost?

Perturbations applied to the held-out set, MECHANICALLY — a
transformation cannot be authored to flatter the model, and a
different author differs at least this much:

- [x] `lower` — lowercased, final punctuation dropped
- [x] `hedge` — a hedge in front ("hmm, ", "so ", "quick one — ")
- [x] `tail` — a trailing clause (", if that works", " — no rush")
- [x] `typo` — one deterministic transposition in the longest word
- [x] `blunt` — the politeness frame removed ("Could you please X" ->
      "X"), which for a Request deletes the very cue the tier fires
      on. Reported SEPARATELY, because it is not a register shift so
      much as a test of what the cue tier is really keyed to.

Measured through `Router.offline()` — the door a caller actually
gets — reporting coverage and accuracy per perturbation.

### 3. Then rewrite the numbers

- [x] Every load-bearing quote of 76.7% gets whatever this finds
      beside it, or gets replaced. A number that only holds for
      messages of the same register as its training data must say so
      where it is quoted, not in a spec nobody reads before calling
      `route`.

### What this is not

A perturbed corpus is a LOWER BOUND on the gap, not the gap. A real
second author differs in vocabulary, length, structure and intent
distribution all at once, and none of that is here. If the drop is
already large under a mechanical shift, the real one is larger.

## Results — intent-second-author (2026-09-04)

**The corpus is not scoring itself, and the number still moves ten
points.** Both halves of that sentence matter.

Nearest-training similarity across the held-out half has a median of
0.152 by character-trigram Jaccard and a MAXIMUM of 0.328 — there are
no near-duplicates, so the 76.7% is not the fixture recognising its
own sentences. But split that same held-out set at the median:

| | composite |
|---|---|
| the 30 least like anything trained on | 66.7% |
| the 30 most like something trained on | 86.7% |

Twenty points between "somewhat familiar" and "less familiar" INSIDE
one author's corpus. A different author is further out than the far
half is.

**And the mechanical shifts, through `Router.offline()`:**

| shift | composite | cues fired / right | model alone |
|---|---|---|---|
| as written | 76.7% | 32 / 29 | 61.7% |
| lowercased | 76.7% | 32 / 29 | 60.0% |
| hedge in front | 76.7% | 32 / 29 | 61.7% |
| trailing clause | 73.3% | 32 / 29 | 55.0% |
| one typo | 66.7% | 27 / 23 | 55.0% |
| politeness removed | 65.0% | 19 / 19 | 55.0% |

Three findings in that table, and two of them are about tiers rather
than about the corpus.

1. **The cue tier trades recall, never precision.** Strip "Could you
   please" and it fires on 19 of 60 instead of 32 — and is right about
   ALL NINETEEN. A syntactic frame is a high-precision, low-recall
   signal, which is the argument for putting it first and for never
   letting it be the only tier.
2. **Character n-grams are not typo-robust here, though that is the
   usual argument for them.** One transposition in the longest word
   takes the model from 61.7% to 55.0%. At 60 training messages the
   hashed 3-5-grams are too sparse for the redundancy that virtue
   depends on. Filed as `intent-typo-robustness`; the fix is more
   data or a smaller n, and both are measurable.
3. **Casing and a hedge in front cost nothing**, which is worth
   knowing before someone normalises input that did not need it.

**What changed because of this.** `Models`, `Router` and
`docs/modules/okay-intent.md` now quote 65-70% as what to expect from
a message somebody else wrote, with 76.7% named as what the model
scores on prose of its own register. The changelog entries for
`intent-fitted-model-ships` and `intent-one-entry-point` quoted the
bare 76.7%; the entry for this lane names them.

**What this still is not.** A perturbed corpus is a LOWER BOUND. A
real second author differs in vocabulary, length, structure and the
distribution of intents all at once. The lane that would settle it is
still open, and it needs a corpus nobody wrote for this repository.

## Results — intent-per-class-not-aggregate (2026-09-05)

A consumer's finding, arriving from their own corpus rather than from
this one: they filled a hole, one class reached 137 of 184 rows, a
probe leaned to the majority, "сегодня в москве шёл дождь" came back
as a REQUEST at 0.90 — and their headline accuracy ROSE, 95.8% to
96.2%, through the regression. On an imbalanced corpus accuracy
rewards predicting the biggest class. A regression test caught it; the
average could not.

Every aggregate this module publishes had the same exposure, starting
with the 76.7% now in a doc comment, a module page and two changelog
entries. `Eval` gained the three numbers that close it — `support`,
`balance`, `majorityBaseline` on the matrix, and `worst` on the report
— and the shipped model's tests now print per class and ASSERT rather
than describe.

**The good half.** The held-out set is 15 messages of each class, so
the majority baseline is 25% and the 76.7% is not being carried by one
class. That is now asserted (`majorityBaseline < 0.40`), so a fixture
that drifts into imbalance fails a test instead of quietly inflating a
number.

**The half the aggregate was hiding.**

| class | precision | recall | F1 |
|---|---|---|---|
| `Proposal` | 0.87 | 0.87 | 0.87 |
| `Request` | 0.70 | 0.93 | 0.80 |
| `Notification` | 0.75 | 0.80 | 0.77 |
| `Other` | 0.78 | **0.47** | 0.58 |

`Other` misses more than half the messages that are not about
meetings. In production that is the worst class to be weak at:
out-of-domain traffic is routed INTO a meeting intent rather than out
of the way, and a caller reading 76.7% would never have guessed it.
The cue tier, separately, is right about every `Other` it fires on
(precision 1.00) and fires on half of them — so the recall is lost in
the model tier, which matches what `intent-split-other` already said
about a diffuse bin and now attaches a number to it.

A per-class floor (`F1 >= 0.50`) is asserted alongside, so a class
dying fails the suite. That is the shape of the consumer's regression
test, in a repository whose tables report means.

## Results — intent-uk-pl-rows (2026-09-05)

Ukrainian and Polish, owed to a consumer who runs in both and asked
twice. Thirty parallel meanings each, so the fixture is eight
languages wide.

**Stated first, because it is the same disease this repository
measured yesterday: these rows are MY writing.** A native speaker
should read them before anything is claimed from them. What makes them
worth adding anyway is that a fixture row is CORRECTABLE — the
consumer speaks both languages and can fix a wording — and that the
numbers below are diagnostic even where the prose is imperfect. The
suite prints every Slavic row the tier gets wrong, so the first thing
a native reader sees is the list to check.

### The shipped model is English, and now it is measured rather than said

| language | shipped model | cue tier fired on |
|---|---|---|
| en | 76.7% | 24 of 30 |
| fr | 26.7% | 0 |
| de | 30.0% | 0 |
| es | 23.3% | 0 |
| ru | 30.0% | 0 |
| ja | 23.3% | 0 |
| uk | 26.7% | 0 |
| pl | 26.7% | 0 |

Four classes, so 25% is chance. Everything but English is AT chance,
and the cue tier — the half that carries the composite — fires zero
times outside English because its cues are English phrases. Passing a
Ukrainian message to `Router.offline()` is a coin flip with a
confident face, and `Models` now says so in those words.

### Fitting on all eight does not rescue it either

Fifteen held-out rows per language, trained on the other fifteen plus
the English corpus:

| en | pl | fr | de | es | ja | ru | uk |
|---|---|---|---|---|---|---|---|
| 86.7% | 46.7% | 53.3% | 46.7% | 40.0% | 40.0% | 33.3% | 33.3% |

And in EVERY non-English language at least one class scores F1 0.00 —
a class the tier never once produces. `CharGrams` is language-agnostic
by construction, which is true and is not enough: the construction is
free, the rows are not, and fifteen a language is not rows.

Polish (46.7%) over Ukrainian (33.3%) is fifteen rows against fifteen
rows and should not be read as Latin script beating Cyrillic. It is
noise until somebody grows the corpus, which is
`intent-language-fixture-growth`.

### What a consumer gets out of this

Two languages they can correct, a printed list of the eighteen Slavic
rows the tier currently misreads, and a number that says plainly: do
not ship the English artifact into a Ukrainian or Polish product. Fit
your own with `Fit.grams(rows)` — the door exists precisely because
this one cannot be the answer.

## Results — intent-split-other (2026-09-05) — MEASURED AND DECLINED

`intent-other-is-a-bin` closed two remedies for `Other` and left one
open: give its members NAMES they can be learned under. This is that
one, measured, and it loses badly enough to close the question at this
corpus size.

Three groups, derived rather than relabelled so every published number
stays comparable — `Social` (a pleasantry or personal news, no action
wanted), `Support` (something is wrong with a product or service),
`Errand` (a real request or question, out of domain).

### The result

| | accuracy | `Other` recall |
|---|---|---|
| unsplit, model alone | 61.7% | 46.7% |
| split three ways, folded back | 55.0% | **6.7%** |
| unsplit, shipped composite | 76.7% | 46.7% |
| split three ways, composite | 75.0% | **26.7%** |

And on the split taxonomy itself, `Social`, `Support` and `Errand` all
score **F1 0.00** — the model never predicts any of them, not once.

### Why, and it is not about the carving

| | rows in the training half |
|---|---|
| unsplit | Proposal 15, Request 15, Notification 15, Other 15 |
| split | Proposal 15, Request 15, Notification 15, **Social 6, Errand 5, Support 4** |

The odd/even split leaves FIFTEEN `Other` rows to train on, and
carving them three ways leaves four to six per class. Nothing is
learnable from four rows, so the classifier stops emitting those
labels entirely and every message that was going to `Other` goes to a
meeting class instead — which is exactly the production failure the
lane set out to fix, made four times worse.

A coarser two-way carve (`Social` 6 against `Trouble` 9) was measured
in case three names were simply too many: 55.0% accuracy, `Other`
recall 13.3%. It does emit both new labels, and it still collapses.
So the finding is about ROW COUNT, not about where the line is drawn.

### What this closes, and what it leaves

Three remedies for `Other` have now been measured and all three lost:
abstention instead of learning it (−20 points, `intent-other-is-a-bin`),
splitting it into names (recall 46.7% → 6.7% here), and doing nothing,
which is the status quo and the best of the three.

`Other`'s F1 of 0.58 is also not an outlier — `Notification` is 0.77,
`Request` 0.80, and the whole model is weak at 60 training rows. What
makes `Other` the one to worry about is not that it is worse but WHERE
it fails: under-prediction sends out-of-domain traffic INTO a meeting
intent.

So the remaining answer is rows, which is the second lane today to
arrive there — the eight-language measurement reached the same
conclusion from the other side. Filed as `intent-other-more-rows`.

The derived view and this suite stay in the repository: the map is a
usable artefact, and the test is the guard that will say so if the
answer ever changes.

## Results — intent-slavic-collision (2026-09-05)

A native reader read all sixty of my Ukrainian and Polish rows. Every
correction they gave is applied. The experiment their second finding
implied was run, and IT DID NOT CONFIRM THE HYPOTHESIS — including
mine.

### What they found, and what happened to it

**1. A parallel fixture propagates a defect across every arm at
once.** Their earlier Russian corrections went into the Russian arm;
the same calques stayed in Ukrainian and Polish, because every arm is
a translation of one English sentence. Fixed as given, and their rule
— *review a row, not a language* — is now a test rather than a habit:
`TestFixtureHygiene` flags near-twin rows within a class and language,
which is the mechanical form of what they caught by reading.

It caught six pairs immediately, in en, de, es, ja and uk — INCLUDING
ONE I HAD JUST CREATED. My fix for their collision finding rewrote a
Ukrainian Request into "Перевірте, будь ласка, чи вільна кімната",
which is a twin of "Заброньте, будь ласка, кімнату на чотирьох". The
guard caught it in the same run that introduced it.

**2. The person-marker collision.** uk "Можемо зустрітися" (Proposal)
against "Можете перевірити" (Request), pl "Czy możemy" against "Czy
możesz" — one or two letters apart in the morpheme that carries the
class.

Whole-sentence similarity does not find this: those pairs score 0.047
and 0.121 by trigram Jaccard over the message. The signal is in the
OPENING WORD alone, so the guard measures edit distance between
openings of different classes and nothing else.

**And it generalises past the two languages they were reading**, which
is the part neither of us expected:

| | | |
|---|---|---|
| en | `would` (Proposal) vs `could` (Request) | 1 edit |
| de | `können` vs `könnten` | 1 edit |
| pl | `może` vs `moje` | 1 edit |
| uk | `може` vs `моє`, `давайте` vs `додайте` | 2 edits |

`would`/`could` is how English marks the distinction and
`können`/`könnten` is how German does. A fixture that avoided them
would be LESS like the language, not more — so the collision list is
printed as a diagnostic of where the tier is structurally blind, and
is not asserted against. In English, messages opening on a modal score
**54.5% against 63.3%** for everything else. Eleven rows, so
directional rather than decisive, and the errors are the predicted
ones: Proposal↔Request↔Other.

**3 and 4.** Three Polish rows addressed only a man (`mógłbyś`,
`obecny`, `pytałeś`) — fixed, and it is the first evidence for
`frame-language-with-grammatical-gender`, which I filed from their
earlier message as a case I did not have and which turned out to be in
my own fixture. One grammatical error and two register slips fixed as
given.

### The experiment, which failed to confirm

The hypothesis was mine as much as theirs: fix the collision and the
Slavic numbers move without adding a row. They did not.

| | en | fr | de | es | ru | ja | uk | pl |
|---|---|---|---|---|---|---|---|---|
| before | 86.7 | 53.3 | 46.7 | 40.0 | 33.3 | 40.0 | 33.3 | 46.7 |
| after | 86.7 | 66.7 | 40.0 | 26.7 | 40.0 | 46.7 | **33.3** | **26.7** |

Ukrainian is unchanged and Polish is thirteen points WORSE. And the
languages I did not touch scatter by the same ±13, which is the
finding inside the finding: fifteen held-out rows means one message is
6.7 points, so this instrument cannot resolve an effect of the size
being looked for. Every number in that table moved by one or two
messages.

So this morning's conclusion stands, unweakened and now supported from
a fifth direction: the binding constraint is corpus size, and it binds
the MEASUREMENT as well as the model. The reader's mechanism is real
— it is visible in the openings, and English pays for it too — but its
cost cannot be measured on fifteen rows a language, and neither can
its repair.

## Results — intent-taxon-wired-to-tiers (2026-09-05)

The last of the three frictions the first caller exposed, parked twice
for other work. Request 1 asked for one taxonomy both tiers read; what
had landed was one taxonomy NEITHER TRAINED TIER read — every fit
inferred its classes from whatever labels its rows happened to carry,
and a caller checked agreement by hand or not at all.

Every `Trained` now carries the `Taxon` it was fitted against.
`train` still infers it, because that is the ordinary case and no
caller should have to declare a taxonomy to fit two classes; `against
(taxon, rows)` DECLARES one and refuses a label outside it, at fit
time rather than as an invented class in a confusion matrix later.
`silent` names the classes a declared taxonomy holds that the rows
never taught — a tier that cannot reach a class is a fact worth
reading before trusting its recall, the same shape `Cues.silent` has.

### The latent bug, which is why this was not tidiness

`NoModel.blend` adds a cue's weight to a probe class BY STRING
EQUALITY. Cues speaking `Proposal` against a probe fitted on
`MeetingProposal` therefore never match: every bonus is zero, the
ensemble silently degrades to the plain probe, and nothing anywhere
says so. No error, no warning, and an accuracy that looks entirely
plausible — it would simply be the probe's.

That is the same defect `Cues.renamed` was built to end one layer up,
still reachable at this layer. `NoModel.fit` now refuses it, and the
check is ONE WAY on purpose: every class the PROBE knows must be in
the cues' taxonomy, because those are the classes a bonus could ever
apply to. A cue class the probe has not learned is legal — a cue set
may cover a class the corpus does not yet.

### What is not persisted, and why

`Fitted` does not write the taxonomy. On the way back in it is
inferred from the classes the model actually learned, so the wire
format is unchanged and no file written by an earlier build stops
decoding. The cost is stated where it can be read: a taxonomy DECLARED
with a class no row ever taught comes back without it, and `silent` is
empty after a round trip. Fit-time knowledge, not model knowledge.

### And a default that was lying

Enforcing the check broke ten existing tests, all of them fitting a
probe on abstract labels ("A"/"B", "north"/"east") while inheriting
`fit`'s default cue set. The guard was right that the taxonomies
disagreed and wrong to forbid it — "no cue tier, just the calibrated
probe" is a real configuration.

The default was the actual defect. `cues` defaulted to
`Patterns.meeting`: a MEETING cue set attached to every fit whatever
the corpus was about, contributing nothing because the default weight
grid is a single zero. A default that is inert is a default that lies
about what it does. It is `Option[Patterns.Cues] = None` now — a
caller who wants the blend names the cues, and only then must the
taxonomies agree. Behaviourally a no-op for every existing caller,
since the inert default contributed nothing to begin with.

### What this does not do

It does not make a mismatch impossible between a fitted tier and the
router that composes it — `Router.of` already checks that, at the
door. What changes is that the check is now possible one level down,
where the tier itself knows what it was fitted against, and that the
one place a mismatch could still go unnoticed is closed.

## Results — intent-english-corpus-twins (2026-09-05)

The twin guard, pointed at the corpus it had not been pointed at:
`labelled`, which the shipped model is fitted on and against which
every number this module publishes is measured. It found three pairs.

    [Proposal]     "Suggestion: we meet on Monday at 9."
                || "Suggest we cancel Monday and meet Wednesday instead."
    [Request]      "Send me the agenda when you get a chance."
                || "Kindly send me the agenda."
    [Notification] "FYI the meeting room has been changed to B2."
                || "The meeting has moved to the other building."

Each second member is rewritten to say the same thing a different way,
and the guard is an ASSERTION on `labelled` now rather than a report.

### What it cost, which is the point of the lane

| | before | after |
|---|---|---|
| composite, held-out English | 76.7% | **75.0%** |
| model alone | 61.7% | 61.7% |
| macro F1 | 0.756 | 0.740 |
| `Notification` F1 | 0.77 | 0.73 |
| `Other` F1 | 0.58 | 0.56 |
| near half of held-out | 86.7% | 83.3% |
| far half | 66.7% | 66.7% |
| one typo | 66.7% | 63.3% |
| politeness removed | 65.0% | 63.3% |

The headline drops 1.7 points — ONE MESSAGE — and every place it was
quoted is corrected: the `Models` doc comment, the module page, the
`Router` doc, and the two tests that assert it. The old number was
very slightly inflated by template redundancy, which is the direction
the theory predicts, and one message is not evidence for the size of
the effect, only for its sign.

The NEAR half fell (86.7 to 83.3) and the FAR half did not move at
all. That is the shape a duplicate-removal should have: the messages
that were scoring well because something like them was in training are
the ones that lose.

"Expect 63-67% from a message somebody else wrote" replaces "65-70%"
wherever it was published.

### A number that went UP, and is not evidence of anything

Under the "hedge in front" shift the composite now reads 78.3% —
higher than the unshifted 75.0%. That is one message, in the
direction that makes no sense, and it is the clearest statement
available of what sixty rows can resolve: ±1.7 points is the
instrument's floor, and every difference smaller than that in this
spec should be read as zero.

### What is superseded

`intent-fitted-model-ships`, `intent-one-entry-point`,
`intent-per-class-not-aggregate` and `intent-second-author` quote the
old numbers. They are correct for the corpus they were measured on and
the changelog entry for this lane names them; nothing was quietly
edited in the history.

## Results — intent-multi-intent-measured (2026-09-05)

`Span` has been in this programme's types since its first lane and was
the argument for why a flat label is not enough — "charged twice AND
the app crashes is two spans, both to be acted on". The fixture
contained no message with two intents, so the sentence had never met
one. Twelve now do, and the claim is measured on both sides.

### What the shipped path does, which is nothing

Every tier that ships returns a single best class, so a two-intent
message gets one label BY CONSTRUCTION. On the twelve:

- all twelve answered
- the answer matched the FIRST intent 3 times and EITHER intent 10
- the other intent is dropped with no trace in the `Action`

The one place it survives: the cue tier ranks what it fired on, and
its RUNNER-UP is the second gold intent in **5 of 12**. `Router` throws
that away — `Action.Act` carries a winner and nothing else. So the
signal exists, is measured, and is discarded at the door.

The module's documentation has read as though multi-intent were a
property of the module. It is a property of ONE tier, and that is now
said where it is claimed.

### What the model tier does, which is half of what the type promises

Live, against the local 4B, `Classify.prompt` on the same twelve:

| | |
|---|---|
| our decoder read the answer | 12 / 12 |
| two spans came back | **6 / 12** |
| the right SET of intents | 5 / 12 |
| the right set AND the right order | 4 / 12 |
| every span's text was IN the message | 12 / 12 |

Half the time it collapses to one span, and the intent it keeps is the
Request — "The room has moved to B2, could you tell the others?" comes
back as a Request alone, "I will be on leave, so please reassign my
reviews" likewise. The imperative half swallows the informative one.

So the claim is not false and is much weaker than the type implies:
the mechanism works, `decide` acts on two spans and stops the whole
message when one is unsure (both tested), and the model produces the
segmentation about half the time on this model at this size.

### Two things found while measuring

**Span texts are grounded.** Every span in every answer was a real
stretch of the message — 12/12 across two runs. That is a property
worth keeping and worth ASSERTING in a future decoder: a span whose
text is not in the message is not a segmentation, it is an invention.

**Nothing bounds the span count.** One run answered a nine-word
message with TWENTY spans, cycling `Notification+Request+Other+
Proposal` five times. It did not recur on the second run. Grounding
would not have caught it — the repeated texts were real substrings —
so the check that would is DISTINCTNESS: no two spans covering the
same stretch. Filed as `intent-span-runaway` rather than guessed at
here, with the observed shape recorded.

## Results — intent-jmh-row (2026-09-05)

This line quoted microseconds for every tier and each one was a
`System.nanoTime` around a loop inside a test: no warmup, no JIT
accounting, one run, on whatever the machine was doing. The repository
keeps `src/jmh/history.tsv` precisely so that a performance claim
means something, and by that standard none of these did. I had quoted
them myself.

JMH, 2 forks × (4 warmup + 6 measurement) × 1s, on a quiet box
announced in the room first:

| what a caller pays | µs/op | was quoted |
|---|---|---|
| cue tier, one message | **1.4 ±0.1** | 96 |
| `Router.offline().route`, a cue answers | **1.4 ±0.0** | — |
| `Router.offline().route`, no cue fires | **15.3 ±1.4** | — |
| centroid score (256-dim, no embedding) | **1.3 ±0.1** | 75-90 |
| probe score (256-dim, no embedding) | **1.7 ±0.1** | 76 |
| character n-grams, shipped model | **13.7 ±0.2** | 92 |
| kNN over 60 examples | **13.9 ±1.1** | 158 |
| **load the shipped model** | **58.9 ±1.1** | never measured |
| fit centroid, 60 rows | 64.4 ±3.9 | — |
| fit probe, 60 rows, 50 epochs | 3 743 ±194 | — |
| fit character n-grams, 60 rows, dim 1024 | 40 090 ±754 | 404 000 |

**Fifty to seventy times.** A cold loop in a test measures the JIT and
divides it by the iteration count; that is the whole gap, and it is
the reason this repository has a benchmark harness at all.

Two things the table would imply if left alone:

- The vector tiers score a 256-dimension synthetic vector and EXCLUDE
  the embedding call. For a real caller that call is a network hop
  and dominates everything here, so 1.3µs is the cost of the TIER and
  not of the answer.
- The door has two costs — 1.4µs when a cue fires, 15.3µs when none
  does and the model is consulted. Both are in the table because
  quoting one would be the same half-truth this lane exists to remove.

And the number nobody had ever asked for: **58.9µs to decode the
shipped model**, which is the whole of what `Models.meeting` costs a
startup path, once. `intent-fitted-model-ships` argued that fitting
must leave the startup path; this is what stayed there, and it is
nothing.


## Results — intent-tfidf-word-linear (2026-09-07)

**The classical baseline, run.** `WordTfIdf`: tokens are runs of
letters or digits in any script, lowercased; a vocabulary and a
smoothed IDF (`log((N + 1) / (df + 1)) + 1`) fitted on the training
half and carried with the model; the L2-normalised vector handed to
`Probe.train` as an embedding, so the descent is the one every tier
uses. It sits between BM25 and the character n-grams in what it sees,
and it was run to answer one question about the n-gram tier: is its
number about characters, or about having a linear model at all?

Same split as `TestCharGrams` (odd rows train, even rows test), same
session, the default gate, no network:

| | word TF-IDF + linear | character n-grams + linear |
|---|---|---|
| English, accuracy over ALL | **61.7%** | 65.0% |
| margin ≥ 0.3 (coverage / agreement) | 68.3% / 58.5% | 50.0% / 70.0% |
| margin ≥ 0.6 | 46.7% / 64.3% | 28.3% / 76.5% |
| per message / fit | 51 us / 66 ms, 303 words | 92 us / 367 ms |

Three points apart at full coverage, so **the n-gram tier's number is
the linear model's**, not the characters' — on English. (The 60.0%
the tables above record for chargrams was the fixture before
intent-language-fixture-growth; on today's it reads 65.0%, and the
comparison is between numbers from one run.) The n-grams' margin is
worth more: at 0.6 they answer fewer messages at higher agreement,
where the word model's margin barely sorts right from wrong.

Per language, one model trained on all six, fifteen held-out rows
each — thin, and read as direction only:

| | en | fr | de | es | ru | ja |
|---|---|---|---|---|---|---|
| word TF-IDF | 53.3 | **66.7** | 26.7 | 26.7 | 40.0 | 26.7 |
| character n-grams | 40.0 | 46.7 | **40.0** | 20.0 | **53.3** | **60.0** |

Characters win exactly where words are not the unit: Japanese has no
spaces, so a `\p{L}+` token is the whole clause and the vocabulary
never matches (26.7% is the four-class chance line); Russian inflects,
so `встретиться` and `встретимся` are two words to a vocabulary and
one window to an n-gram. Words win on French, where they are the unit
and the fixture's phrasing repeats. Neither is a tier to ship — the
static table (63.3%, 5 MB) and the teacher remain the no-network and
the accurate answers — but the question is answered, and it says the
zero-network path's ceiling is the representation, not the head.

## Results — intent-typo-robustness (2026-09-07)

**A typo's cost per tier, and the window that buys it back.** The
claim was that character n-grams survive a typo because a word's
other windows still match; on 2026-09-04 one transposition in the
longest word took the 3–5-gram model from 61.7% to 55.0%, and the
entry named two fixes to measure rather than assume: a smaller n, more
rows. The rows grew on their own (the same model reads 65.0% clean
today); this is the window. `TestTypoRobustness`, default gate, the
`TestCharGrams` split (60 train / 60 test), `TestSecondAuthor`'s
deterministic transposition, the word TF-IDF tier beside as the
control that should collapse:

| window | clean @4096 | typo @4096 | clean @1024 | typo @1024 |
|---|---|---|---|---|
| (3,5) — the default | 65.0% | 55.0% (−10.0) | 61.7% | 53.3% (−8.3) |
| (2,4) | 65.0% | 63.3% (−1.7) | 61.7% | 65.0% (+3.3) |
| (2,3) | **68.3%** | **66.7%** (−1.7) | 58.3% | 66.7% (+8.3) |
| (3,4) | 63.3% | 60.0% | 51.7% | 53.3% |
| (4,6) | 56.7% | 60.0% | 50.0% | 48.3% |
| word TF-IDF (control) | 61.7% | 56.7% (−5.0) | | |

Both hash widths, because they interact: 4096 is `CharGrams`' own
default, 1024 is `Fit`'s and the shipped model's (its (3,5) @1024
column IS `Models.meeting`: 61.7% alone). The diagnosis in the entry
holds: at sixty rows the 3–5-grams are too sparse for the redundancy
the argument depends on — a six-letter word has four 3-grams and a
transposition breaks three — while a 2–4 window keeps most of its
windows, reads the same clean at both widths, and holds under the
typo; 2–3 is better still at 4096. The control did what a vocabulary
must: a transposed word is one it has never seen, five points.

**And yet the default does not move, because a class does.** The
shipped model was refitted at (2,4) and at (2,3) and run through its
own laws (`TestModels`, `TestSecondAuthor`). The totals held or rose
— 61.7% alone at (2,4), 75.0% behind the cues, the typo composite
63.3 → 71.7% — and `Other` fell: recall 0.47 → 0.33, F1 0.56 → 0.45,
under the 0.50 floor the per-class law asserts exactly so that a
rising total cannot hide a dying class. `Other` is the class whose
under-prediction routes out-of-domain traffic INTO a meeting intent
(intent-other-more-rows), and fifteen rows of it are what a narrower
window has to learn from. So `CharGrams.train` keeps (3,5), the
artifact stays what it was measured as, and two things are filed:
the grid above against the per-class law at each width
(`intent-window-by-dim`), and the rows that would let the narrower
window keep `Other` (`intent-other-more-rows`, already open). The
word here is measured: a smaller n buys the typo back, and on this
corpus it pays for it with the one class that must not be lost.

## Results — intent-span-runaway (2026-09-07)

**The decoder-side guard.** intent-multi-intent-measured recorded one
live run answering a nine-word message with twenty spans, cycling
`Notification+Request+Other+Proposal` five times, and noted that
grounding would not have caught it: every span's text was a real
substring of the message. The check that does is distinctness — no
two spans over the same stretch — and it is a property of the
DECODED reading, so it belongs beside the decoder, not in the prompt.

`Reading.grounded(message)` keeps a span only if its text occurs in
the message (lowercased, whitespace flattened) and only if the stretch
it covers — its first occurrence — does not overlap a stretch already
taken by a span kept before it. The model's order is preserved, so
the first span over a stretch is the one that stays; a blank span has
no stretch and drops. `decide` is unchanged: confidence remains its
question, and the reading it sees is now bounded by the message's
length in stretches rather than by the model's appetite.

Laws (`TestClassify`, every platform): the twenty-span cycle over two
stretches collapses to two spans carrying the first label on each;
a span not in the message drops whatever its confidence; a grounded,
distinct reading comes back exactly as it was; overlap is judged on
the message's stretches, so `meet Tuesday` inside a kept `can we meet
tuesday` is the same stretch, and a bare `the` lands at its first
occurrence, inside a kept span. The live multi-intent suite applies
the guard before it counts and prints what it dropped, so the next
runaway is visible as a guard event rather than as twenty labels.

## Results — intent-window-by-dim (2026-09-07)

**The window against the per-class law, at both widths.** The open
question from intent-typo-robustness: a narrower window survives a
typo and takes `Other` under the 0.50 F1 floor at the shipped width —
does it keep `Other` at 4096? `TestWindowByDim` (default gate) runs
the per-class report for every (window, width) pair on the
`TestModels` held-out half, for the grams alone and for the composite
the law is stated on (cues first, grams for the rest), clean and
under one transposition. Composite rows; `Other` F1 is the floor's
class in every configuration:

| window @ width | clean total | Other F1 | typo total | Other F1 |
|---|---|---|---|---|
| (3,5) @1024 — the shipped model | 75.0% | 0.56 | 63.3% | 0.42 |
| (2,4) @1024 | 75.0% | 0.45 | 71.7% | 0.40 |
| (2,3) @1024 | 76.7% | 0.45 | 71.7% | 0.38 |
| (3,5) @4096 | 78.3% | 0.64 | 65.0% | 0.48 |
| (2,4) @4096 | 78.3% | 0.52 | 71.7% | 0.43 |
| **(2,3) @4096** | **80.0%** | **0.52** | **71.7%** | 0.43 |

Grams alone, the same pairs: (2,3) @4096 is the one configuration
whose `Other` holds under the typo as well (0.52 clean, 0.54 typo,
totals 68.3 / 66.7); every window at 1024 loses it clean or typo'd.

Three readings. **Yes, at 4096 the narrower windows keep every class
clean** — at 0.52, on the floor, where the shipped window has 0.64.
**The typo takes `Other` under the floor in every configuration**,
the shipped one included (0.42): fifteen rows of the class that
matters most cannot survive a transposition whatever the window, and
that is the rows' number (intent-other-more-rows), not the window's.
**The best configuration on the table is (2,3) @4096**: 80.0% behind
the cues against the shipped 75.0, 71.7% under the typo against 63.3,
`Other` on the floor clean — at four times the artifact, which is
the reason `Fit` chose 1024 (a quarter of the size for two points at
the time). Whether five points and eight under a typo are worth a
170 KB source file is the shipped model's owner's call, filed as
`intent-shipped-model-4096`; no default moves here.

## Results — intent-temporal-multilingual (2026-09-07)

**One meaning, eight wordings, one date.** `Temporal` parsed English
and declined the rest: 5/5 en and 0/5 in every other language of the
parallel fixture (intent-temporal-slots). It now carries a lexicon per
language and the same shapes as the English parser — weekday, month
and relative-day words matched as PREFIXES of a token, so an
inflection or a compound is the word it starts with (`Freitagvormittag`,
`четвергам`, `Jutrzejsza`, `щопонеділка`); the qualifier before or
after the weekday; `dans 3 jours` / `vor 3 Tagen` / `через 3 дня` /
`za 3 dni`; the next-week pair; `15h`, `15 Uhr`, `15時`, `M月D日` —
and Japanese as a string scan, since it has no spaces. English is
tried first and unchanged; a weekday beats the tomorrow-word (`el
viernes por la mañana` is Friday, `mañana` alone is tomorrow); the
qualifier is read from every lexicon at once, because ru `четверг` is
a prefix of uk `четверга` and `минулого` must still mean last.

The law is the fixture's own (`TestTemporalMultilingual`): a row that
carries a date in English carries THE SAME `When` — date and time —
in every other wording. Five dated rows, eight languages:

| | en | fr | de | es | ru | ja | uk | pl |
|---|---|---|---|---|---|---|---|---|
| dated rows agreeing with the English reading | 5/5 | 5/5 | 5/5 | 5/5 | 5/5 | 5/5 | 5/5 | 5/5 |

And the relative and counted forms per language against one Friday
(tomorrow, the day after, yesterday, next and last Thursday, in three
days, three days ago, next week; `3月14日`), and the four time
spellings, each asserted. `TestExtract`'s per-language table, which
used to be the number that said "English only", asserts full
coverage now. What no lexicon says is still `None` (`bientôt`,
`soon`): the parser declines as before, in eight languages instead of
one. A ninth is a lexicon.

Two wordings the fixture does not have taught the parser something on
the way: `15:00` was being split at the colon by the multilingual
tokeniser (fixed: the colon is a time), and a Cyrillic qualifier was
being read by the wrong lexicon (fixed: qualifiers are read from all
of them). Both are in the suite.

## Results — intent-extract-duration (2026-09-07)

**The second parsed slot.** `Temporal` settled what a slot with a
parser and an extractor is: total, deterministic, the value the whole
message's and the evidence the shortest window reproducing it.
`Duration` is that shape for "how long": minutes from a number and a
unit (`30 minutes`, `45 min`, `2 hours`, `1.5 h`, `90m`, `1h30`, `a
2-hour workshop`) or the spoken forms (`an hour`, `half an hour`, `a
quarter of an hour`, `an hour and a half`, `two hours`, `forty-five
minutes`); `None` for what it cannot read (`a while`, `all day`, `a
couple of hours`, a bare number, zero, more than a day). `Slots.
duration` asks in the six languages `when` asks in and shows a value
back as `1h30` / `2h` / `45min`; a `Proposal` frame of `when` and
`duration` fills both from "Can we meet next Tuesday for 30 minutes?"
with nothing left to ask. English phrases: the parallel fixture has no
duration rows, so the "one meaning, eight wordings" law that settled
`Temporal`'s languages has nothing to hold on to yet, and the other
languages' number-and-unit words are filed (intent-duration-
multilingual) with a fixture to be written first. `who`, places and
amounts remain on the more-slots entry; `who` is the one that is not
a parser.

## Results — intent-duration-multilingual (2026-09-07)

**Eight meanings, eight wordings each, one value.** `Duration` took
the lexicon shape `Temporal` settled: unit words matched as token
prefixes (`heures`, `Stunden`, `часа`, `godziny` are the words they
start with), number words with their genders and the one-and-a-half
words (`anderthalb`, `полтора`, `półtorej`), a ten and a unit read as
one number (`сорок пять`, `cuarenta y cinco` through its connector,
`czterdzieści pięć`), the fraction phrases (`une demi-heure`, `un
quart d'heure`, `eine Viertelstunde`, `media hora`, `четверть часа`,
`кwadrans`), and the half-hour that Romance puts after the hours (`et
demie`, `y media`) and Slavic before them (`с половиной`, `з
половиною`, `i pół`); Japanese as a string scan (`N時間`, `N時間半`,
`N時間M分`, `N分`, `半時間`). English first and unchanged; `None` for
what no lexicon says (`bientôt`, `eine Weile`, `un rato`, `скоро`,
`chwilę`, `しばらく`).

The parallel fixture carries no duration, so the fixture is in the
suite (`TestDurationMultilingual`): `30 minutes`, `an hour`, `half an
hour`, `an hour and a half`, `two hours`, `a quarter of an hour`,
`forty-five minutes`, `2.5 hours` — each in eight languages,
dictionary facts rather than corpus data — under the law that one
meaning has one value whatever the wording, and the phrases inside a
sentence in each language with the evidence as the phrase. A ninth
language is a lexicon.

## Results — intent-extract-people (2026-09-07)

**The third parsed slot, and the first with its law in the fixture
from the start.** `People.parse` reads a count beside a people-word —
`a room for four people`, `4 people`, `six of us`, `a team of 5`, `12
attendees`; `quatre personnes`, `vier Personen`, `cuatro personas`,
`5 человек`, `трое участников`, `6 осіб`, `dla czterech osób`, `trzy
osoby`, `4人用`, `6名` — and the Slavic collective numerals that count
people by themselves (`на четверых`, `на чотирьох`, `czterech` after
`dla`). A number with nothing to count is `None` (`for 4`, `room 4`,
`at 3pm`, `in 30 minutes`), as is a bare number word and any count
outside 1..1000. `People.find` is the evidence rule; `Slots.people`
asks in `when`'s six languages.

The parallel fixture's book-room row — "Could you book a room for four
people?" — carries the count in all eight languages, so the law that
took a fixture of its own for durations held here from the first
commit: four in every wording. A `Proposal` frame of when, duration
and people fills all three from "Can we meet next Tuesday for an hour,
six of us?" with nothing left to ask. The number words moved out of
`Duration` into `Numbers`, one place both parsers read; a fourth
counting parser is a people-word list and a call. On the more-slots
entry `who` and places remain, and neither is a parser: a name is
whatever the message says it is, and that is the model tier's job or
a person's.

## Results — intent-extract-amount (2026-09-07)

**The fourth parsed slot, the same two promises.** `Amount.parse`
reads a sum of money as `Amount(value: BigDecimal, currency)` with
the currency an ISO 4217 code: a number beside a currency, where the
currency is a symbol before or after (`$20`, `20$`, `€15.50`, `15,50
€`, `£30`, `¥500`, `3000円`, `20zł`, `30грн`), a code (`USD 20`, `20
USD`, `1000 PLN`), or the currency's NAME in the language as a token
prefix so inflections match (`dollars`, `euros`, `гривен`/`гривень`,
`рублей`/`руб`, `złotych`, `доларів`, `dolarów`, `ドル`). The number
is digits with `.` or `,` as either the decimal or the thousands
separator — told apart by what follows: both present, the last one
is the decimal point (`1,000.50`, `1.000,50`); one kind, more than
once or before exactly three digits it groups thousands (`1,000`,
`2.500`), otherwise it is the decimal (`20,5`); a thousands SPACE
(`1 000`, plain or narrow no-break) joins — or number words
composed: units add, hundreds and thousands multiply the group
(`two thousand five hundred`, `двести пятьдесят`, `sto dwadzieścia`,
`deux cents`, `mil`, `5k`, `1,5 тыс.`), the cardinal words from
`Numbers` plus the one-word hundreds each Slavic language has. The
number NEAREST the currency wins — the longest run of number words
ending just before it, else the run after it (a symbol or a code
before its number) — and the first amount in the sentence. A number
with nothing beside it (`20`, `room 20`, `20 minutes`), a currency
with no number (`dollars`), zero, and an ambiguous name are `None`:
`pound`, `livre` and `libra` are weights and books too (`20 pounds
of flour`, `j'ai lu 20 livres`), so pounds are `£`, `GBP`,
`sterling` and `quid` only — the honest cost of a parser that never
guesses. Fifty-eight shapes across the eight languages, the
negatives, the nearest-wins rule, `Amount.find`'s evidence window
(`200 dollars`, `$1,500`) and a frame that fills people and amount
from one sentence. `Slots.amount` asks in the six languages `when`
asks in and shows `15.5 EUR` (no trailing zeros). Still open here:
named entities (who) and places — neither a parser.

## Results — intent-static-trigrams-and-pca (2026-09-07)

The two extensions intent-static-embeddings filed rather than
guessed, measured in ONE run against the rozum gateway
(`Qwen3-Embedding-0.6B`, 1024 dims), one distillation of every word,
pair and triple in the fixture (2172 units, 2.9 s), the same odd/even
split for every row, the words+pairs baseline re-measured in the same
run so the comparison is like for like (its 61.7/53.3 here against
63.3/58.3 on 2026-09-04 is the split-and-session spread this fixture
shows, and the bar every gain below is held to):

| table | units | bytes | probe | centroid |
|---|---|---|---|---|
| words + pairs @ 1024 (baseline, same run) | 1317 | 5.3 MB | 61.7% | 53.3% |
| words + pairs + triples @ 1024 | 2172 | 8.7 MB | 66.7% | 65.0% |
| words + pairs @ PCA 256 (91.5% of the variance) | 1317 | 1.3 MB | 66.7% | 58.3% |
| words + pairs @ PCA 128 (78.5%) | 1317 | 0.66 MB | 65.0% | 58.3% |
| **words + pairs + triples @ PCA 256** | **2172** | **2.1 MB** | **68.3%** | **66.7%** |
| (teacher, live vectors) | — | — | 86.7% | 80.0% |

**Triples: the same argument once more, and it holds once more.**
+5.0 to the probe (at the spread's edge) and +11.7 to the centroid
(well past it) for 1.65x the units. `Static.units3` is the splitter;
`units` (pairs) stays for what was distilled with it.

**PCA: the cut is free, and then some.** model2vec's step — the
table's own unit vectors centred and projected onto their top-k
principal subspace (`Static.fitPca`, subspace iteration over the
covariance, seeded, 6.3 s for 1317x1024; `Static.projected` is the
table that ships, and request time is still lookup and pool) — at
256 keeps 91.5% of the variance, a QUARTER of the bytes, and gains
five probe points: a denoising, not a loss. At 128 (78.5%) the probe
gives back 1.7 of those and the centroid none, at an eighth of the
bytes. So the production arithmetic the entry asked about — 30k
units at 1024 float32 is 120 MB — is 30 MB at 256 and 15 MB at 128,
and the smaller table classifies better than the big one did.

**Together: the best no-network number so far, at 40% of the bytes
the pairs table cost.** Triples at PCA 256: 68.3% / 66.7%, 2.1 MB.
The gap to the teacher is 18 points now, from 23; the remaining gap
is still CONTEXT (a unit's one vector wherever it appears), and
neither extension touches that.

## Results — intent-shipped-model-4096 (2026-09-07)

The size-for-points decision intent-window-by-dim filed, taken by the
operator ("Делай все что решил"): the shipped no-network classifier
moves from character 3–5-grams hashed into 1024 buckets to 2–3-grams
into 4096. Refitted through `MakeModel`; `Fit.grams`' defaults move
with it so the artifact stays what the generator produces; every
number the doc comment and the tests pin re-taken from the new
artifact on the same 60 held-out messages:

| | 3–5-grams @1024 (was) | 2–3-grams @4096 (now) |
|---|---|---|
| alone, held-out English | 61.7% | **68.3%** |
| full coverage behind the cues | 75.0% | **80.0%** |
| one typo in the longest word | 63.3% | **71.7%** |
| far half / near half | 66.7% / 83.3% | 70.0% / 90.0% |
| lowercased / hedge / tail / blunt | — / 78.3 / — / 63.3 | 76.7 / 78.3 / 75.0 / 70.0 |
| Proposal F1 | 0.87 | 0.85 |
| Request F1 | 0.80 | **0.88** |
| Notification F1 | 0.73 | **0.87** |
| Other F1 (recall) | 0.56 (0.47) | 0.52 (**0.40**) |
| artifact | 43KB | 171KB, three pieces |

**What the size buys, and what it does not.** Five points at full
coverage, eight under a typo (the smaller window is what survives a
transposition, as intent-typo-robustness found), fourteen points of
Notification and eight of Request — and NOT `Other`: recall 0.47 →
0.40, F1 0.52 just above the 0.50 floor the balance test holds every
class to. The out-of-domain bin is fifteen training rows, and a wider
hash does not manufacture rows; `intent-other-more-rows` is the lane,
and its case is stronger for this table, not weaker.

**Found on the way: a class file caps one string constant at 64KB.**
The 171KB artifact — a generated Scala source, because the module is
cross-built and a classpath resource is a JVM-only way to load a
model into something whose claim is that it needs nothing — did not
compile (`UTF8 string too large`). `MakeModel` now emits the JSON as
literals of at most 60000 characters joined once at load (`Vector(…)
.mkString`); the artifact law (`Fit.save(MakeModel.model) ==
MeetingModel.json`) is unchanged and still holds byte for byte, and
`Models.meeting` still decodes lazily on first use.

## Results — intent-active-learning (2026-09-07)

"Labels are the bottleneck everywhere above, so choose the next ones
to label by uncertainty rather than by order." Simulated on the
fixture, whose labels are all known (`TestActiveLearning`, one
embedding of the 120 messages, offline after): the pool is the
training half, the seed eight rows (two per class), each round adds
four — by the probe's smallest MARGIN over the unlabelled pool
(uncertainty sampling), by the fixture's ORDER, or at RANDOM (five
seeds, the mean); probe and centroid refitted every round, scored on
the held-out half; both mirror splits.

| labels | uncertainty (odd / even) | random (odd / even) | order (odd / even) |
|---|---|---|---|
| 8 (seed) | 55.0 / 51.7 | 55.0 / 51.7 | 55.0 / 51.7 |
| 16 | 65.0 / 60.0 | 60.3 / 62.3 | 41.7 / 30.0 |
| 28 | **78.3 / 80.0** | 65.7 / 75.0 | 43.3 / 45.0 |
| 36 | 76.7 / 85.0 | 75.3 / 80.0 | 41.7 / 51.7 |
| 40 | 76.7 / 90.0 | 75.0 / 82.7 | 53.3 / 63.3 |
| 48 | 75.0 / 86.7 | 75.3 / 84.3 | 60.0 / 68.3 |
| 60 (all) | 73.3 / 85.0 | 74.3 / 85.7 | 75.0 / 86.7 |

(probe accuracy; the centroid's curves have the same shape and are in
the run's output.) Mean over rounds, probe: uncertainty − random
**+2.4 / +1.0**; uncertainty − order +17.3 / +20.0. Labels to reach
80% probe on the split where 80% is reachable: uncertainty **28**,
random 36, order 52.

**A chosen label is worth a little, consistently.** Uncertainty
sampling is ahead of random on both splits over the run and reaches
the same accuracy eight labels sooner — a quarter fewer — where the
accuracy is reachable at all; the gain is largest in the middle (28
labels: +12.7 / +5.0) and gone once the pool is nearly exhausted,
which is what a label-selection method should look like. The known
cost shows too: at 32 the uncertainty curve dips (78.3 → 71.7 on one
split) because the rows nearest the boundary are the least
representative — the classical wobble, recovered by 36.

**Order is a straw man here, and the straw is informative.** The
fixture is grouped by class, so "in order" labels one class for
rounds on end — 30 points behind at 20 labels — which is not what a
stream of real messages does but IS what a reviewer working down a
sorted export does. Class balance first, then uncertainty, is the
whole recipe.

**What follows.** The review queue (the chain's handed-over turns,
Harvest signal 6) is the place this applies: order the rows a person
confirms by the probe's margin, smallest first, after balancing the
classes; the mechanism is one line over `Probe.score(_).margin` and
lives with the queue, not here. `intent-ensemble-weights` stays gated
with its reason stated (below); a calibrated confidence for the
margin — `intent-no-model`'s — is what would turn "smallest margin"
into "below a threshold", and is not needed for the ordering.

## Results — intent-examples-in-language (2026-09-07)

The native-names lane moved one variable and, deliberately, kept the
five example MESSAGES English; translating them was the untried lever,
and examples of a class are the one lever that has consistently paid
here. Measured (`TestExamplesInLanguage`, Live): per language of the
parallel fixture, the same thirty messages, the same English case
names (`Meeting`), the same decoder; the only difference between the
two arms is the language the five examples are written in. The
translations are the author's (the second-author limitation applies
to them as to the fixture); the `what` summaries stay English, which
is what the model has seen in every example.

| language | English examples | examples in the language | delta |
|---|---|---|---|
| fr | 0.887 | 0.927 | +0.040 |
| de | 0.895 | 0.925 | +0.030 |
| es | 0.890 | 0.855 | −0.035 |
| ru | 0.898 | 0.964 | +0.066 |
| ja | 0.927 | 0.888 | −0.039 |
| uk | 0.856 | 0.927 | +0.071 |
| pl | 0.792 | **1.000** | **+0.208** |
| mean | | | **+0.049** |

**The lever holds, and where it pays most is where it was needed
most.** Better on five languages of seven; the biggest gains on the
three Slavic languages, whose English-examples baselines were the
lowest (pl 0.792 → 1.000, uk 0.856 → 0.927, ru 0.898 → 0.964); `Other`
F1 reaches 1.00 in every language but es once the examples speak the
message's language. Thirty messages resolve to 3.3 points each, so
es's −0.035 and ja's −0.039 are one message of thirty apiece — inside
the noise — while pl's +0.208 is six. The reading is the sign across
languages, and it is positive in five of seven with the losses at the
noise floor.

**What ships.** `IntentFixture.meetingExamplesIn(lang)` — the five in
fr, de, es, ru, ja, uk, pl, English for anything else — so a caller
that knows the message's language passes them; the shipped prompt
and its English examples are unchanged for a caller that does not.
Not done: a language detector in front of `Classify` to pick them
automatically (`intent-language-gap` has the per-language numbers a
detector would have to beat), and the fixture-growth question that
would turn a per-language cell from 3.3 points to something a
default could rest on.

## Results — intent-structured-output (2026-09-07)

Every lane in this line bought its answer's SHAPE by persuasion — a
rendered example, written rules, a field order found by measuring the
residue. OpenAI-compatible gateways take `response_format` with a JSON
schema, and `JsonSchema.of(Schema[I])` is exactly that document; the
rozum gateway honours the field (a flat object came back valid). So
`OpenAi.request` gained `responseFormat` and `OpenAi.jsonSchema(name,
schema, strict)`, and five arms ran over the 120 messages on the
`Meeting` taxonomy, the same decoder throughout
(`TestStructuredOutput`, Live, 28 minutes of calls):

| arm | prompt chars | macro F1 | undecodable | reply chars | s/msg |
|---|---|---|---|---|---|
| shipped (persuasion only) | 2276 | 0.909 | 0/120 | 304 | 1.4 |
| shipped + schema | 2276 | 0.909 | 0/120 | 304 | 1.9 |
| minimal + schema | 136 | — | 120/120 | 507 | 3.6 |
| minimal + schema + `conf` vocabulary line | 173 | — | 119/120 | 1097 | 4.8 |
| minimal + schema + the five examples | 657 | — | 115/120 | 497 | 2.5 |

**Where the persuasion already works, the contract is a no-op with a
bill.** The shipped prompt with and without `response_format` gave
the same macro F1, the same per-class scores, the same reply length —
the replies are the same replies — at 36% more latency (1.4 → 1.9
s/msg): the gateway's constrained decoding costs and buys nothing
here, because there was nothing left to constrain.

**Where the persuasion is removed, the contract does not replace it —
and the replies do not even satisfy the schema.** Three minimal arms,
zero decodable replies among 360. The failures are not the model
being wrong, they are the CONTRACT being unenforced: `conf` filled
with `"0.95, 0.95, …"` or `"}, {"`, `why` missing though the schema
requires it, a string where the schema says a tagged object. A
gateway that enforced the grammar could not produce these. So the
rozum gateway's `response_format` is a hint for flat objects and not a
grammar for this shape (nested sums, lists) — which is the one thing
this lane had to find out before anyone leaned on it.

**And one thing the contract could not carry even if enforced.**
`Schema[Conf]` is a refinement over a string (`Schema.refine`), and
`JsonSchema.of` renders an `SIso` as its underlying type — so the
schema says `string` where the decoder wants one of three words. The
persuasion's one-line vocabulary rule carries what the derived
contract loses; a JSON schema with an `enum` for refinements is a
codec change, filed here rather than done inside a measurement —
and done the same day (codec-jsonschema-refinement-enum,
specs/codecs.md): `Conf` declares its vocabulary, the derived schema
says `"enum": ["low", "medium", "high"]` — for contracts and tool
declarations. Not for the prompt: rendering the enum into the
prompt's schema changed the journal's fingerprint, the promotion
rule fired (Request 0.93 → 0.89), and two live runs each way showed
the model deterministic and the enum costing 1.7 macro-F1 both times
(`TestEnumPromptEffect`), so `Classify.prompt` renders its schema
with `vocabularies = false` and the recording stands.

**What follows.** No default moves: the shipped prompt stays as it
is, `responseFormat` stays as a door for a gateway that does enforce
(a later re-measure of the minimal arms is the test of one), and
"persuasion versus contract" has its number — on this gateway the
contract is worth nothing on top of the persuasion and nothing
instead of it.

## Results — intent-distil-diversity (2026-09-07)

The entry: the distilled corpus alone scores ten points below a human
fixture a third its size, which says its distribution is narrow
rather than its labels wrong; "measuring the corpus's own diversity
(say, distinct trigram ratio against the human fixture's) is the
honest way to tell". Measured, offline, in the gate
(`TestDistilDiversity`): distinct-1/2/3 ratios, vocabulary, words
per message and distinct OPENERS (the first two words) per message,
on samples of equal size — the ratios are length-sensitive — averaged
over twenty seeded draws:

| sample of 120 | words/msg | distinct-1 | distinct-2 | distinct-3 | vocab | openers/msg |
|---|---|---|---|---|---|---|
| human fixture | 9.2 | 41.3% | 87.1% | **98.6%** | 457 | **85.0%** |
| distilled, all 320 | 10.1 | 24.4% | 54.0% | 69.1% | 295 | 41.0% |
| distilled, self-consistent 182 | 10.1 | 22.5% | 48.3% | 64.3% | 273 | 33.6% |

**Narrow is the number, and it is large.** Where the human fixture
almost never repeats a three-word run (98.6% distinct), the generated
corpus repeats nearly a third of them (69.1%); it uses two thirds of
the vocabulary for slightly longer messages; and it opens every other
message the same way — `could you` twenty-one times in 182 rows,
`your access` and `the meeting` thirteen each — where the fixture's
commonest opener appears six times in 120. Per class the gap is the
same shape (Request 74 vs 98 distinct-3, Notification 75 vs 100,
Other 67 vs 100; Proposal has too few self-consistent rows to
sample). Only 46% of the corpus's words are the fixture's and only
32% of the fixture's words are in the corpus: the two overlap less
than half, which is what "a mean of the wrong population"
(intent-distil-for-probe) looks like as counts.

**The filter makes it narrower.** The 182 rows the model agreed with
on second sight are LESS diverse than the 320 (64.3% vs 69.1%
distinct-3, 33.6% vs 41.0% openers): self-consistency keeps the
formulaic, which is exactly what a model is most consistent about.
This is why the filter moved the dose's peak and softened the
overdose without changing the verdict (intent-distil-dose) — it
removed noise and variety together.

**What follows, and what is filed.** The persona/register prompting
the entry proposed is not built: a later corpus, generated any way,
now has the number to beat before anyone embeds it — a distinct-3
ratio and an openers ratio within ten points of the human fixture's
on equal samples — and intent-distil-dose and intent-distil-static
have just measured what a corpus that misses it buys (one split's
gain, and none). The generator is the wrong author of variety; the
review queue (Harvest, the chain's handed-over turns) is the right
one, and it does not need a prompt.

## Results — intent-distil-static (2026-09-07)

intent-distil-more's second half: "worth trying on the STATIC table,
which was not fed here because its vocabulary would have to be
re-embedded — a second pass over the teacher rather than a change of
method." This is that pass. A static table has two inputs a corpus
can feed — its VOCABULARY (which units exist, each embedded once by
the teacher) and its WEIGHTS (the SIF counts) — and the classifier
over it a third, its TRAINING ROWS. The 182 self-consistent distilled
rows were tried at each in turn, on the best table so far (words +
pairs + triples at PCA 256), on both mirror splits, the human-only
table re-measured beside them (`TestStaticDistilled`, Live, one
distillation of 3673 units in 4.6 s, 2172 of them the fixture's own):

| arm | units | KB | probe (odd / even) | centroid (odd / even) | means |
|---|---|---|---|---|---|
| human vocabulary, human weights (baseline) | 2172 | 2172 | 65.0 / 68.3 | 71.7 / 66.7 | 66.7 / 69.2 |
| + distilled vocabulary | 3673 | 3673 | 66.7 / 66.7 | 71.7 / 65.0 | 66.7 / 68.3 |
| + distilled vocabulary and weights | 3673 | 3673 | 68.3 / 68.3 | 71.7 / 65.0 | 68.3 / 68.3 |
| + vocabulary, weights, and 20 training rows | 3673 | 3673 | 61.7 / 68.3 | 68.3 / 73.3 | 65.0 / 70.8 |

**Nothing moves outside the noise.** Fifteen hundred generated units
change the probe by 0.0 and the centroid by −0.8; the generated
counts as weights add +1.7 to the probe (+3.3 on one split, 0.0 on
the other); twenty generated training rows cost the probe 1.7 and
give the centroid 1.7 (−3.3 / +6.7 — one split's, the same shape
intent-distil-dose found on live vectors). The table grows 1.7x for
it. A static table's ceiling was measured as CONTEXT (one vector per
unit wherever it appears) and a generated corpus supplies more units,
not more context — so this is the answer the method predicted, now
with the number.

**What follows.** The static table stays distilled from the human
fixture alone; the entry closes with both halves decided: this one
measured, and "more generated rows" declined on intent-distil-dose's
finding that the gain a dose showed was one split's. (The baseline's
65.0 / 68.3 against the 68.3 single-split cell of
intent-static-trigrams-and-pca is the PCA refitted on a table two
units different plus the split's own spread — the same run is the
only fair comparison, which is why every arm here has one.)

## Results — intent-distil-dose (2026-09-07)

intent-distil-for-probe found +40 distilled rows worth ten centroid
points (80.0 → 90.0) and more worse, monotonically — an optimum found
by accident between two arms, on ONE split, with the self-consistency
filter not applied. Found properly: the 0.6B embeds the 120 human
rows and the 320 distilled once; the filter (the model re-judging its
own rows, exactly as intent-label-distillation ran it) keeps 182 of
320, and its verdicts are now data beside the corpus
(`intent-distilled-kept.json`), so that half runs once; the dose on a
grid 0..320, class round-robin, on BOTH mirror splits, unfiltered and
filtered; then every distilled row counted at a WEIGHT in the
centroid's mean (`TestDistilDose`, Live, `Conditions` printed).

| dose | centroid, unfiltered (odd / even) | centroid, filtered (odd / even) | probe, unfiltered (mean) |
|---|---|---|---|
| 0 | 78.3 / 78.3 | 78.3 / 78.3 | 81.7 |
| 20 | 75.0 / 86.7 | 76.7 / 88.3 | 81.7 |
| 40 | 70.0 / **93.3** | 75.0 / 90.0 | 77.5 |
| 50 | 75.0 / 93.3 | 73.3 / 86.7 | 76.7 |
| 100 | 68.3 / 86.7 | 71.7 / 88.3 | 75.0 |
| 320 (unfiltered) / 160 (filtered) | 61.7 / 78.3 | 66.7 / 86.7 | 70.8 |

**The optimum was a split artefact.** On the test-even split the dose
reproduces the earlier lane exactly — 78.3 → 93.3 at 40–50, the "ten
points" — and on the test-odd split the same dose LOWERS the centroid,
78.3 → 70.0 at 40, and no dose on either pool ever puts the odd split
above its 78.3. The two halves of one fixture disagree by fifteen to
twenty points about whether distilled data helps at all; the means
peak at +5.9 (dose 50, unfiltered) and +4.2 (dose 20, filtered), both
made entirely of one split. The probe never gains, on any dose, on
either pool — its best dose is zero, as the earlier lane also saw.

**The filter moves the shape, not the verdict.** Keeping the 182 the
model agreed with on second sight moves the mean's peak from 50 to
20, and flattens the harm at large doses (160 filtered: 76.7 against
74.2 unfiltered at 160, 70.0 at 320) — a cleaner corpus hurts less
when overdosed — but it does not make any dose clear the bar on both
splits.

**A weight instead of a dose.** Every distilled row at w in the
centroid's mean:

| pool | w | centroid (odd / even) | mean |
|---|---|---|---|
| unfiltered | 0.10 | 76.7 / 86.7 | 81.7 |
| unfiltered | 0.25 | 73.3 / 91.7 | 82.5 |
| unfiltered | 1.00 | 61.7 / 78.3 | 70.0 |
| **filtered** | **0.10** | **80.0 / 86.7** | **83.3** |
| filtered | 0.25 | 76.7 / 90.0 | 83.3 |
| filtered | 1.00 | 66.7 / 86.7 | 76.7 |

The filtered pool at w = 0.10 is the ONE cell ahead of the human-only
centroid on both splits — +1.7 on the split that dislikes distilled
data, +8.4 on the one that likes it — which is what "a little
different data broadens a mean" looks like when it is stated on both
halves: real on one, inside the noise on the other. A knob, then, but
a small one, and the honest default stays the human fixture alone:
nothing here clears the ten-point bar on both splits.

## Results — intent-4b-with-more-data (2026-09-07)

The entry's prediction: the 4B embedder is worse at 60 examples
because 2560 dimensions need more of them — re-run the learning
curve on both embedders and find where the lines cross. Done on the
120-message fixture, both vectorisers through the rozum gateway,
both mirror splits (the bar intent-instruction-prefix measured), the
training half grown class-balanced from 8 to 60, probe and centroid
at each step, every row printed with its `Conditions`
(`TestLearningCurveBoth`, Live). The two splits' means:

| n | 0.6B probe | 0.6B centroid | 4B probe | 4B centroid | 4B ahead on both splits |
|---|---|---|---|---|---|
| 8 | 53.3% | 53.3% | 55.0% | 50.0% | no |
| 16 | 65.0% | 64.2% | 64.2% | 60.8% | no |
| 24 | 71.7% | 75.8% | 71.7% | 68.3% | no |
| 32 | 84.2% | 75.0% | 78.3% | 74.2% | no |
| 40 | 80.0% | 78.3% | 80.0% | 75.0% | no |
| 48 | 80.8% | 79.2% | 80.0% | 77.5% | no |
| 60 | 80.0% | 78.3% | 78.3% | 75.8% | no |

**The lines do not cross, and they are not converging on a crossing.**
At no n up to 60 is the 4B ahead of the 0.6B on both splits, on
either classifier; its means sit level with or under the small
model's at every step (centroid 2.5–7.5 points under from 24 on),
and BOTH curves flatten from 32 — the same shoulder the first curve
found on the 0.6B alone. A bigger vectoriser that needed more data
would show a steeper late slope; it shows the same flat one. So the
prediction is not supported inside this fixture, and a crossing past
n = 60 is a claim the fixture cannot make — "not shown" rather than
"refuted", but nothing here argues for it.

**What follows.** The 0.6B stays the vectoriser for every measured
row and for a distilled corpus (the question intent-distil-more
would have asked); the 4B stays what it was found to be — 2.5x the
dimensions for the same or fewer points — until a fixture at least
twice this size is measured with the same two splits and shows a
late slope. The 4B's load is also the only cost in the run: 2 s to
embed 120 messages once resident against 0 s for the small one.

## Results — intent-instruction-prefix (2026-09-07)

The entry's own ask: the first measurement (60 messages) found +1.6
(probe) and +3.3 (centroid) for "Classify the intent of this
message: ", a 6.6-point spread across framings, and a cost for the
long e5-style one — at or near the noise floor — so re-measure on the
grown fixture before any default moves. Done: the same four framings,
the same probe/centroid, `Qwen3-Embedding-0.6B` through the rozum
gateway, on the 120-message fixture — and on BOTH mirror splits
(train odd / test even and the reverse), so the split-to-split spread
of the bare framing is measured in the same run and is the bar a gain
has to clear on both splits (`TestPrefixGrown`, Live, every row
printed with its `Conditions`).

| framing | probe (test-odd / test-even) | centroid (odd / even) |
|---|---|---|
| bare | 76.7% / 86.7% | 78.3% / 78.3% |
| classify-instruction | 78.3% / 86.7% | 76.7% / 81.7% |
| task-instruction (e5 style) | 86.7% / 81.7% | 83.3% / 63.3% |
| represent-for | 78.3% / 80.0% | 73.3% / 78.3% |

**No framing clears the bar, and the bar is the finding.** The bare
probe is 10.0 points apart between the two halves of one fixture
(the centroid 0.0); the classify prefix is +1.7 / +0.0 on the probe
and −1.7 / +3.3 on the centroid — inside that; the long e5-style
instruction is +10.0 / −5.0 and +5.0 / −15.0, which is not a cost or
a gain but a coin, and the represent-for prefix +1.7 / −6.7. No
default moves; the bare message stays the framing every row is
measured under, and `Conditions.Bare` stays what it prints.

**What the ten points say.** Sixty test messages resolve to 1.7
points each; two halves of the same 120 rows disagree by six
messages on the probe. That is the fixture's size speaking, not the
embedding's, and it bounds every single-split claim in this
programme at about that width — which is the case
intent-language-fixture-growth already makes for the per-language
set, now with a number for the English one. A framing gain that
would be worth a default has to be larger than ten points on one
split, or hold on every split of a larger fixture.

## Results — intent-rule-induction (2026-09-07)

**Cues induced from the corpus, measured where the hand-written ones
are.** `Induced` is a RIPPER-shaped tier: literals are words, adjacent
pairs and either at the start of the message (`^please`); a rule is a
conjunction, grown on two thirds of the training half by FOIL gain,
pruned on the other third by dropping trailing literals while its
precision there holds, and kept on the whole training half at a
support floor (two rows) and a precision floor; class by class,
rarest first; deterministic; a `Trained` is rules a person can read.
Same split as `TestCharGrams`, the held-out half, beside
`Patterns.meeting` on the same rows, `classify` at margin 0.4:

| | rules | fired on | right where fired |
|---|---|---|---|
| hand-written cues (`Patterns.meeting`) | — | 53.3% | 90.6% |
| induced, support 2, floor 0.8 | 8 | 51.7% | 67.7% |
| **induced, support 2, floor 0.9 (default)** | 4 | 11.7% | **85.7%** |
| induced, support 3, floor 0.8 | 5 | 46.7% | 60.7% |
| induced, support 3, floor 0.9 | 2 | 8.3% | 80.0% |
| induced, support 4, floor 0.8 | 2 | 28.3% | 58.8% |

The eight rules at the 0.8 floor read as cues a person could have
written — `^please -> Request`, `you & could -> Request`, `^can &
^can you -> Request`, `^the & now -> Notification`, `has & ^the ->
Notification` — and as the ones a person would not: `we -> Proposal`,
`no -> Notification`, `minute -> Proposal`, each right on every one
of the few training rows it stood on and wrong on a third of the
held-out ones it fires on. That is the finding: **at sixty rows
induction buys coverage or precision, not both**, where the
hand-written tier has both because its author had more than sixty
rows in mind. The default keeps the cue tier's property — being right
where it fires — at 85.7% and says how little that covers (11.7%);
the grid is in the suite for the corpus that grows, which is where
`intent-static-embeddings` and the learning curve put the limit for
every other tier too. Two corrections the algorithm needed on the way
are in the code: a literal that leaves a rule on fewer rows than the
support floor is a row's name, not a literal; and keep-or-stop is
judged on the whole training half, because a five-row pruning set
was rejecting a rule right on all thirty rows it was grown on. The
tier is not wired into the Router: its number does not earn a place
ahead of the hand-written cues.

## Results — tod-schema-diagnostics (2026-09-07)

The two experiments from the operator's task-oriented-dialogue
papers, run against our own taxonomy. Both were filed as
"experiments, not features", to turn "our schemas are good" from an
assumption into a number before more is invested in writing them.
Four arms, the whole labelled fixture (120 messages), no examples and
no gate in any arm — the NAMES are the only thing that differs.
`TestSchemaDiagnostics`, Live-tagged; 24 minutes of model time.

| arm | macro F1 | Other F1 | undecodable | vs shipped |
|---|---:|---:|---:|---:|
| `Meeting` (shipped names) | 0.685 | 0.63 | 6/120 | — |
| `Indexed` — C1..C4, field `s1`, no words | 0.100 | 0.00 | 0/120 | **−0.585** |
| Near synonyms (Suggestion/Ask/Update) | 0.468 | 0.45 | 4/120 | −0.217 |
| Far synonyms (Offer/Solicitation/Advisory) | 0.260 | 0.23 | 4/120 | −0.425 |

### (a) D3ST's diagnostic: the names are the classifier

With every word removed from the schema the model does not degrade,
it stops classifying: it answered `C1` for 100% of messages (Proposal
recall 1.00 at precision 0.25 — the base rate — and F1 0.00 for the
other three classes). Every reply decoded, so this is not a parsing
failure: the shape was right and the content was empty of decisions.

- [x] the shipped taxonomy's four identifiers carry essentially ALL
      of the prompted model's discrimination; the message alone, with
      an index-named schema, carries none
- [x] this is not a defect to fix. D3ST randomizes names so a TRAINED
      model cannot lean on them; a PROMPTED model is bought precisely
      for those priors. What the number settles is the STATUS of a
      schema in this repository: `Schema[I]` used as a taxonomy is
      not a data declaration that happens to be shown to a model, it
      IS the prompt, and 0.585 macro F1 of the result lives in the
      identifiers.

### (b) SGD-X's robustness: the reading is NOT stable under a rename

A paraphrase a colleague would make without thinking — `MeetingRequest`
→ `MeetingAsk`, `MeetingNotification` → `MeetingUpdate` — costs 0.217
macro F1, and the damage is per class rather than spread: Request
recall fell 0.67 → 0.07 under "Ask", and Notification F1 fell 0.77 →
0.00 under "Advisory" in the far arm. The plain, standard word for a
class is worth more than any wording around it.

- [x] near paraphrase −0.217, far paraphrase −0.425; the spread across
      three wordings of the SAME four classes (0.685 / 0.468 / 0.260)
      is larger than the gap between the shipped prompt and the bare
      one that this line spent four lanes closing
- [x] caveat, stated rather than hidden: `NotAboutMeetings2` carries a
      digit (the enum needed a distinct name in the same file), and a
      digit in a case name is itself a small paraphrase. `Other` fell
      0.63 → 0.45 in the near arm, so some of that 0.217 may be the
      digit rather than the synonyms. The Request and Notification
      collapses do not depend on it.

### Decisions
- **Renaming a taxonomy case is a PROMPT CHANGE and must be measured
  like one.** The same rule the JSON Schema rendering earned
  (`vocabularies = false`, measured at 1.7 macro F1): a case name
  reaches the model, so a rename is a model-facing change, not a
  refactor. A pull request that renames a case in a taxonomy without
  a number beside it is changing behaviour blind.
- **Name a class with the plainest standard word for it.** "Request"
  beats "Ask" by 0.55 recall on the same messages; "Notification"
  beats "Advisory" by 0.77 F1. Synonym choice is not style here.
- **The deterministic tiers are the answer to this fragility, not a
  better prompt.** The cue and gram tiers do not read identifiers at
  all, which is exactly why the shipped door blends them with the
  probe; this measurement is the strongest argument yet for keeping
  the model off the critical path where a deterministic tier can
  answer (Okay!Chat's own direction: drive the handed-over share
  down).
- **Not shipped as a lint.** A rule that refuses unusual case names
  would be a guess about which words are plain; the honest artifact
  is this measurement plus the decision above. If a taxonomy rename
  ever lands without a number, the suite is here to produce one.

## Results — intent-offline-other (2026-09-07)

The autonomous path, on the operator's redirection: the model lanes
of this line improve what the MODEL does, and the offline door's own
worst number had never been attacked. It is `Other`: recall 0.40, F1
0.52 on held-out English — more than half the traffic that is not
about meetings lands in a meeting class with a confident face.

The hypothesis came from the model path's own history: what closed
that hole there was not a better four-way answer, it was a SEPARATE
BINARY QUESTION asked first. The offline door had never had that
step, and the one abstention mechanism it did try (a margin floor on
the four-way model) was measured not to separate. So: a second
`CharGrams` model fitted on the same rows folded to in/out, run
before the four-way tier. No network, no new corpus.

The criterion was written into the claim before any number was seen:
`Other` recall ≥ 0.60, total accuracy down no more than one point,
every class F1 ≥ 0.50.

### What the binary tier can and cannot do
- [x] by ARGMAX it never fires at all: out-of-domain recall 0.0% over
      15 held-out rows, binary accuracy 75.0% — exactly the base rate.
      With 15 out-of-domain rows against 45 in-domain, the fit
      collapses to the majority answer, and the gated door is
      identical to the shipped one at every margin floor.
- [x] balancing the classes (15/15) makes the argmax fire (out
      recall 66.7%) and destroys the tier: binary accuracy 50.0% and
      the ranking quality falls with it (AUC 0.615). Undersampling
      threw away two thirds of the in-domain rows to buy an argmax.
- [x] the REPRESENTATION can see it even where the argmax cannot: on
      the imbalanced fit, p(OUT) ranks the held-out rows at **AUC
      0.843** and puts 9 of the 15 true out-of-domain messages in the
      top 15. A rare class can rank its own rows first while never
      winning an argmax, and a threshold is how that becomes a
      decision.

### The trade, and why it is declined
Threshold on p(OUT), one split (60 held out):

| door | total | Other recall | worst class F1 |
|---|---:|---:|---:|
| shipped | 80.0% | 0.40 | 0.52 |
| p(OUT) ≥ 0.30 | 81.7% | 0.47 | 0.58 |
| p(OUT) ≥ 0.10 | 78.3% | 0.53 | 0.57 |
| p(OUT) ≥ 0.05 | 73.3% | 0.73 | 0.59 |

The +1.7 points at 0.30 is ONE message of sixty, so the same
comparison was run over eight random splits with both tiers refitted
each time (scoring the shipped artifact against a fresh split would
leak its training rows):

| door | total (8 splits) | Other recall | better than base |
|---|---:|---:|---:|
| no gate | 71.0% (sd 4.2) | 0.46 (sd 0.21) | — |
| p(OUT) ≥ 0.30 | 71.5% (sd 4.2) | 0.51 (sd 0.23) | 3/8 |
| p(OUT) ≥ 0.20 | 71.0% (sd 4.2) | 0.52 (sd 0.23) | 3/8 |
| p(OUT) ≥ 0.10 | 70.6% (sd 4.7) | 0.59 (sd 0.23) | 3/8 |
| p(OUT) ≥ 0.05 | 67.7% (sd 5.3) | 0.65 (sd 0.21) | 2/8 |

- [x] DECLINED against the stated criterion. The best recall inside
      one point of the baseline is 0.59, and the criterion said 0.60;
      the gain that looked free on one split is +0.4 points with sd
      4.2 and wins on three splits of eight, which is noise.

### Decisions
- **Nothing ships.** The offline door is unchanged, and no second
  artifact is added: a knob that does not improve the default on
  average is not worth 85KB in an artifact whose whole claim is that
  it needs nothing. The mechanism and its curve are recorded here so
  a caller who values refusal over accuracy can build it in four
  lines, with a number for each threshold.
- **The single-split gain was reported and then retracted by the
  resampling, in that order.** Worth stating because the tempting
  path was to ship "+1.7 points, better on every axis" — which was
  one message.
- **The binding constraint is named, and it is not the algorithm.**
  p(OUT) at AUC 0.843 says the signal is there; 15 out-of-domain
  training rows say why no decision rule can use it well. This is the
  strongest evidence yet for `intent-other-more-rows`, and it changes
  that lane from "more data would presumably help" to "the detector
  already ranks at 0.843 and is starved of rows".
- **What would settle it**: 40-60 real out-of-domain rows (the
  operator's, or harvested from the service's own traffic, which is
  where `harvested.json` is meant to fill from). Then this suite
  re-runs unchanged and the criterion is either met or the mechanism
  is dead for good.

## The autonomy programme (2026-09-07)

> Moved and expanded: the ARCHITECTURE of this programme — the seams
> every future idea plugs into, the literature each rests on with
> quotes, and the staged plan with criteria — now lives in
> [specs/intent-autonomy.md](intent-autonomy.md). What stays here is
> the measurement history: the numbers below are the evidence that
> document rests on.

Written on the operator's instruction to aim this line at working
WITHOUT models, and to put everything that serves that into the plan.
It collects what is already measured, names the metric the programme
is judged by, and orders the lanes. Nothing here is a hope: each item
says what would move a number, and the items measured NOT to move one
are listed too, so they are not tried a fourth time.

### The metric, now printed (intent-autonomy-report, 2026-09-07)
"Works without a model" is not one number, it is two, and this line
had never printed them together: the AUTONOMY RATE (the share
answered with no network at all, and the precision among those
answers) and the HANDED-OVER SHARE (the rest, which must reach a
model or a person). `MeasureAutonomy` prints both, per tier and per
abstention floor, over the same held-out English 60.

| door | coverage | precision among answered | right of all | handed over |
|---|---:|---:|---:|---:|
| cues only (floor 0.4) | 53.3% | 90.6% | 48.3% | 46.7% |
| grams only, margin ≥ 0.0 | 100.0% | 68.3% | 68.3% | 0.0% |
| grams only, margin ≥ 0.3 | 68.3% | 73.2% | 50.0% | 31.7% |
| grams only, margin ≥ 0.5 | 46.7% | 78.6% | 36.7% | 53.3% |
| cues, then grams ≥ 0.0 (SHIPPED) | 100.0% | 80.0% | 80.0% | 0.0% |
| cues, then grams ≥ 0.2 | 86.7% | 82.7% | 71.7% | 13.3% |
| cues, then grams ≥ 0.5 | 73.3% | 88.6% | 65.0% | 26.7% |

And the table a caller actually reads, which is the point of the
lane — at the precision they need, how much needs no network:

| precision needed | best offline door | coverage | handed over |
|---|---|---:|---:|
| ≥ 95% | none reaches it | 0.0% | 100.0% |
| ≥ 90% | cues only (90.6%) | 53.3% | 46.7% |
| ≥ 85% | cues + grams ≥ 0.5 (88.6%) | 73.3% | 26.7% |
| ≥ 80% | cues + grams ≥ 0.0 (80.0%) | 100.0% | 0.0% |

- [x] the framing this line had been quoting — "80.0% at full
      coverage" — was the WORST of the available promises, because it
      never offered the abstention. Three quarters of the traffic can
      be answered at 88.6% with nothing but the artifact and the
      cues; a quarter is handed over. That is the honest product
      statement, and it was one flag away the whole time.
- [x] the per-class law is asserted here too, so a refit that kills a
      class fails the autonomy report and not only the model suite.

Every lane below is judged by moving one of those two numbers without
breaking the law (no class below F1 0.50).

### Measured NOT to help — do not retry without new evidence
- **Another representation.** TF-IDF 61.7%, hashed char n-grams
  68.3%, the static centroid table 68.3% at 2.1MB, both embedders
  flat from 32 examples with the same slope: four roads to one
  ceiling. The limit is register and context, not the encoder
  (2026-09-07). `intent-fasttext-subword` stays gated for this
  reason; a per-language check found char-grams do NOT lose Russian
  (53.3%, above English's 40.0% on the same 15-row slices), so
  morphology is not the gap either.
- **Generated rows.** Three generators, zero gain; the distilled rows
  are a third as diverse as the fixture and carry the generator's
  register (intent-distil-*).
- **Abstention by a margin floor** on the four-way model: no
  threshold separates right from wrong (Router.Floors).
- **An argmax binary gate** for out-of-domain: with 15 rows against
  45 it collapses to the majority; balanced, the tier dies
  (intent-offline-other, above).

### Measured TO help — every ceiling moved only when the corpus moved
- Induced cues reach 85.7% precision at 11.7% coverage on 60 rows;
  the hand-written ones are at 90.6% and 53.3% after months of
  patience. Coverage is a function of rows.
- The out-of-domain detector ranks at AUC 0.843 on 15 rows and cannot
  be turned into a decision rule at that size.
- Examples in the message's own language paid +0.049 macro F1.

### The lanes, in order
1. **`intent-autonomy-report`** — print the two metrics. A suite over
   the fixture that reports, per tier: coverage, precision among
   answered, the class breakdown, and the share that would reach a
   model. Offline, no new data, and it must run in the ordinary
   suite so every later lane moves a tracked number. Nothing else in
   this programme can be judged without it, which is why it is first.
2. **`intent-harvest-loop`** — real rows, with provenance. Every
   model classification that survives grounding and the confidence
   floor becomes a candidate row; a person confirms it; confirmed
   rows land in the service's `corpus/harvested.json` with the
   message, the label, the language and who confirmed. The property
   that makes this different from distillation is that the MESSAGES
   are real traffic, which is exactly what the generated rows lacked.
3. **`intent-label-queue`** — which rows to ask a person about:
   uncertainty (small margin) and disagreement between tiers, the
   shape the active-learning lane already measured (28 labels against
   36 for the same gain). A queue a person can clear in ten minutes a
   day is worth more than a corpus nobody writes.
4. **`intent-refit-gate`** — a refit that lowers ANY class below the
   law is refused, and the refit prints the before/after per class. A
   consumer's corpus grew unevenly, one class reached 137 of 184 rows,
   their headline rose and a class died; this is the guard for that.
5. **`intent-induce-on-harvest`** — re-run cue induction whenever the
   corpus grows, and ship the induced cues beside the hand-written
   ones. Cues are the only tier that needs NOTHING at run time, so
   every point of coverage they take is a point of pure autonomy.
6. **`intent-offline-slots`** — the frame is filled today by rules
   plus a model. A sequence labeller (CRF or a grammar over the
   existing `Amount`/`Duration`/`People` extractors) would fill slots
   with no network, which is a capability the offline door does not
   have at all, not merely a better number.
7. **`intent-per-language-models`** — the shipped artifact is English
   only and scores chance elsewhere; one artifact per language, fitted
   the same way, once `intent-language-fixture-growth` supplies rows.

### The one honest caveat
Three of the seven lanes are blocked on rows that only people can
produce, and this programme cannot generate them (measured). What it
CAN do is make every row count: the queue picks which to ask for, the
gate stops a refit from killing a class, and the report says whether
autonomy actually moved.

## The harvest programme — the model reads the logs, people only arbitrate (2026-09-07)

> The full treatment, with the seams and the citations, is
> [specs/intent-autonomy.md](intent-autonomy.md) §4. This section is
> kept as the measured rationale.

The operator's instruction: people should not be writing rows; the
system should mine its own chat logs WITH a model and improve the
network-free tiers from what it finds. That is a different shape from
this line's failed distillation lanes, and the difference is exactly
the one the literature draws.

**Why this is not the distillation that failed here.** `intent-distil-*`
had a model WRITE messages, and the rows carried the generator's
register (a third of the fixture's diversity, no gain anywhere).
Here the model does not write anything: the MESSAGES are real
traffic, and the model only proposes a LABEL for them. Labelling real
text with an LLM and training a small classifier on the result is a
studied, working practice ([Knowledge Distillation in Automated
Annotation](https://arxiv.org/pdf/2406.17633); [Efficient Intent-Based
Filtering ... Knowledge Distillation from LLMs](https://arxiv.org/html/2503.17336v1)),
with a known and quantified failure mode — systematic label noise and
category bias — that has its own remedies
([Calibrating Classifiers on LLM-generated Noisy Labels](https://arxiv.org/pdf/2505.19675),
reporting ~7% recovered by noise-aware refinement).

**Where people come in, and how little.** Not writing rows: arbitrating
the uncertain ones. [CoAnnotating](https://arxiv.org/pdf/2310.15638)
allocates work between the model and a person by UNCERTAINTY —
the model keeps what it is sure of, a person sees only the rest. Our
own active-learning lane measured the same shape (28 labels for the
gain that took 36 chosen at random), so the mechanism is already
half-built here.

**The tiers we already ship are labelling functions.** Cues, grams and
the static table each answer some messages and abstain on others, at
known precision — which is the definition of a labelling function in
[Snorkel](https://www.vldb.org/pvldb/vol12/p223-varma.pdf)'s sense.
Today the offline door is a CASCADE: the first tier that fires wins,
and every agreement or disagreement behind it is thrown away. A label
model learned from agreement (no gold labels needed) is a different
combiner over the same tiers, it runs with no network, and it is
directly a change to the AUTONOMOUS door rather than to the prompt.

**And what the log holds that our taxonomy does not.** `Other` is a
diffuse bin with the worst numbers in the door, and part of the
reason is that it is several real classes nobody has named. Intent
DISCOVERY does exactly that from logs:
[Dial-In LLM](https://aclanthology.org/2025.emnlp-main.300/) (LLM in
the loop for cluster naming and coherence, >95% agreement with human
judgement on 100k real customer-service calls) and
[NILC](https://arxiv.org/abs/2511.05913) (clusters refined by the
model on the uncertain utterances, WSDM 2026). For us that is
`intent-split-other` with a method attached.

### The lanes this adds (all listed in BACKLOG)
1. `intent-annotate-log` — the model labels REAL logged messages; a
   row is kept only if the reading grounds in the message, the
   confidence is at least Medium, k samples agree, and no
   deterministic tier contradicts it at high margin. Provenance per
   row: model, prompt fingerprint, date, filters passed.
2. `intent-coannotate-queue` — uncertainty-guided routing: a person
   sees only the rows the filters could not settle, ranked by
   disagreement. The measurement is human effort per point of
   autonomy, not accuracy alone.
3. `intent-label-model` — the tiers as labelling functions, combined
   by learned agreement instead of a cascade. Offline, and judged on
   the autonomy report at equal coverage.
4. `intent-discover-classes` — cluster what lands in `Other`, name
   the clusters with the model, propose new classes for a person to
   accept or reject. Judged by `Other`'s recall after the split and
   by how many proposals survive review.
5. `intent-noise-aware-refit` — if the harvested labels prove noisy
   enough to bind, the noise-aware refinement the literature reports;
   opened only by a measurement showing noise is what limits the
   refit.

### The honest ordering
1, then 2 (they are the same pipeline), then 3 (needs no new data at
all and improves the no-network door on its own), then 4, then 5 if
measured to matter. Lane 3 is the one that improves autonomy without
a single new row, which is why it should not wait behind the harvest.

## Results — intent-label-model (2026-09-07)

Stage 1 of the autonomy programme, and the one that needed no new
rows: our offline tiers are labelling functions, the door combines
them as a CASCADE (first that fires wins), and Snorkel's argument is
that a combiner learned from AGREEMENT beats that. Measured, and on
this corpus it does not.

### The apparatus
Six offline labelers, none touching a network: hand cues, induced
cues, char-grams at (2,3), the same at (4,5), a word TF-IDF head, and
the gram model over the first clause only. `Agreement.estimate`
weighs each by how often it matches the weighted consensus of the
others (Dawid-Skene's iteration; Snorkel's generative model is the
same idea at scale) and never sees a label — the weights come from
UNLABELLED held-out messages, which is the property that would have
let a deployment re-estimate them on its own log.

### What happened, in order
- [x] **Correlation broke the estimate first.** With all six, the
      estimator ranked `prefix` at 0.833 — a labeler whose actual
      precision is 58.3% — because it IS the gram model on a
      substring, and it agrees with its own parent. Three of six
      votes came from one model; a majority they form is an echo, not
      evidence. Combined door: 66.7% at full coverage against the
      cascade's 80.0%.
- [x] **Dropping the echoes fixed the WEIGHTS.** Over four
      independent labelers the estimator put the cues first (0.914),
      which is correct — they are the precision tier at 90.6%. On one
      split the door then looked good: 85.4% precision at 80.0%
      coverage, worst class F1 0.59 against the cascade's 0.52.
- [x] **And resampling took it back.** Eight random splits, every
      labeler refitted per split and the weights re-estimated on that
      split's unlabelled half:

| door | coverage | precision | worst class F1 |
|---|---:|---:|---:|
| cascade, full coverage | 100.0% (sd 0.0) | 72.9% (sd 5.9) | 0.52 |
| cascade, grams margin 0.20 | 89.6% (sd 2.0) | **77.5%** (sd 5.6) | **0.56** |
| agreement over 4, margin 0.20 | 82.9% (sd 2.7) | 72.2% (sd 5.1) | 0.42 |

      The agreement door is ahead on ONE split of eight. DECLINED.

### Decisions
- **Nothing ships, and the estimator moves to the test sources.** It
  is the apparatus of a measurement; keeping it in `main` unused
  would be commemorating a result rather than recording one.
- **The cascade is not naive — it is right for this shape.** One
  labeler (the cues) is far more precise than the rest and abstains
  cheaply. A cascade lets it answer and steps aside; a weighted vote
  DILUTES it with three weaker voices. Snorkel's setting is dozens of
  genuinely different sources, none dominant; ours is four with one
  dominant. The technique is not wrong, our shape is not its shape.
- **What would change the answer**, stated so the lane can be
  reopened honestly: many more independent labelers (per-class cue
  sets, per-language models, a slot-based labeler), or dependency
  modelling so correlated labelers stop inflating each other. Both
  are lanes, neither is a tweak.
- **The by-product worth keeping**: the resampled table above is the
  first measurement of the shipped cascade AT A MATCHED FLOOR, and it
  confirms the autonomy report's finding from another angle —
  abstention pays: 89.6% coverage at 77.5% beats 100% at 72.9%, and
  the worst class rises with it.

## Results — tod-demonstrations-from-the-log (2026-09-07)

"Show, Don't Tell" (Zhao & Gupta 2022) reports that ONE annotated
example in the input does the work slot DESCRIPTIONS are supposed to
do. Run here against the sharpest question this line has: the
previous lane measured that our taxonomy's identifiers carry
essentially all of the discrimination (0.685 → 0.100 macro F1 with
the words removed), so — does a demonstration RECOVER what the names
carry? If it does, a taxonomy stops being a prompt and the rename
hazard is a paper cut. Four arms, one session, the whole 120-message
fixture, so both baselines are re-measured beside the new arms rather
than remembered.

| arm | macro F1 | Other F1 | undecodable |
|---|---:|---:|---:|
| `Meeting`, no demonstrations | 0.685 | 0.63 | 6/120 |
| `Meeting` + 4 demonstrations | **0.892** | 0.84 | **0/120** |
| `Indexed` (C1..C4), no demonstrations | 0.100 | 0.00 | 0/120 |
| `Indexed` + the same 4 demonstrations | 0.376 | 0.38 | 0/120 |

Both baselines reproduced the diagnostics lane's numbers to three
decimals on a different day (0.685 and 0.100), so the harness is
stable and the deltas are the measurement.

### What the demonstrations are, and where they come from
`Demonstrations.perClass(recorded, exclude)` takes recorded
`(message, intent)` pairs and returns ONE per class, in the
taxonomy's own case order, the first the log offers — the dullest
rule that can be stated in a sentence, because a clever one needs its
own measurement. `Demonstrations.fromReplies` is the bridge from a log
of RAW replies: each is decoded with the same reader the live path
uses, and an undecodable or empty one is dropped rather than
inherited. Nothing in the module reads a log itself; the caller passes
the pairs, so okay-intent gains no persistence dependency and
okay-chat can point it at the ChatLog it already writes. The scored
messages are passed as `exclude`, so no arm is ever shown its own
answer key (asserted in the suite, not just intended).

### The answer: demonstrations do not replace names, they compose with them
- [x] **With real names, four demonstrations are worth +0.207 macro
      F1** (0.685 → 0.892) and `Other` +0.21. That is the largest
      single move this line has measured from a prompt change.
- [x] **They also fix the SHAPE**: undecodable replies 6/120 → 0/120.
      A model shown one filled answer per class stops inventing
      wrappers. Every earlier lane that fought the decode rate was
      fighting this.
- [x] **With index names they recover only part of the gap**: 0.100 →
      0.376. Of the 0.792 between "no words, no examples" and "names
      and examples", the examples buy 35% and the names 74% — they
      overlap, and neither is a substitute for the other. SDT's
      finding (an example beats a description) holds in the sense
      that examples are powerful; in OUR shape the names are worth
      more than the examples, and the two together are worth more
      than either.
- [x] **The demonstrations were SELECTED, not written**: one per
      class, first the log offers, in taxonomy order, with the scored
      messages excluded by construction. So the mechanism the
      programme needs — the log becoming prompt material — performs
      like hand-written examples, which is the result that matters
      operationally.

### What a caller should do with this
1. Name classes with the plainest standard word (measured:
   tod-schema-diagnostics).
2. Show one demonstration per class, taken from the log
   (`Demonstrations.perClass`), never from the messages being scored.
3. Expect the decode rate to go to zero, and stop paying for the
   defensive parsing that a bare prompt needs.
4. A taxonomy whose names cannot be changed (someone else's types, a
   wire format) is not hopeless: demonstrations take it from 0.100 to
   0.376 on this fixture — usable, and far below what a renamed
   taxonomy reaches.

### Decisions
- **The selection rule is stated, not tuned.** One per class, first
  seen, taxonomy order. Nearest-neighbour retrieval of demonstrations
  (the obvious next idea) needs the vector tier and a measurement of
  its own; ranking by confidence needs a calibration this stack does
  not claim to have. Both are named in the backlog rather than
  smuggled in as "obviously better".
- **Demonstrations are prompt text, like names.** A change to the
  selected demonstrations changes the prompt and therefore the
  journal fingerprint, exactly as a rename does. The selector is
  deterministic for that reason: the same log yields the same prompt.

## Results — intent-offline-slots (2026-09-07)

Stage 7 of the autonomy programme, run measure-first: a door that
names the intent with no network and then asks a model for every slot
has moved the call, not removed it — so what do the extractors we
already ship actually fill? `MeasureSlotCoverage`, offline.

### What the offline path fills (English fixture, 120 messages)

| slot | before | after |
|---|---:|---:|
| `when` | 29 (24.2%) | **35 (29.2%)** |
| `duration` | 7 (5.8%) | 7 |
| `people` | 1 (0.8%) | 1 |
| whole frame (all three) | 0 | 0 |
| any slot | 33 (27.5%) | 39 (32.5%) |

The whole-frame zero is not a failure: most messages never mention a
duration or a headcount, and a frame that asks for what was not said
is doing its job. The number that can be read as recall is the one
against a denominator: of the messages whose words suggest a time,
the extractor found **66% before and 80% after**.

### What the misses actually were
The measurement printed every message that mentions a time and yields
nothing — twenty of them — and reading that list is the whole lane.
Most were false alarms of the crude hint list ("I **am** writing",
"Good **morning**!", "over the weekend" with no day). Four shapes
were real, and they split two and two:

- [x] **A possessive was invisible**: "before tomorrow's meeting",
      "add the finance team to Thursday's invite". The tokeniser
      stripped `.,!?;` and not `'s`, so the word did not match. Fixed:
      a trailing apostrophe-s (straight or curly) is dropped.
- [x] **A plural weekday was invisible**: "Thursdays are remote from
      now on". `weekdays.startsWith(token)` fails for the longer
      token. Fixed: the plural is tried second, so nothing that
      matched before changes.
- [x] **A bare time is DECLINED on purpose**: "a badge after 7pm"
      names an hour with no day, and guessing today or tomorrow would
      put a date in a frame that nobody said.
- [x] **A range is DECLINED on purpose**: "sometime this week", "any
      afternoon this week". `When` holds one date; the honest move is
      to ask which day, not to pick one.

Both refusals are asserted in `TestTemporalForms`, so a later change
cannot quietly turn a decision into a bug.

### Decisions
- **No sequence labeller.** The plan named a CRF for this stage; the
  measurement says the gap was four word-shapes, two of which are
  correct refusals. Two regex-level fixes bought +6 messages of the
  120 and 14 points of recall against the hint denominator — a
  learned labeller would have needed rows this programme does not
  have, to beat that.
- **The measurement stays as a suite.** It is the baseline for slot
  work, it prints the misses rather than a score, and reading the
  misses is what produced the fix.
- **Duration and headcount are unmeasured, not broken.** They fire on
  7 and 1 messages, but nobody has counted how many messages CARRY
  them; without that denominator those two numbers say nothing. Filed
  as part of the lane's own follow-up rather than acted on blindly.

## Results — intent-refit-gate (2026-09-07)

Stage 4 of the autonomy programme, done before the harvest that will
write into this path. Refitting the shipped artifact used to be
`Files.writeString(out, source)`: rows in, model out, no question
asked. The failure that guards against is not hypothetical — a
consumer of this module grew their corpus unevenly, one class reached
137 of 184 rows, their headline rose from 95.8% to 96.2%, and a class
died on the way. No aggregate says that.

### Interface
`Refit.propose(rows, heldOut, incumbent, rules)` fits a candidate,
scores BOTH it and the incumbent on the same held-out rows, and
answers a `Verdict` carrying every class's before and after:
`Accepted(model, scores, total)` or `Refused(why, scores, total)`,
where `why` names the class and the rule. `Refit.report(v)` prints
the table. Proposing is not publishing: nothing is written by this
module, ever.

Two rules, both from measurements already in this document:
- **the law** — no class below F1 0.50, the same rule `Models`' suite
  asserts, so a refit cannot hand that suite a model it will reject;
- **no slide** — no class may drop more than 0.10 against the
  incumbent even while staying legal, because 0.85 → 0.55 over three
  refits passes the law every time and is dead at the end.

### Behavior
- [x] the shipped corpus passes its own gate, and the report prints
      every class before and after (TestRefitGate)
- [x] a corpus with one `Other` row is REFUSED, naming it: "Other
      would be at F1 0.00, below the floor 0.50"
- [x] a two-class corpus for a four-class taxonomy is refused even
      with no incumbent — a first fit is held to the law too
- [x] a slide that stays above the law is refused under a stricter
      `slip`, so the rule is not decoration
- [x] `MakeModel` now goes THROUGH the gate: it prints the verdict,
      writes only on `Accepted`, and `--force` writes anyway while
      saying so. Run on the shipped corpus it accepts and reproduces
      the artifact byte for byte, so the reproducibility test that
      guards the blob still passes.

### Decisions
- **The verdict is data, not an exception.** A caller that publishes
  a refused model has to write `--force` in a shell history somebody
  can read, rather than catching something.
- **The gate comes before the data.** Every lane of the harvest
  programme ends in a refit; a guard added afterwards would be added
  after the first bad corpus, which is exactly when nobody is calm
  enough to design it.
- **The held-out half is the caller\'s to supply**, and the door says
  so: a corpus and the rows it is judged on come from the same place,
  and this module cannot check an overlap it was not shown.

## Results — intent-slot-denominators (2026-09-07)

The follow-up `intent-offline-slots` filed against itself: `duration`
fires on 7 messages of 120 and `people` on 1, and those numbers mean
nothing until somebody counts how many messages CARRY those slots.
Counted, with the same method that worked for `when` — a wide hint
list, then every miss printed and READ.

| slot | messages the hints suggest | extractor found | of the suggested |
|---|---:|---:|---:|
| `duration` | 19 | 7 | 37% |
| `people` | 5 | 1 | 20% |

### The answer: 7 and 1 are the fixture, not a gap
Reading the sixteen misses, not one is a duration or a headcount the
extractor should have found. They are three things:

- **substrings, not tokens** — the hint list matched "min" inside
  *reminder*, "hr" inside *chairing* and *through*, "long" inside
  *longer*. The extractors tokenise; the hint list did not, which is
  why a hint list is a denominator and never a verdict.
- **the same word meaning something else** — "take the **minutes** on
  Thursday" and "**Minutes** from the last meeting" are notes, not a
  length; "dial in **five minutes** early" is a duration standing
  next to a people-ish hint, not a headcount.
- **vague by nature** — "a **quick** chat", "the all-hands ran
  **long**", "forward it to the **people** who missed it", "would
  Wednesday suit **everyone**". No number was said, and inventing one
  puts a length or a headcount in a frame that nobody stated.

- [x] no code changed: there is nothing here to fix
- [x] the two refusals a widening would break FIRST are now asserted
      (`TestSlotRefusals`): a number beside a people word is not a
      headcount, and "minutes" as notes is not a duration. Someone
      will one day widen these extractors, watch coverage rise, and
      ship exactly those two mistakes.

### Decisions
- **A hint list measures the DENOMINATOR, never the extractor.** Both
  slot lanes now rest on that: print the misses and read them. Twice
  running, the reading changed the conclusion — for `when` it found
  two real bugs among false alarms, and here it found none at all.
- **`duration` and `people` need a corpus that uses them** before
  anything about them can be claimed. The meeting fixture barely
  does, which is a fact about the fixture and is now written down
  rather than mistaken for a defect.

