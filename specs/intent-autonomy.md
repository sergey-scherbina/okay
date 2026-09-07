# intent-autonomy — working without a model, and learning from the logs to need it less

Status: specification, 2026-09-07. Owner lane family: `intent-*`.
Home of the autonomy metric, the harvest loop, and the seams both
plug into. The measurements this rests on live in
[specs/intent-classify.md](intent-classify.md); this document is the
ARCHITECTURE and the plan, written so that each future idea — ours or
the literature's — lands as an implementation of an existing seam
rather than as a rewrite.

## 1. What this is for

`okay-intent` answers "what does this message want" through tiers of
increasing cost: hand-written cues, hashed char n-grams, a static
centroid table, and — last — a language model. The programme is to
push the boundary: answer more without a network, hand over less, and
let the traffic that DOES reach the model pay for its own replacement
by becoming training data for the cheap tiers.

Two numbers say whether that is working, and they must always be
quoted together:

| metric | definition | today (held-out English, 60 messages) |
|---|---|---|
| **autonomy rate** | share answered with no network at all, and the precision among those answers | 73.3% at 88.6%, or 100% at 80.0%, or 53.3% at 90.6% — the caller chooses |
| **handed-over share** | the rest, which reaches a model or a person | 26.7% / 0% / 46.7% respectively |

`MeasureAutonomy` prints the whole curve (intent-autonomy-report).
The lesson of that lane is in the table: this line had been quoting
"80.0% at full coverage" for months, which is the WORST promise the
same tiers can make, because it never offered abstention.

## 2. The seams

Everything below is an interface with more than one plausible
implementation, and each cited technique in §4 lands in exactly one
of them. That is the test of the design: **a new idea should be a new
instance, not a new architecture.**

### 2.1 `Labeler` — anything that may propose a class, or abstain

```scala
trait Labeler:
  def name: String
  /** None means ABSTAIN, which is different from a wrong answer and
   * must stay different: a tier that abstains cheaply is what makes a
   * high-precision door possible at all */
  def vote(message: String): Option[Labeler.Vote]

object Labeler:
  final case class Vote(intent: String, confidence: Double, why: String)
```

Today's instances: `Patterns` (cues), `CharGrams` (the shipped
artifact), `Centroid` (the static table), the model door
(`Classify.prompt` + `Classify.read`), and a person.

The structural claim, which is the whole reason this seam exists: **a
run-time tier and an annotator are the same thing.** The cues that
answer a live message are the same function that votes on a logged
one. Snorkel calls this a labelling function; we already had four of
them and did not know it.

### 2.2 `Combiner` — how votes become one answer

> Measured 2026-09-07 (`intent-label-model`): the cascade BEAT a
> learned agreement combiner here, and the reason belongs in the
> design rather than in a results table. A cascade is the right shape
> when one labeler is much more precise than the rest AND abstains
> cheaply — it lets that labeler answer and steps aside, where a
> weighted vote dilutes it with weaker voices. Snorkel's setting is
> dozens of sources with none dominant. So this seam stays, with the
> cascade as its default and the condition for changing it written
> down: more independent labelers, or a combiner that models their
> dependence.

```scala
trait Combiner:
  def decide(votes: Vector[(String, Option[Labeler.Vote])]): Combiner.Decision

object Combiner:
  enum Decision:
    case Answer(intent: String, confidence: Double, by: String)
    case Abstain(candidates: Seq[String], why: String)
```

Today: a CASCADE — the first tier that fires wins (`Router.decide`).
That is one implementation of this seam and a poor one: it throws
away every agreement and disagreement behind the first answer.
Alternatives that fit without changing anything else: majority with
per-tier precision as weights; a generative label model learned from
agreement alone (§4.3); a cost-aware combiner that stops as soon as
the answer cannot change.

### 2.3 `Acceptance` — whether a proposed row may enter the corpus

```scala
trait Acceptance:
  /** Left says WHY not, in the words a reviewer will read */
  def accept(c: Candidate): Either[String, Row]
  def and(other: Acceptance): Acceptance
```

`Candidate` carries the message, the proposed intent, every tier's
vote, the model's self-consistency across k samples, and provenance
(model id, prompt fingerprint, date). Today's filters, each its own
instance: grounding (the span must occur in the message), a
confidence floor, k-sample agreement, and no high-margin
contradiction from a deterministic tier.

This is where noise control lives, and it is composable ON PURPOSE:
§4.5's denoising is another `Acceptance`, not a rewrite.

### 2.4 `Queue` — which rows a person should see

```scala
trait Queue:
  /** most valuable first; a person clears the top of it */
  def rank(candidates: Seq[Candidate]): Seq[Candidate]
```

Today: none — rows are written by hand, which is the bottleneck this
programme exists to remove. Next: uncertainty-guided ranking (§4.2),
tier disagreement, and class starvation (a class with 15 rows earns
priority over one with 60).

### 2.5 `Refit` — corpus to artifact, with the law as a gate

> Implemented 2026-09-07 (`intent-refit-gate`). `Refit.propose(rows,
> heldOut, incumbent, rules)` is the door; `Verdict.Refused` names the
> class and the rule it broke, and nothing in the module writes
> anything. Two rules: no class below F1 0.50, and no class down more
> than 0.10 against the incumbent — the second because three legal
> slides kill a class between them while every one of them passes.

```scala
trait Refit:
  def refit(corpus: Corpus): Either[Refit.Refused, Artifact]
```

`Refused` is data, not an exception, and it names the class that
would have died. The law: no class below F1 0.50 on held-out rows,
asserted by `MeasureAutonomy` and by the shipped-model suite. A
consumer already lived the failure this guards: their corpus grew
unevenly, one class reached 137 of 184 rows, the headline rose from
95.8% to 96.2% and a class died unnoticed.

### 2.6 `Discovery` — classes the taxonomy does not have yet

```scala
trait Discovery:
  def propose(unlabelled: Seq[String]): Seq[Discovery.Proposal]

object Discovery:
  final case class Proposal(name: String, examples: Seq[String], why: String)
```

`Other` is a diffuse bin with the door's worst numbers, and part of
the reason is that it is several real classes nobody has named. A
proposal is never accepted automatically: it is a suggestion a person
takes or drops, and the taxonomy is a type, so accepting one is a
code change with a measurement attached (see §5, and the rename
hazard in §3.4).

### 2.7 `Autonomy.report` — the metric, always available

```scala
object Autonomy:
  def report(door: Labeler, combiner: Combiner, corpus: Corpus): Report
```

Coverage, precision among answered, per class, and the handed-over
share. Every lane in §5 is judged by moving one of those two numbers
without breaking the law.

## 3. What we have already measured (do not re-derive)

### 3.1 Representation is not the limit
TF-IDF 61.7%, hashed char n-grams 68.3%, the static centroid table
68.3% at 2.1MB, and both embedders flat from 32 examples with the
same slope: four roads to one ceiling. A per-language check found
char-grams do NOT lose Russian (53.3%, above English's 40.0% on the
same 15-row slices), so morphology is not the gap either.
**Consequence:** a fifth encoder is not a lane; rows are.

### 3.2 Generated rows are worth nothing here
Three generators, zero gain; the distilled rows were a third as
diverse as the fixture and carried the generator's register.
**Consequence:** the model may LABEL real text (§4.1); it may not
WRITE the text.

### 3.3 The out-of-domain signal exists and is starved
A binary in/out detector over the same rows ranks at AUC 0.843 but
cannot be turned into a decision rule: by argmax it never fires (15
out-of-domain rows against 45), balanced it destroys the tier, and a
threshold buys 0.13 of recall for 0.4 points of accuracy, winning on
three random splits of eight.
**Consequence:** the blocker is rows, and it is quantified.

### 3.4 A taxonomy's names are prompt text
Stripping the words out of the taxonomy (C1..C4) collapses the model
tier from 0.685 to 0.100 macro F1; a plain synonym costs 0.217.
**Consequence:** accepting a discovered class (§2.6) is a
model-facing change and must carry a number. The deterministic tiers
read no identifiers at all, which is one more argument for pushing
work into them.

## 4. The literature, and where each piece plugs in

### 4.1 The model labels real text — `Acceptance`, not authorship
Pangakis & Wolken, *Knowledge Distillation in Automated Annotation:
Supervised Text Classification with LLM-Generated Training Labels*
([arXiv 2406.17633](https://arxiv.org/abs/2406.17633)):

> "Our findings indicate that supervised classification models
> fine-tuned on LLM-generated labels perform comparably to models
> fine-tuned with labels from human annotators. Fine-tuning models
> using LLM-generated labels can be a fast, efficient and
> cost-effective method of building supervised text classifiers."

They replicate 14 classification tasks to get there. Read together
with §3.2 this is the exact correction our failed distillation
needed: **real messages, proposed labels.**

The same shape for conversations specifically: *Efficient
Intent-Based Filtering for Multi-Party Conversations Using Knowledge
Distillation from LLMs* ([arXiv 2503.17336](https://arxiv.org/html/2503.17336v1)),
which distils an LLM into a small intent filter for
compute-constrained deployment — our okay-script container's problem,
stated by someone else.

### 4.2 People arbitrate the uncertain — `Queue`
Li, Shi, Ziems, Kan, Chen, Liu & Yang, *CoAnnotating:
Uncertainty-Guided Work Allocation between Human and Large Language
Models for Data Annotation*, EMNLP 2023
([arXiv 2310.15638](https://arxiv.org/abs/2310.15638)):

> "However, limited work has leveraged LLMs as complementary
> annotators, nor explored how annotation work is best allocated
> among humans and LLMs to achieve both quality and cost objectives.
> We propose CoAnnotating, a novel paradigm for Human-LLM
> co-annotation of unstructured texts at scale. Under this framework,
> we utilize uncertainty to estimate LLMs' annotation capability."

> "Our empirical study shows CoAnnotating to be an effective means to
> allocate work from results on different datasets, with up to 21%
> performance improvement over random baseline."

Our own active-learning lane measured the same shape independently
(28 chosen labels for the gain that took 36 random ones), which is
why this is a `Queue` implementation and not a research project.

### 4.3 The tiers are labelling functions — `Combiner`
Ratner, Bach, Ehrenberg, Fries, Wu & Ré, *Snorkel: Rapid Training
Data Creation with Weak Supervision*, PVLDB 11(3), 2017
([arXiv 1711.10160](https://arxiv.org/abs/1711.10160),
[pdf](https://cs.brown.edu/people/sbach/files/ratner-vldb17.pdf)).
Users write labelling functions expressing heuristics and patterns;
Snorkel applies them to unlabelled data and *learns a generative
model to combine the LFs' outputs into probabilistic labels* — from
their agreements and disagreements, with no gold labels. Reported:
2.8x faster model building in a user study, 132% average improvement
over prior heuristic approaches, within 3.60% of large hand-curated
training sets.

For us this is not a new dependency and not a new data need: our
cues, grams and static table already ARE those functions, and the
cascade is the weakest possible combiner over them. **This is the one
lane that improves the network-free door with zero new rows.**

### 4.4 Finding the classes nobody named — `Discovery`
Hong, Ng, Zhang, Song & Jiang, *Dial-In LLM: Human-Aligned
LLM-in-the-loop Intent Clustering for Customer Service Dialogues*,
EMNLP 2025 ([arXiv 2412.09049](https://arxiv.org/abs/2412.09049),
[ACL](https://aclanthology.org/2025.emnlp-main.300/)):

> "(1) examines the effectiveness of fine-tuned LLMs in semantic
> coherence evaluation and intent cluster naming, achieving over 95%
> accuracy aligned with human judgments; (2) designs an LLM-ITL
> framework that facilitates the iterative discovery of coherent
> intent clusters and the optimal number of clusters"

on a corpus of "over 100k real customer service calls with 1,507
human-annotated clusters". And Wang, Yang & Lin, *NILC: Discovering
New Intents with LLM-assisted Clustering*, WSDM 2026
([arXiv 2511.05913](https://arxiv.org/abs/2511.05913)), where

> "clustering assignments are judiciously updated by carefully
> refining cluster centroids and text embeddings of uncertain
> utterances with the aid of large language models"

— note *uncertain utterances* again: the same principle as §4.2, one
level down.

### 4.5 When the labels are noisy — another `Acceptance`, or a `Refit`
Ye, Shah, Zhang & Chava, *Calibrating Pre-trained Language
Classifiers on LLM-generated Noisy Labels via Iterative Refinement*
([arXiv 2505.19675](https://arxiv.org/abs/2505.19675)):

> "However, the reliability of such auto-generated labels remains a
> significant concern due to inherent inaccuracies. When learning
> from noisy labels, the model's generalization is likely to be
> harmed as it is prone to overfit to those label noises."

Their SiDyP recovers "an average of 7.21% and 7.30%" on zero-shot and
few-shot LLM-generated noisy label sets. We open this only when a
measurement shows noise is what limits our refit — the numbers say it
is worth about seven points, not that it is free.

## 5. The plan, staged, with criteria

Each stage names what it must move. A stage that misses its criterion
is recorded as a negative result and does not ship, as
`intent-offline-other` was.

| # | lane | needs new rows? | criterion |
|---|---|---|---|
| 0 | `intent-autonomy-report` — the metric | no | **DONE 2026-09-07** |
| 1 | `intent-label-model` — §4.3 combiner over existing tiers | no | **DECLINED 2026-09-07**: 72.2%/82.9% against the cascade's 77.5%/89.6% over 8 splits; correlated labelers inflate each other and one dominant labeler is diluted by a vote. Reopen with many INDEPENDENT labelers or dependency modelling |
| 2 | `intent-annotate-log` — §4.1 model labels real logged messages | produces them | 100+ kept rows; a refit moves the autonomy rate without breaking the law |
| 3 | `intent-coannotate-queue` — §4.2 uncertainty routing | reduces them | human effort per point of autonomy, against a random-selection baseline |
| 4 | `intent-refit-gate` — the law as a gate on every refit | no | **DONE 2026-09-07**: `Refit.propose` answers Accepted/Refused with every class before and after; the law and a slide rule; `MakeModel` writes only on Accepted |
| 5 | `intent-induce-on-harvest` — cue induction on the grown corpus | uses them | induced-cue coverage up at ≥ 0.85 precision (85.7% at 11.7% today) |
| 6 | `intent-discover-classes` — §4.4 discovery over `Other` | proposes them | proposals a person accepts; `Other` recall after the split |
| 7 | `intent-offline-slots` — slots with no network | no | a capability, not a number: slots filled offline at stated precision |
| 8 | `intent-noise-aware-refit` — §4.5 | no | opened only by a measurement showing noise is the limit |

Stage 1 first, deliberately: it is the only one that raises autonomy
with no new data, and it makes every later stage measurable through a
better combiner rather than through a better cascade.

## 6. Decisions

- **Seams, not a pipeline.** Each stage above is an instance of one
  interface in §2. The literature we have not read yet will also be
  an instance of one of them, or it will tell us a seam is missing —
  which is the useful failure.
- **Abstention is first-class everywhere.** `Labeler.vote` returns an
  `Option`, `Combiner.Decision` has `Abstain`, `Acceptance` returns a
  reason. A system whose only move is to answer cannot have an
  autonomy rate, only an accuracy.
- **Provenance travels with every row.** Model id, prompt
  fingerprint, filters passed, who confirmed. Without it a corpus
  cannot be audited, a regression cannot be traced to a batch, and a
  refit cannot be reproduced — and this repository has already been
  bitten by a prompt change that moved a fingerprint silently.
- **The taxonomy stays a type.** Discovery proposes; a person
  accepts; accepting is a code change carrying a measurement, because
  §3.4 measured what a name is worth.
- **Cost is a property of the design, not an afterthought.** Cheap
  labelers vote first, the combiner may stop when the answer cannot
  change, the queue bounds human effort explicitly, and the refit is
  offline. The model is the most expensive labeler and the programme
  is to need it least.

## 7. Out of scope

- Fine-tuning a small transformer on the harvest. The literature does
  it (§4.1); our constraint is a zero-dependency artifact that runs
  on JVM, JS and Native, and a BERT is none of those. If it ever
  becomes the answer it arrives as a `Labeler` behind a seam, like
  everything else.
- Multi-turn state tracking. This document is about one message's
  class and slots; the conversation belongs to `okay.agent.Conversation`.
- Cross-language transfer. `intent-per-language-models` is filed;
  until rows exist per language, the shipped artifact stays English
  and says so.
