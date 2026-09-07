# okay-intent

Turning a message into a class and a filled frame — with a model, or
without one. Split out of [`okay-agent`](okay-agent.md) in
intent-module-split, at the request of a consumer who imported these
tiers and had never touched the agent loop.

The claim the whole module rests on is that a LABEL cannot be acted on
and a filled FRAME can: "Proposal" does not answer an email,
`Proposal(when, who, where)` does. Both come from one `Schema[I]`,
because FrameNet's Frame Elements ARE a product's required fields — so
the enumeration a model is shown and the decoder that reads its answer
are the same value, and a label outside the taxonomy is a decode error
rather than a class of parsing bug.

| | |
|---|---|
| `Classify` | the model tier: a prompt generated from `Schema[I]`, and a decoder that is the same value |
| `Taxon` | one taxonomy both tiers read — `of[I]` from a `Schema`, `parsed` from data |
| `Eval` | a confusion matrix as a `Monoid`, per-class scores, and `regressions` as an executable promotion rule |
| `Probe` | a linear probe over frozen embeddings, fitted by plain gradient descent |
| `Centroid` | class means; four vectors, no training loop |
| `Nearest` | kNN over example embeddings |
| `Patterns` | syntax cues (`shall we`, `could you`, `FYI`) — no network at all |
| `Induced` | the same kind of cue, induced from the corpus (RIPPER-shaped: grow, prune, keep at a support and a precision floor) — measured at sixty rows as coverage OR precision, not the hand-written tier's both |
| `CharGrams` | hashed character n-grams with a linear head — language-agnostic by construction; the window and the hash width interact (specs, intent-typo-robustness) |
| `WordTfIdf` | the classical baseline: a word vocabulary and IDF fitted on the training half, into `Probe`'s linear head — within three points of `CharGrams` on English, behind it where words are not the unit (ja, ru) |
| `Symbolic` | BM25 over labelled examples, via [`okay-rag`](okay-rag.md)'s `Postings` |
| `Static` | a distilled lookup table: embeddings without an encoder at request time; `units3` (words, pairs, triples) and `fitPca`/`projected` (the table cut to 256 dims, a quarter of the bytes, measured to lose nothing) are the best no-network configuration: 68.3% at 2.1 MB |
| `NoModel` | the assembly — stacking, and a conformal abstention whose promise is an `Option` |
| `Fitted` | every trained model as data, so fitting leaves the startup path |
| `Fit` | the door: fit a corpus, write the model down, read it back |
| `Demonstrations` | examples for a prompt SELECTED from a log rather than written beside it — one per class, deterministic, the scored messages excluded |
| `Models` | a fitted model that SHIPS — 73.3% of traffic at 88.6% precision with no network, or 100% at 80.0%; the caller picks |
| `Router` | the composed door: the measured tier order, and four outcomes |
| `Rows` / `ByLanguage` | a training row knows its language; a thin language borrows the pooled fit |
| `Temporal` | temporal phrases to ISO-8601 in the fixture's eight languages (en, fr, de, es, ru, uk, pl, ja), total and deterministic, refusing rather than guessing |
| `Duration` | duration phrases to minutes (`30 minutes`, `an hour and a half`, `1h30`, `une demi-heure`, `полтора часа`, `2時間半`) in the same eight languages as `Temporal`, the same two promises; `Slots.duration` is the slot |
| `People` | how many people (`four people`, `six of us`, `vier Personen`, `на четверых`, `4人用`), eight languages, the same promises; `Slots.people` is the slot; `Numbers` holds the number words both parsers read |
| `Amount` | how much money (`$20`, `twenty dollars`, `€15.50`, `1.000,50 €`, `двести гривень`, `dwadzieścia złotych`, `3000円`) as `Amount(value, currency)` with the currency an ISO code — a number beside a symbol, a code or an unambiguous currency name, composed number words (`two thousand five hundred`), separators told apart by the digits that follow; `Slots.amount` is the slot |

**The dependency is `String => Embedding`, not "a server".** The vector
tiers need a function; whether it is an HTTP call or an in-process
encoder is the caller's deployment, and where it is in process the
probe is the CHEAPEST tier rather than the dearest. Main compiles
against [`okay-codec`](okay-codec.md) and [`okay-rag`](okay-rag.md)
alone — no agent loop, which is the point of the split. The live
suites keep test-only dependencies on `okay-agent` (its journal, for
replaying recorded model answers) and `okay-llm` (a gateway).

Every claim here is measured, and the measurements — including the
ones that refuted earlier claims of mine — are in
[`specs/intent-classify.md`](../../specs/intent-classify.md), lane by
lane, with what each number cost and what it does not support.

Cross-built JVM + JS; the test suites are JVM-only, since several
summon a `Handler[Async]` that needs a `CanBlock` JS does not have.

## The slot descriptor, for a caller who wants to review it

It is not in this module. `Slot`, `Frame`, `Found`, `Answered` and
`Source` live in [`okay-frame`](okay-frame.md), which exists because
`okay.agent.Conversation` needed the same thing and neither module may
depend on the other.

```scala
val when = Slots.when(today)                    // okay-intent supplies parsers
val frame = Frame.of("MeetingProposal", when).in("ru")
frame.fillFrom(message)                         // what the message already says
     .answer("when", "next Tuesday")            // what a person then says
     .map(_.valueOf(when))                      // a Temporal.When, not a string
```

`Slot.choice` for a closed set of values with per-value wordings,
`assume` for a default nobody typed, `Source` for telling those apart,
and `okay.agent.Conversation` to run the whole exchange across a
restart. See specs/conversation.md.

## Multi-intent is one tier's property, not the module's

`Span` and `Reading` let a message carry two intents, and only the
MODEL tier can produce them. Everything that ships without a network —
`Patterns`, `CharGrams`, `Centroid`, `Probe`, and `Router` over them —
returns a single best class, so a two-intent message gets one label and
the other intent is dropped.

Measured on twelve two-intent messages (2026-09-05): the shipped path
answered all twelve, matched the first intent 3 times and either
intent 10; the cue tier's RUNNER-UP was the second gold intent 5 times,
and `Action.Act` discards it. The model tier, live against a local 4B,
returned two spans 6 times of 12, the right pair 5, the right pair in
the right order 4 — and every span it produced was a real stretch of
the message.

## Getting a model

Until 2026-09-04 this module measured nine tiers and shipped none of
them: every fitted model existed inside the test that fitted it.

```scala
import okay.intent.*

// what ships, with no network and no fitting at startup
Router.Router.offline().route(message) match
  case Router.Action.Act(intent, frame)            => act(intent, frame)
  case Router.Action.Ask(_, _, question, left)     => ask(question, left)
  case Router.Action.Escalate(candidates, why)     => person(candidates, why)
```

`Router` is the composition, not a new classifier: cues first (90.6%
where they fire, cost nothing), the vector tier next if the caller has
an embedder (85-88%, needs one), the shipped model last (61%, needs
nothing). `Router.of` refuses a tier whose classes are not in the
taxonomy, and `frames` says what each class needs before it can be
acted on — omit it and every class is actionable at once.

ONE CHOICE IS YOURS, and it is not a threshold: whether to load the
last tier. With it, everything gets a class and so does nonsense — the
shipped model's margin on garbage (median 0.437) is indistinguishable
from its margin on real English (0.434), so no floor separates them.
Without it, whatever the cues miss goes to a person. For calibrated
abstention use `NoModel`, whose threshold is conformal and comes with
a promise.

**The number depends on what you ask for, and for months this page
quoted the worst of them.** The door can answer everything at 80.0%,
or answer less at a higher precision, and the choice is one flag.
Measured over the same 60 held-out English messages
(`MeasureAutonomy`, intent-autonomy-report):

| what you need | best offline door | it answers | you hand over |
|---|---|---:|---:|
| precision ≥ 90% | cues only (90.6%) | 53.3% | 46.7% |
| precision ≥ 85% | cues + grams, margin ≥ 0.5 (88.6%) | 73.3% | 26.7% |
| an answer for everything | cues + grams, no floor (80.0%) | 100% | 0% |

Three quarters of the traffic can be answered at 88.6% with nothing
but the artifact and the cues. The cue tier answers the 53% it fires
on at 90.6%; the shipped n-gram model (character 2–3-grams into 4096
buckets, since intent-shipped-model-4096; it was 75.0% at 3–5-grams
into 1024) answers the rest.

**And that 80.0% is a ceiling, not an estimate.** Those held-out
messages were written by the same hand as the training ones, which is
worth about ten points: 70.0% on the half least like anything in
training (90.0% on the near half), 71.7% with one typo in the longest
word, 70.0% with the politeness frame removed, 76.7% lowercased.
Expect **70-72% from a message somebody else wrote**, and read the
table in `Models` before quoting the bigger number. A real second
author differs in vocabulary, length and structure at once, so even
70% is a lower bound on the gap.

**And per class, because a total hides a class.** Fifteen held-out
messages of each class, so the majority baseline is 25% and no single
class is carrying the number:

| class | precision | recall | F1 |
|---|---|---|---|
| `Proposal` | 0.78 | 0.93 | 0.85 |
| `Request` | 0.79 | 1.00 | 0.88 |
| `Notification` | 0.87 | 0.87 | 0.87 |
| `Other` | 0.75 | **0.40** | 0.52 |

`Other` is the row to read: the bigger model bought the total and
three classes and not this one — it still misses more than half the
messages that are not about meetings, so out-of-domain traffic lands
in a meeting class instead of out of the way. The 80.0% never said
that, and no aggregate would.

The reason is now measured rather than assumed. A dedicated
out-of-domain detector over the same rows RANKS well (AUC 0.843) and
still cannot be turned into a decision: by argmax it never fires (15
out-of-domain rows against 45 collapse the fit to the majority), and
a threshold buys 0.13 of recall for 0.4 points of accuracy, winning
on three random splits of eight (`intent-offline-other`). The blocker
is rows, not the algorithm — which is what `intent-other-more-rows`
is for, and why it needs human rows rather than generated ones.

It is fitted on 60 author-written English messages from this
repository's fixture; it is a worked example and a fallback, not a
general intent model, and not multilingual (a six-language fit scores
33-67% per language on fifteen held-out rows each, which is too thin
to stand behind).

For a real corpus:

```scala
val model = Fit.grams(rows)          // rows: Seq[(message, class)]
Files.writeString(path, Fit.save(model))
val loaded = Fit.grams(Files.readString(path))   // Either[String, Trained]
```

`Fit.save` / `Fit.probe` / `Fit.centroid` do the same for the tiers
that need an embedder. The shipped artifact is regenerated with
`sbt "okayIntentJVM/Test/runMain okay.intent.MakeModel"`, and a test
fails if what is committed is not what the generator produces.

## Working the model tier well — three measured rules

Everything above is the network-free path. When a model IS in the
loop, three findings decide how well it works, and each cost a lane
to establish. The measurements are in
[`specs/intent-classify.md`](../../specs/intent-classify.md); the
architecture they feed is
[`specs/intent-autonomy.md`](../../specs/intent-autonomy.md).

### 1. The taxonomy's case names ARE the prompt
`Schema[I]` reaches the model as a JSON Schema, so its identifiers
are prompt text. Strip the words out (`C1`..`C4`, field `s1`) and the
model does not degrade, it stops classifying: **0.685 → 0.100 macro
F1**, answering `C1` for every message with every reply still
decodable. A plain synonym costs 0.217 (`MeetingAsk` takes `Request`
recall from 0.67 to 0.07; `GatheringAdvisory` takes `Notification`
F1 to 0.00).

**So renaming a case is a model-facing change and must carry a
number**, exactly like changing the prompt — and name a class with
the plainest standard word, not a synonym. The deterministic tiers
read no identifiers at all, which is one more reason to push work
into them.

### 2. Demonstrations, taken from your own log, are the biggest lever
```scala
// recorded (message, intent) pairs — okay-chat reads its ChatLog,
// a test passes a list; nothing here opens a log itself
val demos = Demonstrations.perClass(recorded, exclude = scored)
val prompt = Classify.prompt[Meeting](message, demos)
```
One demonstration per class, chosen by the dullest rule that can be
stated in a sentence (first the log offers, in taxonomy order, the
messages about to be classified excluded), is worth **+0.207 macro F1
(0.685 → 0.892)** and takes undecodable replies **from 6/120 to
0/120**. `Demonstrations.fromReplies` builds those pairs from a log of
raw model answers, dropping what does not decode rather than
inheriting it.

They do NOT replace the names: with index names the same four
demonstrations reach only 0.376. Of the whole gap, examples buy 35%
and names 74% — they compose.

### 3. What the offline extractors fill, and what they refuse
`Frame.fillFrom` runs the extractors before anything is asked. On the
English fixture the `when` slot is filled for 29.2% of messages, and
of the messages whose words suggest a time the extractor finds 80%
(`MeasureSlotCoverage`). Two shapes it will NOT guess, on purpose: a
bare time with no day (`after 7pm`) and a range (`sometime this
week`) — `When` holds one date, and inventing one fills a frame with
something nobody said. Asking is the right move there.

## Where the programme is going

[`specs/intent-autonomy.md`](../../specs/intent-autonomy.md) holds the
plan for needing the model less: the two metrics it is judged by, the
seams every idea plugs into (`Labeler`, `Combiner`, `Acceptance`,
`Queue`, `Refit`, `Discovery`), the four approaches measured NOT to
help so nobody tries them again, and the staged lanes with their
criteria. Two results already in it are worth knowing before you
build on this module:

- **A learned combiner lost to the cascade.** Weighing the tiers by
  agreement (Snorkel's idea) scored 72.2% at 82.9% coverage against
  the cascade's 77.5% at 89.6% over eight splits. A cascade is the
  right shape when one labeler is much more precise than the rest and
  abstains cheaply; a weighted vote dilutes it.
- **Abstention pays, in both measurements that touched it.** 89.6%
  coverage at 77.5% beats 100% at 72.9%, and the worst class rises
  with it. If your caller can hand something over, let it.

