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
| `CharGrams` | hashed character n-grams with a linear head — language-agnostic by construction |
| `Symbolic` | BM25 over labelled examples, via [`okay-rag`](okay-rag.md)'s `Postings` |
| `Static` | a distilled lookup table: embeddings without an encoder at request time |
| `NoModel` | the assembly — stacking, and a conformal abstention whose promise is an `Option` |
| `Fitted` | every trained model as data, so fitting leaves the startup path |
| `Fit` | the door: fit a corpus, write the model down, read it back |
| `Models` | a fitted model that SHIPS — 76.7% at full coverage with no network |
| `Router` | the composed door: the measured tier order, and four outcomes |
| `Rows` / `ByLanguage` | a training row knows its language; a thin language borrows the pooled fit |
| `Temporal` | English temporal phrases to ISO-8601, total and deterministic, refusing rather than guessing |

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

76.7% at full coverage on 60 held-out English messages over four
meeting classes — the cue tier answers the 53% it fires on at 90.6%,
and the shipped n-gram model answers the rest at 61%.

**And that 76.7% is a ceiling, not an estimate.** Those held-out
messages were written by the same hand as the training ones, which is
worth about ten points: 66.7% on the half least like anything in
training (86.7% on the near half), 66.7% with one typo in the longest
word, 65.0% with the politeness frame removed, unchanged under
lowercasing or a hedge. Expect **65-70% from a message somebody else
wrote**, and read the table in `Models` before quoting the bigger
number. A real second author differs in vocabulary, length and
structure at once, so even 65% is a lower bound on the gap.

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

