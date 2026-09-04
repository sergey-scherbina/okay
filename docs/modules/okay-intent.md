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
