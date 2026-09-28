# okay-dlm-remote

The model's remote backends ([specs/dlm.md](../../specs/dlm.md),
"Backends"): what a `given` names when the default — ours, in
[`okay-dlm`](okay-dlm.md) — is not the choice.

| | |
|---|---|
| `Wire` | one POST, one body back, a `Left` with a sentence when it fails; ours is `java.net.http` with a deadline, `Canned` is what every test passes |
| `SystemOne` | the typed-question wire (`POST /v1/systemone`: `choice`, `score`, `noul` over a `state`; `answers` with `probabilities` and `confidence`) — the codec, pure, and a `Client` whose `judge` is the model's `Judge` |
| `Jev` | TypeSafe AI's hosted System One model: the host, `TYPESAFE_API_KEY`, the name a verdict carries; `fromEnv` |
| `Laya` | Convai's open System One model, served from a container of your own (`pip install "laya[serve]"`, `LAYA_API_KEY` optional); `fromEnv` |
| `SystemOne.Service` | OUR MODEL ON THE SAME WIRE: `serve(body, judges)` answers a Jev/Laya-shaped request from our own judges — a question reaches the head whose classes it names, `noul` is a yes/no choice, `score` an expectation over the levels, an unanswerable question an `error` in its own slot, and no `confidence` we do not have |
| `Embeddings.openAi` | a remote encoder over the OpenAI-shaped `/v1/embeddings` wire, named by host and model so a foreign table is refused by name |

**Depends on:** `okay-dlm` (and through it `okay-intent`, `okay-codec`).
JVM only.

The wire runs both ways: `SystemOne.Service.serve` answers the same
request from our judges, so a client written against Jev or Laya runs
against this model unchanged, and three judges are measured through
one client. What it never sends is a `confidence` our probe does not
have — the field appears only where a judge answered one.

Both vendors are one protocol — Laya documents its wire as identical
to Jev's — so a caller measures the hosted model and the open one on
the same held-out rows through the same code. Every remote judge here
comes wrapped in `Judge.guarded`: a wire that fails is an abstention,
a run of failures retires the judge for a cooldown, and `Judge.orElse`
keeps ours answering behind it. Built against the vendors'
documentation of 2026-09 and measured against nothing here; the
source implementation read Laya and declined it as its default (okay-chat
specs/model.md §7), which is why the choice is the operator's and
measured, not the library's and assumed.

```scala
import okay.dlm.*, okay.dlm.remote.*

given Judge.Fit = Judge.Fit.constant(Judge.orElse(Laya.judge(), Judge.probe(acts)))
val head = Head.of(Some(acts), margin = 0.5f, quiet = Some("answer"),
  instructions = "What kind of move is this message?",
  descriptions = Map("social" -> "a pleasantry", "correct" -> "a correction of what we understood"))
```

## Further

| | |
|---|---|
| [`okay-dlm`](okay-dlm.md) | the model, and the seams these plug into |
| [`specs/dlm.md`](../../specs/dlm.md) | the design and its decisions |
