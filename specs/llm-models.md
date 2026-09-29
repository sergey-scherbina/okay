# llm-models — the model catalog, residency and store, as capabilities a provider may lack

## Overview

okay-llm speaks to a model (`OpenAi.complete`, `Anthropic.stream`,
`Provider.openAi`/`anthropic` in okay-agent) and says nothing about
**which models exist**, which one is loaded, or how to load another. An
operator running a local gateway wants exactly that from a screen: the
catalog, the resident one marked, load, unload, pull, remove. The first
consumer is `../nadia`'s Models screen (nadia `docs/specs/app.md`; the
seam is nadia `SPEC.md` §10, which this file is the okay side of).

The operator's rule (2026-09-29): find the standard abstraction first, put
it in okay, and make rozum one adapter of it. There is no single standard
— hosted providers have nothing to load — but two de-facto ones cover it
between them, checked against each vendor's own documentation:

| | catalog | residency | store |
|---|---|---|---|
| OpenAI | `GET /v1/models`, `GET /v1/models/{id}` | — | — |
| Anthropic | `GET /v1/models`, `GET /v1/models/{id}` (`max_input_tokens`, `max_tokens`, `capabilities`) | — | — |
| Ollama | `/v1/models` and `GET /api/tags` | `GET /api/ps`; load = an empty-prompt request, unload = the same with `keep_alive: 0` | `POST /api/pull`, `DELETE /api/delete`, `POST /api/show` |
| LM Studio | `GET /api/v0/models` with a loaded state | `lms load`/`unload` | in the app |
| llama-swap | `GET /v1/models` | `GET /running`; load implicit in a request's `model`; `POST /api/models/unload[/:id]` | — |
| rozum | `GET /v1/models` | `GET /control/status`, `POST /control/switch`, `/control/unload`, `/control/reload` | `rozum models list\|pull\|rm` |

The catalog is OpenAI's shape and everyone returns it. The lifecycle verbs
are Ollama's and the local runtimes converged on them. The seam is their
union, with each part a capability a provider either has or does not.

## Interface

```scala
package okay.llm

object Models:
  /** one model as a catalog names it */
  final case class Model(id: ModelId, owner: Option[String], created: Option[Long],
                         contextTokens: Option[Int], maxOutput: Option[Int],
                         capabilities: Set[String])
  final case class Resident(id: ModelId, memoryBytes: Option[Long], expiresAt: Option[Long])
  final case class Weights(id: ModelId, bytes: Option[Long], digest: Option[String])
  enum Progress:
    case Bytes(done: Long, total: Option[Long])
    case Done
    case Failed(reason: String)

  /** what every provider has */
  trait Catalog:
    def list: Vector[Model] ! Async
    def info(id: ModelId): Option[Model] ! Async
  /** what a host with memory has */
  trait Residency:
    def running: Vector[Resident] ! Async
    def load(id: ModelId): Either[Refused, Unit] ! Async
    def unload(id: ModelId): Either[Refused, Unit] ! Async
  /** what a host with a disk has */
  trait Store:
    def local: Vector[Weights] ! Async
    def pull(id: ModelId): Unit ! Writer % Progress + Async
    def remove(id: ModelId): Either[Refused, Unit] ! Async

  final case class Refused(method: String, code: Int, description: String)

  /** adapters — the TYPE says what each can do; a hosted provider is a
   * Catalog and nothing else, so `.load` on it does not compile */
  def openAi(transport: Transport, apiKey: String, base: String = "https://api.openai.com"): Catalog
  def anthropic(transport: Transport, apiKey: String, base: String = "https://api.anthropic.com"): Catalog
  def ollama(transport: Transport, base: String = "http://127.0.0.1:11434"): Catalog & Residency & Store
  def rozum(transport: Transport, base: String): Catalog & Residency & Store

/** one model, several spellings: `org:repo`, `org/repo`, `hf:org/repo` —
 * one identity (nadia SPEC §8.5; rozum warmed a second resident copy
 * once because a string compare said they differed) */
opaque type ModelId = String
object ModelId:
  def apply(s: String): ModelId          // normalized: `hf:` stripped, `:` → `/` after the org
  def show(id: ModelId): String          // the canonical `org/repo` form
  def same(a: String, b: String): Boolean
```

`Transport` is okay-llm's existing one (`post(url, headers, body)` as a
stream of lines); a `get` is added for the catalog reads.

## Behavior

Catalog:
- [x] `openAi(...).list` decodes `{object:"list", data:[{id, owned_by, created}]}` into
      `Model`s with `contextTokens` and `maxOutput` `None`
- [x] `anthropic(...).list` carries `max_input_tokens` → `contextTokens`, `max_tokens` →
      `maxOutput`, `capabilities` → the set; pagination (`has_more`, `after_id`) is followed
- [x] `rozum(...).list` and `ollama(...).list` decode the same OpenAI form; `ollama` also
      reads `/api/tags` for `Weights` in `local`
- [x] `info(id)` on a spelling that differs from the catalog's (`org:repo` against `org/repo`)
      finds the model

Residency:
- [x] `rozum(...).running` reads `/control/status`; `load(id)` posts `/control/switch` and
      returns `Right` once the reply says resident, `Left(Refused)` with the gateway's words
      otherwise; `unload` posts `/control/unload`
- [x] `ollama(...).load(id)` posts an empty-prompt generate; `unload` posts it with
      `keep_alive: 0`; `running` reads `/api/ps` (`expires_at`, `size_vram`)
- [x] the type of `openAi(...)` and `anthropic(...)` is `Catalog` alone: a call to `.load`
      does not compile (asserted with a `compileErrors` test)

Store:
- [x] `ollama(...).pull(id)` tells `Progress.Bytes` as the streamed status lines arrive and
      `Done` at the end; a `Failed` carries the daemon's error line
- [x] `rozum(...).pull` shells nothing: it posts the gateway's control route, and where the
      gateway has none, the adapter's type says so (`Catalog & Residency` only, decided at
      implementation against the gateway of the day)

Identity:
- [x] `ModelId.same("mlx-community:Qwen3.5-4B-MLX-4bit", "hf:mlx-community/Qwen3.5-4B-MLX-4bit")`
      is true; `show` of either is `mlx-community/Qwen3.5-4B-MLX-4bit`
- [x] a catalog with `org/repo` and a residency answering `org:repo` mark the same model resident

All of it on a scripted `Transport`: no network in the suite. One live test per
adapter behind an env var (`OKAY_LLM_URL` for rozum, `OLLAMA_URL`), skipped loudly
when unset, as `TestLive` does today.

## Out of scope

- Choosing a model for a completion. That is the `model` field of a request, unchanged;
  this seam lists and manages, it does not route.
- Hugging Face Hub as a store (weights download). It fits `Store` and is a later adapter.
- Admission, queues, multi-model residency policy. The gateway's; `load` asks and reports.
- A CLI. nadia draws a screen; `okay-ops` may add a route; neither is here.

## Design

- **Capabilities as intersection types, not a flag.** `rozum(...)` returns
  `Catalog & Residency & Store`; `openAi(...)` returns `Catalog`. A consumer that
  wants to draw a `Load` button asks for `Residency` and gets it only where it exists.
  This is the okay-platform rule (a platform contributes evidence, not API — "can I
  park a thread?" is a compile error, not a runtime failure) applied to a provider.
- **The catalog is one decoder, four bases.** OpenAI, rozum, Ollama and llama-swap
  return the same JSON; Anthropic adds fields. One `Catalog` implementation
  parameterized by base and header, plus Anthropic's superset decoder.
- **`ModelId` is opaque and normalized on construction**, so every comparison inside
  the seam is a string equality of canonical forms and the spelling rule lives in one
  place.

## Decisions

- **Ollama's verbs for the lifecycle, OpenAI's shape for the catalog** — chosen
  because those are the two an operator arriving from any local runtime already
  knows; inventing a third vocabulary would cost every reader a translation.
- **A missing capability is a missing method** — chosen over `Either[Unsupported, …]`
  on every call because a screen drawn from the type has no button that answers
  "not supported", which is the failure the consumer's spec forbids (nadia `SPEC.md`
  §3.2's rule for confinement, applied here).
- **Load is a request, not a command** — `rozum` posts `/control/switch` rather than
  shelling `rozum gateway switch`, so the adapter works against a gateway on another
  machine and needs no binary on the path.

## Implementation lane

`llm-models` — okay-llm, additive (new file `Models.scala`, `Transport.get`, new
suite `TestModels`). Consumer: `../nadia` `app/` Models screen.

## Results

Implemented 2026-09-29 (lane `llm-models`), with four things decided
against the interface as first written, each for a reason found in the
code or the vendor's wire:

- **`Fetch` beside `Transport`, not `Transport.get`.** `Transport` has one
  abstract method and nine implementations in this repository's tests; a
  second abstract method would have made an additive lane a change to
  every one of them. `Fetch` (`get`, `delete`) is a second trait; the two
  real transports return `Transport & Fetch`, a test writes what it needs.
- **`ModelId` is a class, not an opaque type.** Equality had to hold across
  spellings, which an opaque type over `String` cannot give; and Ollama's
  ids are `name:tag`, so the canonical `org/repo` form is the KEY the
  identity compares by, while `spelling` — what the provider said — is
  what goes back on the wire. `show` is the key.
- **rozum's residency is read off `/v1/models`, not `/control/status`.**
  The gateway's own list marks its resident row (`resident: true`) and
  carries the real spec in `display_name` behind a Claude-shaped alias
  `id`; `/control/status` reports the HOST's residents across gateways,
  which is a different question. The adapter calls a model by its spec.
- **rozum is `Catalog & Residency`.** `rozum models pull|rm` is a CLI with
  no route; the type says so rather than a `Store` whose methods fail.

`TestModels`, 9 tests, cross (JVM and JS): a scripted wire answers by
verb and path; the compile-error test proves `.load` on a hosted
provider does not exist. `Iso.epochMs` parses Ollama's `expires_at` on
every platform (no java.time on JS).
