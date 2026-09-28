# okay-dlm

A deterministic dialogue language model, as a library
([specs/dlm.md](../../specs/dlm.md)). Not a network and not a
generator: one function `(state, utterance) -> (state, action, what
to say)`, assembled from layers that decide without a model — rules,
typos of a rule's word, centroids over a frozen encoder, what the
person taught — and proved by replay. Lifted from a service that ran
four languages through a hand-rolled version of it for a month,
because every part worth keeping was the part with no domain in it.

**What it holds is the mechanism. Every word, rule, phrase and slot
is the caller's, passed in.**

| | |
|---|---|
| `Dlm` | the model as one value: intents, router, named heads, detector, phrasing, confirm; `tiers` says what is live |
| `Intents` / `Intent` / `Slot` | the authored set — rules, phrasings by language, slots, `require`, `semantic`, help, rank — parsed and validated as data |
| `Router` → `Route` + `Support` | which intent and WHY: a lesson's exact band, the earliest-matching rule, a typo, a probability on a margin; `Missing` for a hole, `Unclear` as a first-class outcome; `noticed` for everything seen; `command` for an exact-argument intent |
| `Head` | one question by vectors or not at all — which act, yes/no/tell, remote, which frame — with a margin and a `quiet` class that silence already means |
| `Exemplars` | the compiled `(label, vector)` table with its encoder: `compile`, JSON to diff, checkpoint to boot from, `guard` against a changed encoder, `read`/`resource` binary-first |
| `Checkpoint` | safetensors: F32 or F16, labels as a UTF-8 column, `__metadata__` naming the encoder — refused by name when it disagrees, bytes a pure function of the numbers |
| `Memory` / `Lesson` | what the person taught, folded from journal `Event`s: bounded per person, shared at N people or by a teacher, withdrawn append-only; the exact band and the near band |
| `Decision` | the pure turn decision: `State`, `Pending`, `Standing`, `Evidence`, `Action`, and the `Record` a replay resumes from |
| `Language` | `Cues` (exclusive letters and everyday words, within the text's script), `Detector` (character trigrams over the authored phrasings, gated by alphabet), `Profiles` (the detector as an artifact) |
| `Confirm` | yes, no, or tell me more, read by rule before any head; a leading yes with its remainder; the offer's own verb as a yes; control words |
| `Phrasing` | the caller's read-backs, narrowing questions and field names by language, `{what}` filled, holes named |
| `Fuzzy` / `Script` / `Alphabet` | bounded Levenshtein over a vocabulary mined from the rules; which script, and which languages a word could be written in |
| `ModelChain` / `Lane` | the door: lanes in order, timeout, strikes and cooldown, a daily cap seeded from a journal, events for the caller's log |
| `Calibration` | reliability per layer, Brier where a probability exists, a rule's correction rate with people and sentences beside it |

**Depends on:** `okay-intent` (the probe, centroids, `Taxon`),
`okay-agent` (`ToolCall` for a lane's tool table); through them
`okay-frame`, `okay-rag`, `okay-codec`. JVM only: the checkpoint maps a
file and the door runs on threads.

## In sixty seconds

```scala
import okay.dlm.*

val intents = Intents.parse(json).toOption.get          // the caller's file
val embed: String => okay.rag.Embedding = …             // any encoder, or hashing for a test
val vectors = Exemplars.compile(intents.rows, embed, "minilm-l12")
Exemplars.write(Path.of("resources/intents.vec.json"), vectors)   // JSON and safetensors

val model = Dlm.of(intents, embed, Some(vectors),
  heads = Map("acts" -> (acts, 0.5f)), alphabet = Alphabet.of("ru", "uk", "pl", "en").toOption.get)

model.router.route("ищю сантехника")        // Fires("need", Map(what -> …), Typo(1))
model.head("acts").of("спасибо большое")     // Some("social"), or None: an answer
Decision.decide(State("ann", "ru"), evidence) // Action.Act(route) | AskPlainly | Menu | …
```

## Further

| | |
|---|---|
| [`specs/dlm.md`](../../specs/dlm.md) | the design, its decisions, and what stayed in the service |
| [`okay-intent`](okay-intent.md) | the tiers a head and the router are built from |
| [`okay-frame`](okay-frame.md), [`okay-agent`](okay-agent.md) | the form and the suspension the decision hands an intake to |
