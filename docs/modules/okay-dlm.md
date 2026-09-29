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
| `Rule` | rules written as keywords instead of regexes: `word`, `prefix*`, `a phrase`; typo-tolerant like a hand-written trigger ([specs/dlm-rule-keywords.md](../../specs/dlm-rule-keywords.md)) |
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
| `Embedder` | the encoder as a seam — ours (`hashing`) by default, `static` for the distilled table, `of` for a model on disk, a remote one from `okay-dlm-remote` |
| `Judge` | the judge as a seam — which of these options is this text, with probabilities and a confidence; ours (`probe`) by default, `Fit` as the configuration seam, `orElse` and `guarded` around one that leaves the process |
| `Teaching` | who may teach what: own lessons, teachers, stewards, a kill switch per channel — ours the narrowest ([specs/dlm-learning.md](../../specs/dlm-learning.md)) |
| `Ledger` | every change to what the model knows, refusals included; append-only, `replay` is the fold, a JSON wire for a service's journal |
| `Explanation` | why a decision: the layer, the rule verbatim, the lesson and whose, everything noticed, the judge's ranking and name, the encoder, the tables by hash |
| `Governed` | the model read and corrected under rights: `explain`, `lessons`, `teach`, `forget`, `share`, `ledger` — learning never creates a class, edits a rule or moves a threshold, by the type |
| `Language.Detector` | the detector as a seam — `Trigrams` (ours) or `Judged` (a judge asked which language) |
| `Shelf` / `Kept` | every table a model has served, kept by the hash of the table as served (`Exemplars.stored`); ours a directory of checkpoints or memory; `get` refuses another encoder and a file whose content no longer hashes to its name |
| `Retention` | which old tables may leave the shelf — ours `all` (none), `latest(n)`, `within(ms)`, `any`, `of("last:5,days:30")`; `Shelf.prune` spares the newest and the serving whatever a policy says, and returns a `Pruned` entry per drop |

**Depends on:** `okay-intent` (the probe, centroids, `Taxon`),
`okay-agent` (`ToolCall` for a lane's tool table); through them
`okay-frame`, `okay-rag`, `okay-codec`. JVM only: the checkpoint maps a
file and the door runs on threads.

## In sixty seconds

```scala
import okay.dlm.*

val intents = Intents.parse(json).toOption.get          // the caller's file
val vectors = Exemplars.compile(intents.rows)             // by the Embedder in scope: ours unless a given says otherwise
Exemplars.write(dir.resolve("intents.vec.json"), vectors)   // JSON and safetensors

val model = Dlm.of(intents, Some(vectors),
  heads = Map("acts" -> (acts, 0.5f)), alphabet = Alphabet.of("ru", "uk", "pl", "en").toOption.get).toOption.get

model.router.route("ищю сантехника")        // Fires("need", Map(what -> …), Typo(1))
model.head("acts").of("спасибо большое")     // Some("social"), or None: an answer
Decision.decide(State("ann", "ru"), evidence) // Action.Act(route) | AskPlainly | Menu | …
```

## Rules as keywords

A rule is a regular expression, but most rules are only a list of
trigger words, so `Rule.keywords` writes the regex from the words. A
plain word matches a whole word in any case and any script, `payout*`
matches a word prefix, and a keyword with a space is a phrase over any
whitespace. The words are typo-tolerant: “pricng” still reaches
`sales`. Hand-written regexes still work and combine by `++`; in
intents JSON the same words go in a `"keywords"` array.

```scala
Intent("billing", rules = Rule.keywords("payout*", "invoice*"), semantic = false),
Intent("technical", rules = Rule.keywords("crash*", "outage", "bug"), semantic = false),
Intent("sales", rules = Rule.keywords("pricing", "upgrade"), semantic = false),
```

The whole example, a support-ticket triage set beside the Jev SDK's
own, is [the Jev examples guide](../guides/dlm-jev-examples-showcase.md).

## Backends: ours by default, any other by a given

Three functions are all the model asks of a "model": embed a
sentence, choose among named options, name a language. Each is a
trait whose companion holds our implementation as the `given`, so the
line below reaches no network and needs no file:

```scala
val model = Dlm.of(intents, Some(vectors), heads = Map("acts" -> (acts, 0.5f)))
```

…and one `given` at the composition root changes what every head and
the router's vector layer run on, with nothing else moving:

```scala
import okay.dlm.remote.*
given Embedder = Embedder.of("minilm-l12", 384, onnx.embed)            // the model on disk
given Judge.Fit = Judge.Fit.constant(Judge.orElse(                      // Jev first, ours behind it
  Jev.judge(sys.env("TYPESAFE_API_KEY")), Judge.probe(acts)))
val model = Dlm.of(intents, Some(vectors), heads = Map("acts" -> (acts, 0.5f)))
```

Jev (TypeSafe AI, hosted) and Laya (Convai, open, a container of your
own) speak one wire, so [`okay-dlm-remote`](okay-dlm-remote.md) is one
client and two configurations. A table compiled by another encoder
than the one in scope is refused by name at the door.

## Tables by hash: kept, reverted, dropped by a policy

A rebuild does not replace a table. It puts the new one on a `Shelf`
beside the old, both named by one hash: the hash of the table as a
boot will read it back, F16 rounding included. That is the string an
`Explanation` names, the string a `Ledger.Entry.Rebuilt` carries
before and after, and the name of the file on the shelf. A rebuild
that changed nothing hashes the same and records nothing.

A revert is a rebuild from the shelf: its entry's corpus is
`shelf:<hash>`, so it is audited and reverted like any build. Tables
leave the shelf only through `Shelf.prune` under a `Retention`. Ours
keeps everything, and no policy can drop the newest table of an
artifact or one being served. Each drop is a `Pruned` entry naming
the policy. `Ledger.File` keeps the entries as JSON lines beside the
shelf ([specs/dlm-learning.md](../../specs/dlm-learning.md) §9).

## Further

| | |
|---|---|
| [`specs/dlm.md`](../../specs/dlm.md) | the design, its decisions, and what stayed in the service |
| [`okay-dlm-remote`](okay-dlm-remote.md) | Jev and Laya as judges, a remote encoder, the wire seam |
| [Jev examples guide](../guides/dlm-jev-examples-showcase.md) | support-ticket triage on keywords, beside the Jev SDK's example |
| [`okay-intent`](okay-intent.md) | the tiers a head and the router are built from |
| [`okay-frame`](okay-frame.md), [`okay-agent`](okay-agent.md) | the form and the suspension the decision hands an intake to |
