# okay-dlm — a deterministic dialogue language model, as a library

Status: specification and stage 1, 2026-09-28. Owner lane family:
`dlm-*`. Lifted from a service in this workspace (Okay!Chat) that ran
four languages through a hand-rolled version of everything below for
a month, at the operator's word: *«в открытой библиотеке окей должно
быть всё кроме данных — а в окей-чат должны быть данные (диалоги и
т.д.)»*.

## 1. Overview

A dialogue language model here is not a network and not a generator.
It is one function

    (state, utterance) -> (state, action, what to say)

assembled from DETERMINISTIC layers and proved by replay: the same
build over the same journal reaches the same decisions. Its "weights"
are tables of vectors a frozen encoder produced over phrases a person
wrote; its learning is a person in the loop; nothing on the request
path generates a word, and the one place a generative model may run
is a door with a budget, holding tools and forbidden to state a fact
no tool returned.

`okay-intent` answers "what class is this message" (specs/intent-classify.md);
`okay-frame` and `okay.agent.Conversation` answer "what happens next
inside one intake" (specs/conversation.md). What neither answers, and
what the consumer had written by hand, is the WHOLE: which layer
decides first and what happens when it is silent; what kind of move a
message is; what was answered to a yes/no question; what the person
taught last week; which language to answer in; what gets written down
so a replay resumes from the verdict and not the text; and the
container all of that ships in. That is this module.

The organising claim, the same one specs/conversation.md made for the
intake: **all of it is mechanism.** Strip the source implementation of
its subject matter — a marketplace, four languages, twenty-eight
intents — and what is left routes, reads, decides, remembers,
records and refuses, and none of those sentences mention what the
conversation was ABOUT. What a caller owns is exactly that missing
noun: the intents and their rules, the phrases, the words in every
language, the slot readers, the gazetteer, and the performing half
that acts on a decision. **No words live here, in any language.**

## 2. Interface

One value names the whole and every part is usable alone:

```scala
final case class Dlm(intents: Intents, router: Router,
                     heads: Map[String, Head], detector: Option[Language.Detector],
                     phrasing: Phrasing, confirm: Option[Confirm])
```

| part | answers | the caller supplies |
|---|---|---|
| `Intents`, `Intent`, `Slot` | the authored set: rules, phrasings by language, slots, `require`, `semantic`, help, rank | the file |
| `Router` → `Route` with `Support` | which intent, and WHY: `Exact(rule)`, `Typo(d)`, `Semantic(p, runnerUp)`, `Remembered(lesson, near)`; `Missing(intent, slot)`; `Unclear(candidates, score)` | the intents, optionally `Exemplars` and an encoder, a margin, an `Alphabet` |
| `Head` | one question by vectors or not at all: which act, yes/no/tell, remote or present, which frame — with a margin and a `quiet` class | the `Exemplars` per head, the bar |
| `Exemplars` | the compiled table: `(label, vector)` rows and the encoder that made them; JSON to diff, checkpoint to boot from | the `(label, phrase)` rows and the encoder |
| `Checkpoint` | the container: safetensors, F32 or F16, labels as an Arrow-style UTF-8 column, `__metadata__` with the encoder, refused by name when it disagrees | — |
| `Memory`, `Lesson` | what the person taught, folded from journal `Event`s: the exact band before the rules, the near band by cosine | the events and the `Rules` |
| `Decision` with `State`, `Pending`, `Standing`, `Evidence`, `Action`, `Record` | the pure turn decision, and the record it leaves so a replay resumes from the verdict | the state and the evidence, lazily |
| `Language.Cues`, `Language.Detector`, `Language.Profiles` | which language: exclusive letters and everyday words first, then character trigrams over the authored phrasings, and never a guess | the word lists, the `Alphabet`, the phrasings |
| `Confirm` with `Words`, `Answer` | yes, no, or tell me more — read by rule, before any head; the offer's own verb as a yes | the words in every language |
| `Phrasing` | the caller's read-backs, narrowing questions and field names, one cell per key with every language beside it, and the holes named | the file |
| `Fuzzy`, `Script`, `Alphabet` | bounded edit distance over a vocabulary mined from the rules; which script a word is in and which languages it could belong to | the languages spoken |
| `ModelChain`, `Lane` | the door: lanes in order, timeouts, strikes and a cooldown, a daily cap seeded from the journal, events for the caller's log | the lanes as plain functions |
| `Calibration` | when it said 0.9, how often was it right — per layer, never across them; Brier only where a probability exists | the `Seen` rows read from its journal |
| `Embedder` | THE ENCODER, as a seam: a name, a width, a function — ours by default (`hashing`), a static table, any model on disk (`of`), a remote one (`okay-dlm-remote`) | a `given Embedder`, or nothing |
| `Judge` with `Question`, `Choice`, `Fit` | THE JUDGE, as a seam: which of these options is this text, with probabilities and a confidence — ours by default (a probe over the exemplars), Jev or Laya over the wire; `orElse` and `guarded` around one that leaves the process | a `given Judge.Fit`, or nothing |
| `Language.Detector` with `Trigrams`, `Judged` | THE DETECTOR, as a seam: ours over the authored phrasings, or a judge asked which language | the phrasings, or the judge |

Languages are CODES, as in `okay.frame`: a caller with an enum passes
its code, and a code can be written down in a journal, which an
opaque type cannot.

## 3. Behavior

- [x] the router decides in four layers — a lesson's exact band, the
      earliest-matching rule, a typo of a rule's word, a probability
      over the exemplars — and `Unclear` is a first-class outcome
- [x] a required slot that is missing is `Missing`, never a fire with
      a hole in it and never `Unclear`
- [x] the typo vocabulary is mined from the rules already written and
      never crosses a language when an `Alphabet` is given
- [x] the vector layer needs enough letters; a fragment is asked about
- [x] `noticed` reports every intent the layers saw while `route`
      acts on the first, so a second fact is never dropped in silence
- [x] a head answers above a margin of probabilities and is silent
      below it, and silence means the caller's own default
- [x] the same exemplar table ships as JSON and as a checkpoint, the
      checkpoint is read first, a checkpoint by another encoder is
      refused by name, and the bytes are a pure function of the numbers
- [x] the decision is a pure function of a `State` and an `Evidence`,
      with the fallback ladder in the order the cost of being wrong
      dictates: courtesy, correction, a narrowing question, one plain
      question, the menu, shorter
- [x] the record is a function of the action, `recall` reads the
      action back, and a record written before actions had names
      recalls nothing rather than guessing
- [x] the memory is a fold over journal events: the latest teaching
      of one sentence wins, a withdrawal is honoured, a person is
      bounded, a pair enough people taught is shared
- [x] a language is never guessed from a message with no evidence
- [x] a yes/no is a whole short reply of answer and courtesy words; a
      yes that leads a longer message hands back the rest
- [x] the door skips a lane that throws, times out or answers empty,
      retires it after `retireAfter` strikes, and closes it at the
      daily cap
- [x] every model backend is a seam with ours behind it by default: the
      encoder (`Embedder`), the judge (`Judge`), the language detector
      (`Language.Detector`); a `given` at the composition root replaces
      one everywhere it is summoned and nothing else moves
- [x] a remote judge or encoder plugs in through one wire (`Wire`),
      faked in every test; a remote judge that fails abstains, a run of
      failures retires it, and ours behind it keeps answering
- [x] a table compiled by another encoder than the one in scope is
      refused by name at the model's door (`Dlm.of`)
- [ ] the memory's near band served by default — waits on a bar a
      consumer has measured on its own rows (the source keeps it OFF)
- [ ] a compile door that takes a corpus directory and writes every
      artifact — stage 2, once a second consumer names its shape
- [ ] cross-built for JS — the checkpoint maps a file and the door
      runs on threads; a JS leg needs neither and could carry the rest

## 4. Design

**One table shape for every head.** The source implementation had four
classes — acts, answers, presence, frames — with identical bodies and
different corpus files, and a fifth for intents with the same
`(label, vector)` rows under a different name. `Exemplars` is that
shape once and `Head` is that body once; what differs is the corpus
and the bar, and both are the caller's.

**The scores are typed because the wire is not.** A rule's 1.0, a
typo's `1 - 0.2·d`, a probability and a cosine were one `Float`
beside a layer name, and the first calibration over a live journal
found what happens when somebody compares them anyway: corrected
thirty percent of the time at "1.0". `Support` carries the layer and
the evidence; the wire keeps carrying `by` and `score` because a log
outlives a type.

**The decision reads a snapshot and never the world.** `State` holds
what the decision reads and nothing else; `Evidence` is a trait so an
expensive head is asked only on the branch that needs it; the stuck
count is read here and incremented by the caller, because
incrementing is performing. Every branch is listable and testable
with constants, which is what the nested method it replaced could not
be.

**The record is a function of the action.** Which fields a journal
record carries is behaviour: a log that says only "an answer arrived"
cannot tell a later reader whether the service diverted it or failed
to parse it, and those want opposite fixes. An `Unclear` verdict IS a
verdict; the source found that not writing it down had filed every
message nobody understood as the intake working, 201 of 292 turns
indistinguishable.

**The memory is a projection, not a file.** `Memory.of(events)` is a
pure fold, so a boot arms exactly what the log says and a replay that
re-performs recorded actions learns nothing. Its exact band decides
BEFORE the rules, because the person's own sentence overruled a rule;
its near band is off until a bar is measured.

**The alphabet is data about languages, not about a domain.** Which
letters exist in Russian and not Ukrainian is linguistics and belongs
here beside `Temporal`'s eight languages; which words a service's
users say is the service's. So `Alphabet.known` is a table this
library keeps and `Language.Cues.words` is the caller's.

**The container is borrowed and the policy is ours.** safetensors is
eight bytes, a JSON header and tensor bytes; what the library adds is
`__metadata__` — format, encoder, dimension — and the refusal.
Strings ride as an Arrow-style UTF-8 column so it stays one file and
one parser, and `tools/read-checkpoint.py` in the source repository
is an independent reader written from the specification.

**Backends: an abstract core, pluggable implementations, ours by
default, alternatives by a `given`** (the operator, 2026-09-28: «все
компоненты сразу делай так — абстрактное ядро как набор интерфейсов
и подключаемые реализации и дефолтная конфигурация с перегружаемыми
через имплиситы альтернативами»). Three functions are all a
deterministic model ever asks of a "model": embed a sentence, choose
among named options, name a language. Each is a trait with our
implementation as the `given` in its companion — the lowest-priority
place a given can live, so any `given` a caller writes wins without an
import to shadow. The doors that build the model (`Dlm.of`,
`Router.of`, `Head.of`) summon them; the doors the first consumer
wrote (`Router.apply`, `Head.apply` with a plain function) stay and
are ours by construction. A remote implementation never sees the
exemplars: `Judge.Fit.constant` ignores the table, and what the judge
is told instead is the `Question` — the option names with a
description each and an instruction — which ours ignores. So one
head is built the same way whatever answers it, and the journal says
who did.

**Jev and Laya are one protocol, so one client and two configurations.**
Laya documents its wire as identical to Jev's (`POST /v1/systemone`,
`choice`/`score`/`noul`, `answers` with `probabilities` and
`confidence`), which is what lets a caller measure the hosted model
and the open one against the same rows through the same code. Built
against the vendors' documentation of 2026-09 and measured against
nothing here: the source implementation read Laya and declined it as
its default (okay-chat specs/model.md §7), and a caller that switches
its judge owes itself the same measurement on its own held-out rows
first. The hosted endpoint's host is a `Config` field, not a promise.

**JVM only, for now.** The checkpoint maps a file and the door runs on
a thread pool; everything else is portable and a JS leg is an
`unmanagedSourceDirectories` split away, taken when a consumer needs
it rather than before.

## 5. Decisions

**The words the source carried are NOT lifted.** Twenty-eight intents,
a thousand rules, three thousand lines of phrases in four languages,
the yes/no lists, the everyday-word lists: all of it stays in the
service, and the library's `Confirm.english` exists only so the reader
works before a caller authored a word. The first disagreement about
tone must not become a pull request against a library.

**`Route.Distinguish` is one name in the log.** The source recorded
the need/offer pair as `which-side` and every other pair as
`distinguish`, because its domain knew that one pair by itself. The
library knows no pair; `Evidence.distinguishable` answers for all of
them and the record says `distinguish`. A consumer with a log written
under the old name keeps its own `recall` for that name.

**`Standing`'s cases are the library's, its rows are the caller's.**
`Shown`, `Offered`, `Asked`, `Searched` are the shapes a context
reading has; the nineteen rows that say which intent each fires are a
service's table, and `Standing.collisions` is the check the service
asserts empty.

**`Exemplars.print` writes `label`, and `parse` reads `intent` too.**
The source's artifacts are keyed `intent` because the first head was
the intent head; a build output outlives its format, and refusing a
file over a key would make the migration a recompile for no numbers.

**The compile door is not here yet.** The source's `Compile` reads
seven corpus files, writes nine artifacts, compacts a gazetteer and
prints an evaluation; nearly all of it is a service's own shape. What
is generic — compile rows, guard the encoder, write both formats — is
`Exemplars.compile`, `guard` and `write`, and a consumer strings them
together in twenty lines. A door that takes a directory waits for a
second consumer to say what its directory looks like.

## 6. Out of scope

- The performing half: what `Action.Act(route)` DOES is a service's
  tool table, store and journal.
- The intake itself: `okay.agent.Conversation` over `okay.frame`.
- Slot reading by span: `okay.intent.Spans`, `Temporal`, `Amount`.
- A generative model's own guard against invented facts: it reads the
  caller's tool results and belongs where those are.

## Results — dlm-module (2026-09-28)

Stage 1: the module, lifted and generalised. Fourteen files, 81 tests
green through `okayDlm/test`, every one over the hashing embedder so
nothing needs a model on disk. Three things changed on the way from
the source and each is a decision above: one `Head` for five classes,
languages as codes with an `Alphabet` where the source hard-coded four
letter sets, and the record decoupled from a service's journal row.

Found while lifting: the source's mixed-script check used
`[^\W\d_]+`, which in Java is ASCII unless `(?U)` is set — so it
matched Latin letters only and could never see a Cyrillic prefix in
front of a Polish verb, which is the very case it was written for.
`Intents.validate` reads `\p{L}+` after taking the regex escapes out,
and the test holds «наprawiam» down.

Stage 2, filed and not started: Okay!Chat switches to this module and
deletes its copies — `Router`, `Fuzzy`, `Acts`/`Answering`/`Presence`/
`Frames`, `Decision`, `TurnState`, `Standing`, `Memory`, `Checkpoint`,
`CompiledIntents`, `Lang.Detector`/`Profiles`, `Phrasing`, `ModelChain`
and `Calibration`'s arithmetic — about 3 300 lines, keeping its corpus,
its `Catalog`, its `Phrases`, its readers and its `Executor`.

## Results — dlm-backends (2026-09-28)

Stage 3: the seams. `Embedder` (ours: hashing; `of` for a model on
disk; `static` for the distilled table), `Judge` (ours: the probe;
`Question`/`Choice`; `Fit` as the configuration seam; `orElse`,
`guarded`), `Language.Detector` (ours: `Trigrams`; `Judged`), and
`Dlm.of`/`Router.of`/`Head.of` summoning them. `okay-dlm-remote`:
`Wire` (ours: java.net.http; `Canned` for a suite), `SystemOne` (the
codec, pure, and the client), `Jev`, `Laya`, `Embeddings.openAi`.
89 + 8 tests, every remote one over a canned wire that asserts on the
request it saw. The old constructors kept: the consumer compiled
against the new pin without a change to its model code.

## Results — dlm-serving (2026-09-28)

Stage 4: our model on the same wire. `SystemOne.Service` in
okay-dlm-remote — `decode` of the vendors' request shape (`state` as
text, body, every string field or a bare string; `questions` with
`criteria` as an object or a list), `answer` per question by the
first judge that can rank its options, `serve` as a status and a body
for any route. Four tests, one of them the round trip: our client
reads our server's answer, so the wire is one. The consumer mounts it
as `/v1/systemone` beside `/route`. The learning mode is
specs/dlm-learning.md, written before its code.
