# Conversation runtime — a program that waits for a person

## Overview

specs/intent-classify.md answers "what is this message". This answers
"what happens next", and the two are not the same problem. A
classifier is a pure function and stays one. A conversation has state,
outlives the process that started it, and its next step depends on a
message that may not be an answer at all.

The organizing claim: **all of that is mechanism.** Strip a working
implementation of its subject matter and what is left asks the next
unanswered question, asks again when the answer cannot be read, reads
back what it understood before writing anything, keeps the language it
started in, reaches someone who is no longer present, and remembers
where it was across a restart. None of those sentences mention what
the conversation was ABOUT.

What a caller owns is exactly that missing noun: which frames exist,
which slots they have, what those slots ask, and the words. The
boundary is the point of this spec.

Written from an implementation that exists and works — a service in
this workspace routes four languages through a hand-rolled version of
everything below. It is offered upstream rather than kept because
every part of it that is worth keeping is the part with no domain in
it, and the parts that went wrong went wrong for reasons another
implementation would hit identically. Those are recorded in Decisions
rather than smoothed over.

## Interface

The caller describes; the runtime drives.

```scala
/** one question and what its answer means. The caller supplies these;
 * nothing here knows what a `name` refers to */
final case class Slot(name: String,
                      /** the words are the caller's, per language */
                      ask: Lang => String,
                      /** what the answer MEANS. `None` is not a
                       * failure to store, it is a question to ask
                       * again — see Decisions */
                      read: String => Option[Value] = s => Some(Value.text(s)),
                      /** what the OPENING sentence already answered,
                       * so the intake does not ask for what it was
                       * just told */
                      extract: String => Option[Value] = _ => None)

/** the shape of one kind of request */
final case class Frame(name: String,
                       slots: Vector[Slot],
                       /** which slot the opening sentence answers, if
                        * any — a caller whose opening names the thing
                        * asked for by the first slot says so here */
                       opening: Option[String] = None,
                       /** read back what was understood and wait for a
                        * yes before anything is written */
                       confirm: Boolean = true)
```

What arrives at a suspension is a CHOICE, never a string:

```scala
enum Reply:
  case Answer(text: String)
  /** an exact command, recognised by the caller's own deterministic
   * layer — this is what aborts an intake */
  case Interrupt(intent: String)
  case Yes
  case No
```

What the runtime emits is a message KIND, for the caller to render:

```scala
enum Say:
  case Ask(slot: Slot)
  case AskAgain(slot: Slot)                       // `read` said None
  case ReadBack(frame: Frame, values: Map[String, Value])
  case Filled(frame: Frame, values: Map[String, Value])
  case Abandoned(frame: Frame)                    // No, or an Interrupt
```

And the intake itself is a program, not a table:

```scala
def intake(f: Frame, opening: String): Conv[Option[Map[String, Value]]]
```

`Conv` suspends at every question. The suspension is journalled
(specs/llm-agentic.md, `Durable`), so the program that resumes need
not be the process that asked, and the answer may arrive days later.

## Behavior

- [x] a suspension survives a restart: the pending question is in the
      log, and a fresh process resumes it without a stack
- [x] a resumed value is a `Reply`, not a `String` — the caller's
      deterministic layer decides which case it is
- [x] only an `Interrupt` aborts an intake; a fuzzy or a similarity
      guess never does
- [x] a value `read` rejects is asked again ONCE and then accepted as
      text
- [x] the language is resolved once for the exchange and pinned to it
- [x] with `confirm`, nothing reaches the caller as `Filled` until a
      `Yes` follows a `ReadBack`
- [x] an opening sentence answers `opening` and every slot whose
      `extract` reads it, and those are not asked
- [ ] replay reconstructs from RECORDED decisions and asks nothing,
      sends nothing, and reaches nobody
- [ ] a message to someone not present is a recorded DECISION plus a
      delivery at the edge, and a replay repeats neither

## Out of scope

- **The words.** The runtime names message kinds; the caller renders
  them. See Decisions.
- Which frames exist, and choosing one for a message — that is the
  caller's domain classifier over specs/intent-classify.md.
- Anything about a store. `Filled` hands back values; what they mean
  is the caller's.
- Multi-party conversations. One person, one exchange.

## Design

**The state is reified because the log demands it, not because a
state machine is nicer.** An intake written over an effect system is a
delimited continuation (specs/delimited-control.md): ask, ask, act,
with an interrupt as an abort to the prompt. That is the natural
form, and a caller should write it. But a closure on the heap cannot
be appended to a log and read back after a deploy, and a conversation
that spans days must survive one. So the suspension is journalled as
DATA — Reynolds' defunctionalisation, arrived at from the durability
end rather than the compiler end. The implementation this spec comes
from hand-wrote that reification as an ADT of pending states, one case
per suspension, and would not have needed to if the suspension itself
were durable.

**`Durable` already holds the hard half.** It journals intent first
and the answer after, and on recovery hands back recorded answers
without touching the world. An `Entry` whose `answer` is `None` is
structurally a question asked and not yet answered. What is missing is
that reading: today every missing answer is the crash window, for
`OnRepeat` to resolve, and there is no state for *asked a person,
waiting, and this is normal.* Adding it is what makes this spec
implementable rather than another state machine.

**Replay must resume from recorded decisions.** A conversation
reconstructed by re-running its own recogniser rebuilds a DIFFERENT
conversation the day the recogniser changes — and a recogniser
changes: rules are edited, a fitted model is refitted. The
implementation this comes from recomputes the route on replay and is
correct only because its rules happen not to have changed since the
entries were written; it edited them four times in one day. What must
be journalled is the verdict, not only the text that produced it.

**An interrupt is rule-only, and that is a measured line.** During an
intake every message is a candidate answer, and the free-text answer
to "what are your skills" is exactly the shape a similarity layer
misreads as a new request. So only an exact, deterministic match to a
DIFFERENT command aborts — anything else is the answer to the question
that was asked.

**Reaching someone who is not present** is the same split as every
other effect here: the decision is journalled, the delivery happens at
the edge, and a restore repeats neither. A door registers itself when
it is running; a person nobody can reach is not told AND not recorded
as told, because the two must not come apart.

## Decisions

**The runtime carries no words, in any language.** It names kinds
(`Ask`, `AskAgain`, `ReadBack`) and the caller renders them. The
alternative — shipping phrasing here — makes this module accumulate
four-language copy for every product that ever uses it, and the first
disagreement about tone becomes a pull request against a library. The
slot's own `ask` is a function of `Lang` for the same reason: the
runtime knows a language must be chosen and knows nothing about what
is said in it.

**Frames are VALUES the caller supplies, not a schema this module
parses.** A JSON frame format would let a caller edit without a
compile, and would cost that caller the check it wants most: adding a
frame should force every place that must learn about it to say so. A
caller that wants data-authored frames can build the values from its
own file; the runtime needs no parser, no schema and no opinion about
someone else's domain.

**A re-ask happens once.** A person who answers "we'll agree on it"
twice means it, and holding an intake hostage to a parser is worse
than storing their words as words. Measured on the source
implementation: the second answer is accepted as text and the
conversation continues.

**The language is pinned to the exchange, not re-derived per
message.** Answers inside an intake are three words long and share
business vocabulary across languages, and a per-message detector
guesses. INCIDENT: an intake conducted entirely in one language
switched to another on its second-to-last question, because the
answer "rate, full-time, Wrocław" carried no letter unique to either
and the trigram fallback picked the larger corpus. In a live
continuation the language would have been a captured variable and
correct for free; the reified state has to carry it explicitly, and
this is what that costs when it does not.

**`read` failing is a question, not a value.** The alternative — store
the string and move on — is how a marketplace ends up with a budget
field containing the word "blue". A slot that declares what its answer
means is also declaring what it cannot accept, and the honest response
to that is to ask again rather than to file it.

## Results — conversation-runtime (2026-09-04)

Built as `Conversation.scala` on `durable-waiting-on-a-person`, with
`Durable` supplying the suspension and nothing here holding state of
its own. Ten tests. Two items stay unticked and are honest about it:
replay-from-recorded-verdicts is the CALLER's discipline (this module
never re-classifies anything, so it cannot violate it, and cannot
enforce it either), and reaching someone who is not present is not
this module's business at all — it was listed in the spec's behavior
because a conversation needs it, and it belongs to whoever owns the
channels.

**The unused parameter that found a hole.** The first cut had the
runtime emit `Say.Ask(frame, slot)` and let the caller render, on the
principle that no words live here. The compiler then reported `lang`
as unused — correctly, because nothing ever applied `Slot.ask`. That
is not a tidying problem: with no rendering at ask time, the LANGUAGE
of an exchange is stored nowhere, and a restarted process holding only
the journal cannot render the outstanding question without
re-deriving a language from whatever was typed last. Which is exactly
the failure the Decisions section already recorded from the source
implementation, arriving a second time by a different route.

So every `Say` now carries the text as it was ACTUALLY asked, rendered
once through the caller's own function, and `Frame` gained
`readBack: (L, Map[String, Json]) => String` for the one sentence a
slot cannot compose. The module still authors no words — it applies
the caller's — and the language is pinned by construction rather than
by remembering to pin it. `pending(journal)` therefore answers with
something renderable on its own.

**What the caller supplies, in full:** a `Frame` of `Slot`s, each with
a name, a question per language, a `read` that says what an answer
means, and optionally an `extract` that reads the opening sentence.
Everything else — which question is next, when to ask again, what a
`Reply` that is not an answer does, where the state lives — is here.

**Values are `Json`.** okay-agent does not depend on okay-match, and
`Json` is already the currency of tool arguments and answers in this
module, so a slot's `read` produces one. A caller with a richer value
type converts at its own edge.

## Results — conversation-over-frame (2026-09-04)

Two slot models appeared in this repository on the same day. This
lane merged them, at the request of the consumer of the older one,
who named three defects and one operational warning. All four are
answered below; none of the four was my finding, and the warning is
the one I would not have thought of.

`okay.frame` is the merged form and `Conversation` keeps what a frame
cannot have: the suspension. It no longer carries a `Slot` or a
`Frame` of its own.

### The three

1. **A typed value, not `Option[Json]`.** The old `Slot.read` parsed
   an answer to check it was acceptable and then stored the TEXT, so
   the caller parsed it a second time — the same defect
   `intent-frame-typed-values` had just closed in the other module.
   `Outcome.Filled` now carries the FRAME, and `valueOf(price)` is a
   `Double`.

2. **An answer may answer more than was asked.** `Conversation`
   answered only the pending question, so "Wrocław, and remote works"
   yielded the city and then a question about the terms the person had
   just given. `Frame.take` answers the named slot and offers the same
   sentence to every other slot's extractor, and the loop recomputes
   `missing` each round — so what an answer filled in passing is not
   asked for.

3. **How many questions are left.** `Say.Ask` carries `remaining`.
   It is an `Option[Int]` because a journal outlives a deploy: an
   entry parked by the previous build decodes as `None`, which is "not
   written down" rather than "none left", and a caller renders no
   count instead of a wrong one. There is a test for that entry shape.

### The warning, which changed the design

The consumer's own words: the language must be an argument of the
whole conversation, not a parameter of every call — an intake that
re-decides its language from a three-word answer switches it in the
middle of a profile, and they measured one, on the second-to-last
question.

Both halves took a language per call (`question(lang)`, `answer(name,
lang, text)`). The frame now CARRIES it: `in(lang)` once, where the
exchange begins, and no method takes another. `intake` has no `lang`
parameter at all. The property is tested rather than documented:
answering and extracting leave `lang` alone, and there is no signature
that could be handed a different one.

`untranslated` came out of the same thought: a slot with no question
in the frame's language falls back to English SILENTLY, which for a
four-language intake is a defect that reaches a person. It is a list a
test can assert empty.

### What the merge cost

`Slot.ask` is a `Map[String, String]`, so the old `L => String` with
its opaque, caller-defined language type is gone. Two reasons, and the
second is load-bearing: a map is DATA, so a service adds a language
without a compiler; and a language that must survive a RESTART has to
be something that can be written down — an opaque `L` cannot go in a
journal, which is exactly why the old runtime had to store every
rendered question as text. A caller with an enum passes its code.

`Frame.opening` is gone too: a slot that should swallow the opening
sentence says so with its own extractor, rather than the frame naming
one slot as special.

`Outcome.Filled` no longer means every slot parsed. It means the
exchange ended with a yes; `frame.complete` says whether everything
was read, and `said(name)` gives the words for a slot that was asked
twice and still could not be read. The old runtime stored those words
AS the value, which is how a field typed as a number held a sentence.

### Where it lives, and why a module

`okay-intent` and `okay-agent` may not depend on each other:
okay-intent's test scope reaches for okay-agent's journal to replay
recorded model answers, and sbt rejects the cycle (verified, not
assumed — `recursive lazy value okayAgent needs type`). So the shared
half is its own module, with no dependencies at all, which is also the
honest description of it: a frame is data, and the things that fill it
— a date parser, a journal, a model — are not.

## Results — frame-said-is-content (2026-09-04)

The consumer reviewed `conversation-over-frame` BY MIGRATING, which is
the only review worth having, and reported it survives contact: four
languages, a live intake, 226 tests green. All three of their asks are
in use rather than merely compiling — `price` is a `Slot[Money]` and a
filled frame hands back a `Money`; their `Kind.Money` tag and the
`retried` flag beside it are gone; `Frame.take` was their unwritten
eighth request; and `remaining` renders as "І останнє: який бюджет на
місяць?", because a person four questions into a form has no idea
whether they are near the end.

They also accepted the two things the merge cost. The Map-keyed
language is fine — `Lang.code` at the boundary, one line — and
`untranslated` is asserted empty for five frames across four
languages, so a missing wording is a failing test rather than a silent
English fallback.

**The one finding, and this lane is it.** `Filled` no longer meaning
"every slot parsed" cost them one line, and only because `said`
exists — a test went red where "договорімось", answered twice, used to
be stored AS the price and now sits in `unread`, so the read-back lost
the person's own words. Their semantics were wrong and mine are right
on that, and the case is still worth naming: in a marketplace those
words are CONTENT. "negotiable", "по договорённости" are perfectly good
things for a listing to say and are exactly the answers no parser will
ever read. A caller reading only `filled` drops them silently.

So `said` is documented as what it is — the other half of the answer,
not an escape hatch — and `words` is the door they had to write by
hand: everything the person said, parsed or not, in one map. `filled`
stays the parsed half and `valueOf` stays the typed one.

**Filed, not fixed:** `frame-language-with-grammatical-gender`. They
raised it and could not test it — a language whose question differs by
the grammatical gender of the ADDRESSEE needs more than a language
code, or needs the caller to key by `"pl-formal-f"` and own that
choice. Their Polish addresses informally and dodges it.

## Results — frame-choice-and-provenance (2026-09-05)

The consumer asked a blocking question in the room: does request 5's
"description" carry value wordings and a non-constant default? The
honest answer was NO — request 5 was a name, a question per language
and a parser, and that is exactly what shipped. Their case was good
enough that the library should hold it anyway, so the split is: the
library owes the TYPE and the PROVENANCE, the caller keeps the
inference rule.

**`Slot.choice`.** A closed set of values, each carrying its wordings
per language. Those wordings are needed three times — to offer, to
read, to echo — which is why they belong beside the value rather than
in the caller's rendering layer, and it is still the same "no words
live here" boundary: the caller writes every one of them.

Reading accepts a wording INSIDE the answer. A person asked "on site
or remote?" writes "можно и удалённо, если так", and a parser that
demands the bare token would re-ask someone who has already answered.
Longest wording wins so one value cannot swallow another, and every
language is matched rather than only the exchange's — someone in a
Russian conversation still types "remote" sometimes.

**`Source`, and this is the part their requirement demanded without
naming it.** They wrote: overridable by what they do say, and VISIBLE
so they can correct it. A default fires when NOTHING was said, so its
value has no evidence behind it and would otherwise be
indistinguishable from one a person typed. `Answered` now carries
`Said` / `Found` / `Assumed`; `assume` fills a slot, `assumed` lists
them, `filled` shows them back in the reader's language, and `words`
leaves them out — because "what you told me" must not contain what
nobody told it. A person's answer beats an assumption in either
order, tested both ways.

**What was deliberately left out.** The rule that reads "можно и
удалённо" out of a CITY answer, and the choice of what to assume when
a question goes unanswered. Both are domain knowledge. The library
gives them a place to be recorded honestly and no opinion about what
they should be.

