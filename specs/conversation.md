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

- [ ] a suspension survives a restart: the pending question is in the
      log, and a fresh process resumes it without a stack
- [ ] a resumed value is a `Reply`, not a `String` — the caller's
      deterministic layer decides which case it is
- [ ] only an `Interrupt` aborts an intake; a fuzzy or a similarity
      guess never does
- [ ] a value `read` rejects is asked again ONCE and then accepted as
      text
- [ ] the language is resolved once for the exchange and pinned to it
- [ ] with `confirm`, nothing reaches the caller as `Filled` until a
      `Yes` follows a `ReadBack`
- [ ] an opening sentence answers `opening` and every slot whose
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

## Results

None yet — this is the spec ahead of the implementation. The behavior
list above is the checklist to tick off as tests cover it, and the
Decisions carry the incidents that produced them, from a working
implementation of the same shape.
