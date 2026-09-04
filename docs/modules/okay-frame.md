# okay-frame

What a FORM is: named slots, typed answers, and what is still missing.

It exists because two slot models appeared in this repository on the
same day. [`okay-intent`](okay-intent.md) grew a frame — typed values,
an answer addressed by name, a list of what is still unanswered —
while [`okay-agent`](okay-agent.md)'s `Conversation` grew a slot model
of its own with the half the frame lacked: the SUSPENSION, a
straight-line intake parked in a journal across a restart. The
consumer of the older one asked for the merge rather than a rival, and
since neither module may depend on the other, the shared half is here
and depends on nothing at all.

| | |
|---|---|
| `Slot[A]` | a name, a question per language, a parser, and an optional extractor |
| `Found[A]` | a value together with the span of the message it came from |
| `Answered` | one filled slot: the slot, the words, and the parsed value |
| `Frame[I]` | the slots an intent needs, the answers so far, and the language of the exchange |

Three properties are the reason it is a type rather than a `Map`.

**A filled frame hands back the VALUE.** `valueOf` takes the SLOT, not
a name — the slot is the evidence that this answer has type `A`, so
there is no way to ask for a type the slot never had. A frame that
keeps only text hands a caller back the string it has just proved was
a date, and the caller parses it again, with whatever reference day it
happens to hold. Both halves of the merge had that defect.

Identity, not equality, decides whose answer it is: hold your slots as
values. A `def` handing back a fresh equal slot per call is a
different slot, and the answer will not be found.

**The language is a field.** Not a parameter of `question`, `answer`
or `missing` — those took one before the merge, which makes a
mid-exchange flip possible by accident. An intake that re-derives the
language from each incoming message switches it on a three-word reply,
and the consumer measured exactly that, on the second-to-last question
of a profile. `in(lang)` is called once, where the exchange begins.
`untranslated` names the slots that cannot be asked in it, so a
four-language intake that would quietly fall back to English is
visible before it ships.

**An answer may answer more than was asked.** `take(name, text)`
answers the named slot and offers the same sentence to every other
slot's extractor: asked where and told "Wrocław, and remote works", a
frame keeps the city and the terms, and the next question is not one
the person has already answered. Text a slot cannot read is KEPT, as
words beside the slot (`said`), and the slot stays unanswered — rather
than being stored AS the value, which is how a field typed as a number
ends up holding a sentence.

`said` is not an escape hatch; it is the other half of the answer, and
the first consumer to migrate onto this frame said so from a live
domain. A price slot parses money, and "negotiable", "по
договорённости", "договорімось" are things a listing legitimately
says that no parser will ever read. Read `words` for everything the
person said, `filled` for the answers that parsed, and `valueOf` when
only a typed value will do.

What it deliberately is NOT: a conversation. A frame describes and
holds. It does not know what has been asked, does not decide when to
ask, and cannot suspend — that is `okay.agent.Conversation`, over
this.
