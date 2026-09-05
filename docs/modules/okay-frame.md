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
| `Slot.choice` | a closed set of values, each with its wordings per language |
| `Source` | where an answer came from: said, found in a sentence, or assumed |
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

## A closed choice, and where an answer came from

Added for a consumer whose slot was neither a string, a date nor a
number: whether a job can be done remotely is a fact that decides
MATCHING rather than wording, and it is the same question on both
sides of their market.

```scala
val mode = Slot.choice[Where]("mode",
  Map("en" -> "On site or remote?", "ru" -> "На месте или удалённо?"),
  Seq(Where.Onsite -> Map("en" -> "on site", "ru" -> "на месте"),
      Where.Remote -> Map("en" -> "remote",  "ru" -> "удалённо")))
```

The wordings per VALUE are needed three times over — to offer the
choices (`options(lang)`), to read what a person typed in their own
language, and to say the choice back in it (`show(v, lang)`). Reading
accepts a wording inside a real answer, because someone asked "on site
or remote?" replies "можно и удалённо, если так" and means it; the
longest wording wins, and every language's wordings are matched, not
only the exchange's.

**And an answer knows where it came from.** `Source` is `Said`,
`Found` (taken out of a sentence written for another purpose) or
`Assumed` — a default that fired because nobody answered, which for
some questions is most people.

```scala
frame.assume(mode, Where.Onsite)   // fills it; complete() is now true
frame.assumed                      // Vector("mode") — show these for correction
frame.words                        // what the PERSON said; the assumption is not here
```

That distinction is the point. A default that looks like an answer is
a lie a form tells quietly; `filled` shows the assumption back in the
reader's own language so it can be corrected, `words` leaves it out,
and a person's own answer always wins in either order.

What the library does NOT decide: what to assume, or how to read one
slot's value out of another's answer. That is domain knowledge and it
stays with the caller.

