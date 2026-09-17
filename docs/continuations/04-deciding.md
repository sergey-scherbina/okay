# 4 · Deciding: reach for it, or don't

> The last chapter of the case, and the one that argues the other way.
> A book that cannot describe the misuse of its own subject is selling
> something. **The honest default is: don't.**

---

## Start from no

Most code does not need this. Most systems contain one or two places
where it would help and several hundred where it would be a liability.
The technique earns its keep in a minority of a program, and a team
that reaches for it everywhere will produce something worse than the
threading and state machines of chapter 2 — because at least those are
familiar to the next person.

So the question is not "is this powerful" (it is) but **"is this one
of the places"**. What follows is how to tell.

---

## The two questions that settle most cases

**Question one: is the control flow the problem, or is the effect?**

These are different. If what you need is *to do something* — write a
log line, read a config value, get the time, talk to a database — that
is an **effect**, and an effect is what you want. Effects compose,
their handlers are ordinary, and everybody on the team already
understands them.

If what you need is *to change where the program goes next* — stop
here and go there, pause and be resumed by somebody else, run this
part twice — that is control flow, and that is what this book is for.

The mistake that costs the most is using a capture to obtain a value
that an effect would have handed you.

**Question two: how many layers are between the decision and the
place that must react to it?**

- **Zero or one.** Use the ordinary thing. An `Either`, an early
  `return`, an exception at the boundary. Do not install machinery to
  jump over a single stack frame.
- **Two or three, all of them yours.** Judgement call. If the layers
  naturally speak about the failure, threading reads fine. If they
  have to be taught a vocabulary that is not theirs, that is the
  smell.
- **Many, or some of them not yours** — a callback from a library, a
  visitor you did not write, a recursive descent with four node types.
  This is the case the mechanism exists for.

If both answers point the same way, stop; you have your decision.

---

## Five smells that mean "reach for it"

Each of these is a *symptom* of a missing continuation. One of them is
weak evidence. Three of them in the same file is not.

1. **A type appears in signatures that have no business with it.**
   `Option[Rejected]` on functions that are about invoice structure;
   an error enum threaded through a parser. The type is travelling,
   and travel is the thing a capture removes.

2. **`toList` exists to escape somebody's callback.** A buffer whose
   only purpose is to turn "you will be called" into "I will ask".
   Especially with a comment apologising for memory.

3. **A thread exists and nothing is concurrent.** A producer thread, a
   queue, a poison pill — and the two ends never actually run at the
   same time by design. A thread is being used as a control-flow
   device.

4. **A state enum whose cases are positions in a procedure.**
   `AwaitingApproval`, `AwaitingPayment`, `AwaitingShipment` are not
   states of a domain object; they are *line numbers* of a program
   that was turned inside out. (Contrast: `Draft`, `Published`,
   `Archived` are real states — a reader can tell you what is true of
   a document in each, with no reference to any procedure.)

5. **The same cleanup or instrumentation is written on two exit
   paths.** And it will be written on a third one next quarter,
   somewhere else.

---

## Five signs to walk away

1. **It is one call site.** A mechanism repays learning when several
   places share it. For one, write the awkward version and leave a
   comment; you will know within a quarter whether a second appeared.

2. **The team has no appetite for a new concept right now.** This is a
   legitimate engineering constraint, not cowardice. A correct design
   nobody on call understands is worse than a clumsy one everybody
   does. Chapter 2's replacements are all maintainable by somebody who
   has never heard of any of this.

3. **The hot path.** A boundary roughly doubles the cost of the work
   inside it (chapter 20 has the measurements). Around a program
   waiting on a human that is invisible. Around a per-element loop in
   a stream processing millions of records it is disqualifying — and
   the answer there is to put the boundary around the *smallest* span
   that needs it, not the whole loop.

4. **A combinator already fits.** `traverse`, `collectFirst`,
   `foldLeft` with an `Either`, a `LazyList`. If one of these
   expresses the whole walk, it is a better answer: more people read
   it, and it cannot be misused.

5. **What you actually want is concurrency.** If the producer should
   genuinely run ahead of the consumer, you want a thread or a fibre
   and a queue. Continuations give you *interleaving without
   concurrency*, which is a different thing and not a substitute.

---

## The comparison nobody makes, and should

When a capture and an effect could both solve the problem, prefer the
effect — and the reason is not taste.

An effect is **named, typed, and handled elsewhere**. Three people can
read the signature and agree on what it does. A capture is **a
structural act**: it changes where the program goes, and its meaning
depends on where the boundary sits, which is somewhere else in the
file.

So the rule of thumb:

> **If it can be an effect, it should be. Reach for a capture when the
> thing you need to talk about is the rest of the program itself.**

Part IV, chapter 14 shows the other direction: how a capture that has
proved itself becomes a *new effect*, so that call sites stop seeing
the capture at all. That is the mature shape — and it means the answer
to "effect or capture" is often "capture once, in a library, so that
everyone else gets an effect".

---

## The checklist

Before writing one, answer these. They take five minutes and they are
the whole of this chapter.

- [ ] **Is this control flow, not an effect?** Am I trying to change
      where the program goes, rather than to obtain a value or perform
      an action?
- [ ] **Are there layers in between that would otherwise have to
      learn a vocabulary that is not theirs?**
- [ ] **Is there more than one call site**, or a clear second one
      coming?
- [ ] **Is this off the hot path**, or can the boundary be placed
      around only the part that needs it?
- [ ] **Have I checked that no ordinary combinator expresses this?**
- [ ] **Do I know what it does to state, resources and `finally`
      here?** (Chapter 18. Not optional — this is the item people
      skip, and the one that produces the bug reports.)
- [ ] **If this program will be resumed or replayed: is everything
      non-deterministic in it — clocks, ids, randomness, I/O — going
      in through a door I control?** (Chapter 21. Skipping this one is
      the most expensive mistake in the book.)

Six yeses and a clear-eyed answer to the last two: go ahead, and Part
II has the shape you need. Fewer: chapter 2's replacements are
honourable, and you can revisit when a second call site turns up.

---

## What Part I claimed

Looking back before going on:

1. **Four ordinary requirements have no straight-line form** in most
   languages (chapter 1).
2. **Each standard replacement leaves a specific bug open** — not
   "is inelegant" (chapter 2).
3. **All four are missing the same noun**, so one mechanism replaces
   four workarounds (chapter 3).
4. **And most code should still not use it** (this chapter).

If you are still reading, Part II is the recipes: four shapes, each
with the ordinary version beside it, each compiled by a test so the
page cannot drift from the library.

---

← [3 · A continuation, in one page](03-a-continuation-in-one-page.md) ·
[Contents](index.md) ·
[5 · Leave early with an answer →](05-leave-early.md)
