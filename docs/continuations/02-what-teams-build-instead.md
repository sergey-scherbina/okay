# 2 · What teams build instead, and what it costs

> Chapter 1 named four programs the language will not let you write
> straight. None of them goes unwritten, of course — they are
> requirements, and requirements get met. This chapter is about what
> gets built instead, what it costs to keep, and the one bug each
> replacement cannot rule out.
>
> **This is the chapter for whoever decides what the team spends its
> time on.** It contains no library API and almost no Scala.

---

## The frame: these are not bad designs

It would be easy, and dishonest, to line up five straw men. Every
replacement below is what a competent team builds, and most of the
time it is the right call. The state machine in particular is a
standard, teachable, honest design; people who write it are not
ignorant of anything.

So the argument is not "this is ugly". The argument has a specific
shape, and it is worth stating once before the list:

> Each replacement **encodes a control-flow decision as data**, and
> nothing then checks that the data and the intention still agree.

That is the cost. It is small at first and it compounds, and it
compounds fastest in exactly the systems that are worth money — the
long-lived ones, with several authors and a history.

---

## Replacement one · Thread a sentinel

**For:** leaving a computation early with an answer.

Every function on the path between the decision and the caller gets a
new return type — `Option`, `Either`, a status enum — and a new
responsibility: look at what the thing below returned, decide whether
to keep going.

**What it costs to keep.** The information spreads. Functions whose
subject is invoice structure now mention rejection; functions whose
subject is parsing now mention a validation outcome. Add a second way
to fail and every one of them is edited again. A reviewer cannot tell,
from a diff that touches five files, whether the propagation is
complete.

**The bug it cannot rule out: the layer that forgets.** One function
in the chain gets the failure and does not propagate it — it logs it,
or maps it to a default, or pattern-matches only the success case
because at the time there was only one. Nothing detects this. The
types are satisfied: every function returns the right shape. The
program simply continues past a decision that should have ended it.

This is not hypothetical, it is the characteristic failure of the
style, and it survives code review because each individual function
looks reasonable in isolation.

**When to prefer it anyway.** When there are one or two layers, when
the failure is genuinely part of the domain vocabulary of every layer
it passes, or when a combinator (`traverse`, `collectFirst`) already
expresses the whole walk. If `Either` in a `for`-comprehension reads
well, stop here — you do not have this problem.

---

## Replacement two · Buffer it all, or add a thread

**For:** consuming a producer that insists on pushing.

Either collect every item before looking at any (`toList`), or run the
producer on its own thread and pass items across a queue.

**What it costs to keep.** The first is memory and latency: you have
turned "the first ten tokens" into "all the tokens". Usually fine;
occasionally an outage, and the day it becomes an outage is the day
the input got big, which is the day everything else is also on fire.

The second is worse in a way that is easy to miss at review time: it
converts a question about *data* into a question about *concurrency*.
Now the code has a bounded queue, a termination signal, a shutdown
path, a policy for what happens when the consumer stops early, and a
rule for moving an exception from the producer thread to the
consumer's. Each of those is a decision somebody has to get right, and
the tests for them are timing-dependent.

**The bug it cannot rule out: the path that never terminates.** A
consumer stops early; the producer blocks forever on a full queue; the
thread leaks. Or the producer fails, and the consumer waits for an
item that will never arrive. Every shop that has done this has a
story, and the stories are all about a system that was fine for a
year.

**When to prefer it anyway.** When the producer really is concurrent —
when it is reading a socket, and you want it running ahead of you.
Then a thread and a queue are not an accident of the API, they are the
design. Chapter 6's technique is for the case where nothing is
concurrent and a thread is being used as a control-flow device.

---

## Replacement three · Write the state machine by hand

**For:** a program that waits for something slow — a person, another
system, a date — and must survive a restart.

The program is turned inside out. Its control flow becomes a set of
named states and a transition function; its position becomes a row in
a table.

**What it costs to keep.** Four things, and the fourth is the
expensive one.

1. **The business rule stops being in one place.** "If approved, ship;
   otherwise refund" was one line. It is now two transitions in a
   `match`, two rows in a table, and a diagram somebody drew in a wiki
   that is already out of date.
2. **The compiler stops helping.** Nothing checks that every reachable
   pair of (state, event) is handled. The pairs you forgot are found
   by a run that stops moving.
3. **The interesting line disappears.** "Wait for a director" was the
   requirement. In the rewritten program there is no line that says
   it; there is machinery about persistence, and the waiting is
   implied by the absence of a transition.
4. **Two artefacts must agree, by hand, for ever.** The code and the
   stored states. Deploy a version that renames or removes a state and
   every run sitting in it is stranded. Nothing fails at build time.
   The failure is a run that quietly never finishes, discovered by a
   customer.

**The bug it cannot rule out: the transition nobody wrote.** A state
and an event that can co-occur, and no case for the pair. In a small
machine you can enumerate them; at nine states and seven events there
are sixty-three pairs, most impossible, and the ones you decided were
impossible are exactly where this bug lives.

**When to prefer it anyway.** When the machine is genuinely small and
genuinely a machine — a protocol handshake, a payment lifecycle with
regulatory meaning where the *states themselves* are the thing being
specified and audited. If a compliance document lists the states, then
the states are the requirement and you should model them.

---

## Replacement four · Write the exit twice

**For:** doing something on the way back out, that also sees the value
coming back.

`finally` runs on both paths but cannot see the result. So the code
that records the outcome is written once in the success path and once
in the failure path.

**What it costs to keep.** Almost nothing, at first. That is why this
one survives.

**The bug it cannot rule out: the third exit.** Somebody adds an early
return, a new catch, a short-circuit — and updates one of the two
copies. Now some requests are timed and some are not, or one branch
records a status the other does not. Nobody notices, because the
instrumentation is not what the tests are about.

**When to prefer it anyway.** When there are exactly two exits and the
function is short enough that both are visible on one screen. Which
is to say: today.

---

## Replacement five · Buy a workflow engine

**For:** the same problem as replacement three, solved by adopting a
system that specialises in it.

This is a real and often correct answer, and the good ones are good.
It deserves a fair accounting rather than a dismissal.

**What you get.** Durable execution as a product: retries, timers,
visibility, a UI, an operations story somebody else maintains, and a
community that has met your problem before.

**What it costs to keep.**

- **A new operational surface.** A service (or a hosted dependency) in
  the path of your business-critical work, with its own availability,
  its own upgrades, its own failure modes to learn.
- **Your code adopts its shape.** Workflow code becomes a special kind
  of code, with rules about what may be called from where, enforced —
  when it is enforced at all — at runtime.
- **A second source of truth about the same process**, in a store you
  do not own.
- **Determinism rules you must remember.** Most such engines replay
  your program, which means your program must not read a clock, a
  random number, or a service directly. In most of them this is a
  documented rule checked at run time, if at all — the compiler does
  not know about it.

**The bug it cannot rule out: the rule the compiler never saw.** A
`new Date()` or an HTTP call added to a workflow function by somebody
who did not know the rule. It works in test, where nothing replays.
It corrupts in production, where things do.

**When to prefer it anyway.** When you need the operations more than
the model: a team that will not build dashboards, a compliance need
for a vendor's audit trail, or a scale where somebody else's
scheduler is worth the money. Chapter 21 is candid that the engine
in this repository has the model and not the operations.

---

## The accounting, for whoever decides

Three questions, and they are not about elegance.

**1. How many of the four shapes does this system already contain?**
Count them honestly — a hand-rolled state machine, a `toList` that
exists to escape a callback, a sentinel threaded through five layers,
a metric recorded twice. Each is a place where a general mechanism
would replace a special one. If the count is zero or one, the answer
is probably "don't", and chapter 4 will say so.

**2. What is the cost of the bug each replacement leaves open?**
For a reporting job, the transition nobody wrote is an annoyance. For
an order-fulfilment path it is a customer whose money moved and whose
goods did not. The technique's value scales with the blast radius of
its characteristic failure, not with the line count.

**3. What does the team stop building?** This is the real number.
A durable workflow built on the mechanism in this book is the business
program plus a journal; built as a state machine it is the business
program, the state enum, the transition function, the persistence, the
migration story, and a test suite for all of it. The difference is not
style, it is a body of code that exists only to work around a missing
noun.

## The honest other side

Adopting this is not free, and a chapter that pretended otherwise
would deserve the scepticism it got.

- **It is a concept the team must learn.** Not large — chapter 3 is
  one page — but real, and unfamiliar concepts slow reviews for a
  while.
- **It changes what some constructs mean.** `try`/`finally` around a
  capture, resource cleanup, shared mutable state: each behaves in a
  way you must know. Chapter 18 is that list, and it is a list, not a
  paragraph.
- **It costs measurable time.** A boundary roughly doubles the cost of
  the work inside it. For a program waiting on a human that is
  invisible; for a hot inner loop it is disqualifying. Chapter 19 has
  the numbers.
- **It is not a free lunch for durability.** A paused program cannot
  be written to disk — it is a closure. What makes it durable is
  journalling the *answers* and re-deriving the position, and that
  discipline has to be kept. Chapter 21 shows how it is kept by types
  rather than by memory.

## What the rest of Part I does

Chapter 3 gives the missing noun a definition, in one page, with one
picture. Chapter 4 is the one that says *don't* — with a checklist,
because a technique that cannot describe its own misuse is being sold
rather than explained.

---

← [1 · Four programs](01-four-programs.md) ·
[Contents](index.md) ·
[3 · A continuation, in one page →](03-a-continuation-in-one-page.md)
