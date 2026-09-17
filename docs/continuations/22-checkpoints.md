# 22 · Saving and restoring: checkpoints, and what cannot be one

> **Part VI is production.** Compiled in
> `okay-persist/.../TestBookCheckpoints.scala`, beside the suites that
> already measure this: `TestDialogue` and `TestDelimPersist`. This
> chapter answers the first question everybody asks, and the answer is
> not the one they expect.

---

## The question, and the answer

> *Can I snapshot a paused program and restore it later?*

**No. And you do not need to.**

The continuation is a closure. It holds references to the stack frames,
the captured variables, the interpreter's own machinery, and — in
this library — a program value built out of lambdas. There is no
honest serialisation of that. Any library claiming to serialise a
running program is doing one of two things: restricting what you may
write inside it, severely, or lying about what a restart gives you.

What can be saved is **everything the program was told**. From that,
its position is re-derived by running it again. That is chapter 6's
idea — the fold is the program — and this chapter is what it costs and
what it buys.

## Four mechanisms, routinely confused

They cut different costs. Getting them mixed up is the source of most
confusion about durable execution:

| | what is stored | what it cuts | durable |
|---|---|---|---|
| the journal | every answer | nothing — replay from the start | yes |
| chapters (`Snapshots`) | a journal **prefix** and its offset | **reading** | yes |
| `continueAs` | one **seed** that supersedes the history | **running** | yes |
| a resume cache | the live paused program in memory | replay between calls | **no** |

## The distinction that matters: reading is not running

This is the claim the chapter exists for, and it is the one most often
missed:

> **A snapshot cuts reading, not running.**

A chapter stores a *journal prefix*, not a program state. On a cold
start the reader takes the prefix from one compacted record instead of
scanning the whole topic — and then **runs the program over every
answer anyway**, because running it is the only way to arrive at the
place it stands.

`TestDialogue` measured the reading half: a snapshotted cold start
reads at least three times fewer records than a plain one, with the
snapshot topic's own scan **on the bill** — the comparison would not be
honest otherwise.

The running half was pinned nowhere, so this book's suite counts it. A
counter inside the program body, incremented once per pause, over two
cold starts of the same 40-answer journal:

```
cold start over 40 answers:
  plain         read 80 records / ran 40 steps
  snapshotted   read  8 records / ran 40 steps
```

**Ten times fewer reads. Exactly the same amount of program.** The
assertion is written so that an improvement fails it, because an
improvement would mean this chapter is wrong:

```scala
assertEquals(snapSteps, plainSteps,
  "the snapshot changed how much the PROGRAM ran")
```

That test also carries the guard that earned its place immediately:

```scala
assert(plainSteps > 0, "the program never ran at all: the counter proves nothing")
```

The first cut of it measured the `Dialogue` **constructor**, which does
not fold the journal — asking a dialogue where it *stands* is what
replays the program. Both step counts were zero, `0 == 0` passed, and
the chapter's central claim was "proved" by a test in which nothing
happened. An equality between two measurements is worth exactly as
much as the proof that either was taken.

Only `continueAs` shortens the execution, and it does so by declaring
the old history **superseded** — a seed that says "start from this
value, the answers before it no longer describe where we are". That is
a different operation from a snapshot, with a different risk, and
chapter 23 uses it where a run would otherwise grow without bound.

## Warm and cold are different problems

```
WARM — you are holding the program: step(p, a) advances it by one and
       journals. A drive that answers n questions costs O(n).

COLD — you have only the log: answer(a) replays, so it is
       O(answers so far).
```

`TestDialogue` measures exactly this, and the numbers are exact
because the store is deterministic:

- the warm path re-read the journal **0 times**;
- the cold loop read **N·(N−1)/2** records — the O(n²) shape, arrived
  at one standing start per answer.

A resume cache is the answer to the warm case: hold the paused program
between calls, and five touches of a waiting run replay **once**
instead of five times. It is per-process and it is **not durable** —
it is an optimisation over a journal that remains the truth. If the
process dies, nothing is lost except the shortcut.

## The log is the truth; a chapter is only a shortcut

The property that makes the whole design safe to operate:

> A reader with **no** snapshot store sees exactly the same journal,
> stands in exactly the same place, and answers exactly the same.

A missing, corrupt, or stale chapter costs **time, not correctness**.
Both suites assert this, and it is the difference between a cache and a
second source of truth. Which brings us to the trap.

## The checkpoint that is a trap

**Storing derived state beside the answers.**

It is tempting and it always looks like an optimisation: along with the
journal, write the totals, the current status, the computed summary —
so a reader can skip work. Now two sources of truth exist. They agree
on the day you write them and they drift on the day somebody changes
how the total is computed, fixes a rounding bug, or adds a field. From
then on, the stored state describes a program that no longer exists,
and the drift is silent, because both sides are internally consistent.

The rule that avoids it:

> **A checkpoint may only contain things the program was told, never
> things the program worked out.**

A journal prefix satisfies this. A `continueAs` seed satisfies it in a
different and explicit way: the seed *becomes* what the program was
told, and the history it supersedes is declared no longer descriptive.
A cached total satisfies nothing.

## What makes a restore safe

Three conditions, and each of them has a failure behind it:

**1. The answers must be the program's only source of
non-determinism.** This is chapter 21's `Replayable`, and it is a type,
not advice. A clock read between two pauses reads a different time on
replay; a `Writer` between two pauses says the same thing again. Both
were measured, and that measurement is why the constraint exists.

**2. A checkpoint carries the program's name.** `"booking/1"` is
stored with the journal. A reader whose program does not match **stops
the fold and says so** rather than mapping old answers onto a new
program's questions. This is the defect that a deploy produces and
nothing else does: same journal, different code, answers landing in
the wrong slots, and every individual value looking plausible.

**3. A refused answer must leave the journal alone.** The fold happens
**before** the append, which is what lets a rejected answer change
nothing — and it is why the cold loop reads `i` records at step `i`
rather than `i+1`. An implementation that appends first has a journal
containing answers the program never accepted.

## What cannot be a checkpoint

For completeness, since the question always comes back:

- **A thread's stack.** Not portable, not versionable, and invalidated
  by the next deploy.
- **A serialised closure.** The JVM can do it; the moment the class
  changes it deserialises into a program that no longer exists.
- **A memory image.** Restores the bug as faithfully as the state.
- **A resume cache entry.** By design: it is a live object in one
  process and it makes no claim to outlive it.

Every one of those tries to save *where the program is*. The working
answer saves *what the program was told* and recomputes where it is —
which is slower, and survives a deploy.

## Could an applicative be checkpointed instead?

The sharpest question anybody asks about this chapter, and the answer
is a genuine design fork rather than a no.

The reason a monadic program cannot be written down is one line of
`Free`:

```scala
case Bind[F[+_], A, B](a: Free[F, A],
                       f: A => Free[F, B]) extends Free[F, B]
```

`f` is a host closure. What the program does after its first operation
cannot be read without running it. That opacity is the price of
`flatMap`, and most programs are worth it.

An applicative has no such function. Its structure is fixed before any
value exists, which is why `Static` — the free applicative with
`select` on top — can offer something `Free` cannot:

```scala
def leaves: Vector[F[Any]]    // every operation the program MAY perform,
                              // before it runs
```

Those leaves are operations **as values**. They serialise.

### So: run it as a monad, save it as an applicative?

No, and the reason is one sentence in `Static.scala`:

> The bridge is one-way and cheap: `toFree`.

You can turn a static program into a monadic one in order to run it.
You cannot recover a static program from a monadic one, because
recovering it would mean reading the closure. The moment a `flatMap` is
written, the shape is gone — not hidden, **gone**, in the sense that no
amount of inspection recovers it.

The direction that works is the other one: **start static**.

### What starting static would buy

Everything in this chapter changes if the structure is known in
advance:

> If a program's shape is static, "where it stands" is an **index**,
> not a closure.

A checkpoint becomes a cursor plus the answers, and a restore is O(1) —
**no replay at all**. That is not a hypothetical design; it is exactly
why BPMN engines, step functions and workflow DSLs look the way they
do. A static graph with a cursor is checkpointable for free, and the
whole family of tools arrived at that shape because it is the only one
that does not have to re-run history.

### What it costs, which is the reason this engine did not

No data-dependent structure. `Select` buys a conditional with **both
sides written down**, and the source names the approximation honestly:

> `leaves` reports both sides of every `Select`, because which side
> runs is decided by a value that does not exist yet. It is an upper
> bound.

An upper bound is what a batcher, a capability list and a dry run all
want. It is not what a workflow wants. You cannot write

```scala
val nights = !w.pause("how many nights?")
for _ <- 1 to nights do !w.pause("which room?")
```

because the *shape* now depends on an answer. The five-line booking of
chapter 23 ends on `if !w.patch("promo")`, and the reason its programs
are worth writing at all is that the line between two pauses is
ordinary Scala.

So the trade is real and it is a trade:

| | position | restore | language |
|---|---|---|---|
| monadic | a closure — cannot be named | replay, O(answers) | anything |
| applicative / selective | an index into a known shape | a cursor, O(1) | no data-dependent structure |

This engine took the left column deliberately, because the code between
pauses is straight-line and replaying it is cheap — measured above: 40
answers, 40 steps.

### The caveat that survives either choice

An applicative does not make the *program* serialisable either.
`Pure(a)` may hold any value, functions included, and the spine of
combining lambdas is host code whichever functor you chose. What
serialises is the **leaves**, not the spine.

The code still has to be on disk, and it is still addressed by name —
which is what `"booking/1"` in the journal has been doing all along.
That is the deep version of this chapter's first answer:

> Nobody serialises the code. Not for monads, not for applicatives.
> The difference is whether you can name your **position** in it
> without running it.

### And the hybrid that is already here

`continueAs` is the third answer, and it sits between the two columns:
it does not make the past inspectable, it declares it **superseded**. A
seed replaces the history, so the next replay starts from a value
rather than from an epoch. Not a cursor, not a full replay — a way to
stop paying for a history that no longer describes anything.

---

← [21 · The disciplines that make it safe](21-the-disciplines.md) ·
[Contents](index.md) ·
[23 · Durable workflows →](23-durable-workflows.md)
