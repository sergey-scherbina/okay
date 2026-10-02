# 21 · The disciplines that make it safe

> **Part V is the limits.** Compiled in
> `src/test/scala/TestReplayable.scala`,
> `src/test/scala/TestDelimDiagnostics.scala` and
> `src/test/scala/TestDelimLimits.scala`, plus
> `src/test/scala/TestBookDisciplines.scala` for the property the
> three share. Chapter 19 listed what a capture does to everything
> else. This one is about the three rules that stop the bad cases
> being writable — and each exists because something went wrong
> without it.

---

## Three rules, and why they are types

Prose in a README is a rule nobody applies at three in the morning. A
constraint in a signature is applied every time, by a machine, for
free. These three earn their keep that way:

| the rule | says | the failure it prevents |
|---|---|---|
| `Replayable[F]` | a program you intend to replay may only hold effects whose re-execution nobody can observe | a restart that does not land where the first run stood |
| `At` | where in the program's own structure this was written | a paused program with no useful stack trace |
| `Shift.Machine[F]` | whether a machine already runs in the row | a nested delimiter on a second machine, which fails at run time with `NoPrompt` |

## `Replayable`: the sentence, as a constraint

A paused program outlives its process because the **answers** are
written down and the place is re-derived by running the program again
over them. That is exact under one sentence:

> **Everything the outside world tells the program enters through
> `pause`.**

For a long time that sentence lived in a document. What made it a type
was measuring what breaking it does: a `Writer` between two pauses says
the same thing again on every replay, and a clock read between two
pauses reads a different time. Both are in `TestDelimPersist`, and that
measurement is the reason anybody believes the rule.

So the row of a replayable program is constrained:

```scala
type Safe = Shift[?, Any] | State[?, Any] | Reader[?, Any] | Throws[?, Any]
```

**In, and why each:** `State` and `Reader` are re-threaded from the
same answers, so a replay produces the same values. `Throws` raises the
same error at the same place. `Shift` is the machine doing the
replaying.

**Out, deliberately:** `Async` and anything reaching outside — replay
performs it again. `Writer` — replay tells it again, which was
measured rather than assumed. `Resource` — replay acquires again.
`Uid` — a fresh id per run is the definition of not replayable.

The error message is the documentation, which is the point of writing
it as a type:

```
this row holds an effect that REPLAY WOULD PERFORM AGAIN, so the
program is not a pure function of its journal and a restart would not
land where the first run stood.
```

### The escape hatch, and why it is a method

```scala
def unchecked[F[+_]]: Replayable[F] = of
```

Two honest uses exist: a test that *measures* what a breach costs, and
a migration that knowingly re-runs. So the hatch exists — but it is a
**method, not a given**, which means it cannot be summoned by
accident and its name is what a reviewer sees on the line that used
it. That is the whole design: make the breach possible, make it
impossible to do silently.

This is a pattern worth stealing whenever you write a constraint. A
rule with no escape gets worked around in ways you will not find; a
rule whose escape is a given gets taken by accident; a rule whose
escape is a named method gets taken on purpose, once, with a comment.

## `At`: where this was written

A captured continuation has no useful JVM stack trace. It is resumed
on another thread, in another process, a week later, and what a
debugger shows is the interpreter's own frames. What the machine *can*
say is where in the program's own structure it stands, and that needs
one fact the compiler has and the runtime does not: the source position
of a call site.

```scala
def delimited[R, F[+_]](body: …)(using At): R ! F
Shift.delimited[Int, Pure](…)      // At("Booking.scala:31")
```

It is a **given** rather than an `inline def` you call by hand, and
that detail is the whole trick: implicit search runs at the *call
site*, so a method taking `(using At)` gets the line of whoever called
it. A library then needs no inline wrapper per door.

```scala
def door(using at: At): String = at.where
door    // "TestDelimDiagnostics.scala:36" -- the CALLER's line
```

**Cost:** one reference to an interned string literal per call that
builds a prompt. Nothing is computed at run time. Chapter 20 prices
it at 8 bytes and no measurable time.

What it buys shows up in the error nobody wants to debug without it:

```
NoPrompt names the capture, the prompt it wanted, what IS installed,
and the rule
```

— and lists the delimiters that *are* installed, innermost first, and
says so plainly when a machine has no delimiter at all rather than
printing an empty list. A `Paused` likewise knows the line of the
pause that made it. All pinned in `TestDelimDiagnostics`.

### The rule for a library's own sources

A macro cannot be expanded in the compilation run that defines it, so
`okay`'s own main sources must never *summon* an `At` — they thread
the one their caller supplied. `Shift` does exactly that. If you build
something like this, expect that constraint; it is not a bug and it has
no workaround.

## `Shift.Machine`: one machine per program

Chapter 12's rule, as evidence:

```scala
inline given of[F[+_]]: Machine[F] = ${ machineImpl[F] }
```

"Tell me whether `F` already contains a `Shift`." The answer is read
off the row at compile time, and every door that runs a machine acts
on it: outermost it runs one, inside one it installs on it. So a second
`delimited` inside a block is not a mistake any more. The rule is kept
by the door, not by the person writing the block.

It is the one of the three that is not a refusal, and the one that
does NOT propagate silently. An abstract row is a compile error asking
for the evidence as a parameter, because "I cannot read this row" is
not an answer it is willing to guess. Chapter 19's `THE LIMIT` was that
guess, and it is closed.

## What the three have in common

`Replayable` (and, until shift-merge-guard, the old one-machine guard,
`OneMachine`) states its property as **subtyping with the concrete row
on the left**, and none of them is written the natural way. The natural
way does not work, and the reason is worth knowing if you ever write a
constraint over a row:

```scala
// DOES NOT RESOLVE
given union[F, G](using Replayable[F], Replayable[G]): Replayable[F + G]
```

`F + G` is `[A] =>> F[A] | G[A]`. Matching a concrete row against it
asks the compiler to invert a union into halves; it leaves both
unsolved and reports every instance as ambiguous for both. What works
is stating the property as subtyping — `A | B <: C | D` decomposes the
**left** side, which the compiler does happily. It also keeps the
question out of `orDominator`, where an abstract row crashes dotty 3.9
outright.

And the consequence, for a guard built that way:

> An **abstract** row is not proved and not refuted — it
> **propagates**.

A helper written over `F[+_]` takes the obligation and hands it to its
caller, where the row is usually concrete. That is a feature: it is how
a row-polymorphic library stays usable. It is also the hole, because a
helper whose signature declares *no* witness is not asked for one, and
`NotGiven` reads an unknown `F` as "absent". The fix available today is
for the helper to take the witness, which propagates the obligation to
a call site that can discharge it.

`Shift.Machine` closed it for the machine by reading the row in a macro
instead of searching for a `NotGiven`: a macro can SEE that the row is
abstract and say so, where `NotGiven` can only fail to find. Region
types (`specs/delim-safety.md` stage 2, a scope tag as `runST` has)
would have closed it at the cost of a type parameter on every signature
carrying evidence, and were not needed.

## The pattern, for your own constraints

1. **Measure the breach before writing the rule.** `Replayable` is
   believed because a test watched a `Writer` say the same thing twice.
   A rule justified only by reasoning gets argued with.
2. **Put the explanation in `@implicitNotFound`.** The error message is
   where the rule is read, and it is the only documentation that
   arrives at the moment it is needed.
3. **Make the escape a named method.** Not absent, not a given.
4. **State it as subtyping, concrete row on the left.** Inductive
   instances over a union row do not resolve.
5. **Expect abstract rows to propagate**, and decide whether that is
   your feature or your hole. It will be one of the two.

---

← [20 · The costs, measured](20-the-costs-measured.md) ·
[Contents](index.md) ·
[22 · Saving and restoring →](22-checkpoints.md)
