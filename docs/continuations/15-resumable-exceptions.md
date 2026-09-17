# 15 · Resumable exceptions: signalling instead of unwinding

> Compiled in `src/test/scala/TestBookResumable.scala`. This finishes
> chapter 1's fifth program, and it is the oldest application of
> everything in this book — Common Lisp had it decades before the
> theory tidied it up.

---

## The difference, in one sentence

> An exception **unwinds**: by the time the handler runs, the
> computation that failed is gone. A condition **signals**: the
> handler runs while that computation is still standing, and may put
> it back to work.

Everything else follows. And notice that "put it back to work" is only
possible if the rest of the computation is a value somebody is
holding — which is why this chapter is in this book and not in a book
about error handling.

## Chapter 1's importer, finished

The requirement: a fifty-thousand-row file, a bad row at forty
thousand, and **four different callers who want four different
things**.

Here is the importer. Read it for what is *missing*:

```scala
def parse(line: Int, raw: String): Int ! Op =
  raw.toIntOption match
    case Some(n) => pure(n)
    case None => signal[Int](Malformed(line, raw))
```

No `strict: Boolean`. No `onError` callback. No error type in the
return. No decision about what a bad row *means* — because the
importer does not know, and this is the first time in the book it has
been allowed to say so.

Around it, one **restart frame per row**, so that abandoning a row
abandons exactly one row:

```scala
def load(rows: List[String]): Vector[Int] ! Op =
  def go(i: Int, left: List[String]): Vector[Int] ! Op = left match
    case Nil => pure(Vector.empty)
    case r :: rest =>
      for
        head <- within[Option[Int], Pure]("skip")(parse(i, r).map(Some(_)))(_ => None)
        more <- go(i + 1, rest)
      yield head.fold(more)(_ +: more)
  go(1, rows)
```

`within("skip")(body)(recover)` offers a restart called `skip`: if
somebody invokes it, this frame — and only this frame — unwinds, and
`recover` supplies its answer.

## Four callers, one importer, nothing recompiled

```scala
// the nightly batch: drop the row, keep the file
case (Malformed(_, _), _) => Invoke("skip", ())
// Vector(10, 20, 40)

// the migration: substitute, in place
case (Malformed(_, _), _) => Resume(0)
// Vector(10, 20, 0, 40)

// the compliance run: refuse, and name what stopped it
(_, _) => Fail
// throws Unhandled(Malformed(3, "oops"), menu = ["skip"])

// and a policy may decide per row, because it SEES the condition
case (Malformed(line, _), _) =>
  if line > 2 then Resume(-1) else Invoke("skip", ())
// Vector(10, 20, -1, 40)
```

Four behaviours, one importer, no flag, no recompilation of the
library, and no vocabulary crossing the boundary in either direction:
the importer never learns the word "nightly", and the caller never
learns how to parse.

## What makes it *resumable* and not merely catchable

`Resume(0)` does something a `catch` cannot: the value **comes back at
the signal point**, and the computation continues from there. The
suite proves the consequence that matters:

```scala
assertEquals(got, Vector(10, 20, 99, 40))
assertEquals(parsed, 3)      // rows 1,2 not re-parsed; row 4 still reached
```

Nothing unwound. The two rows parsed before the bad one were not
parsed again, and the row after it was still reached. Compare with the
exception version, where the handler's only options are "abandon the
import" or "start it over".

That is the whole value proposition, and it is the reason this shape
is worth the machinery: **the expensive thing — work already done — is
preserved across a decision made by somebody else.**

## The menu is part of the interface

When the policy declines, the report names both the condition *and the
restarts that were on offer*:

```
unhandled condition: Malformed(3,oops) (restarts on offer: skip)
```

This is more useful than it looks. An operator reading that line
learns that the importer *could* have skipped, and that nobody chose
to. A plain exception tells you what went wrong; a condition tells you
what could have been done about it.

## The traps, which are real

**A restart set is an API.** `skip`, `useDefault`, `abort` are a
protocol between two pieces of code that never see each other. Adding
one is a compatible change; removing or renaming one breaks callers
silently — invoking a restart that is not on the menu is a run-time
error, not a compile-time one. Keep the set small and name the
restarts after *what they do to the data*, never after *who asked*.

**Restarts by name are dynamic; by handle they are lexical.** A policy
invoking `"skip"` reaches the innermost frame of that name, which is
usually what you want and is occasionally a surprise when two nested
frames share a name. This library also offers a handle — a restart
targeted by identity — for when you mean *that* frame. It is the same
distinction chapter 10 made about prompts, arriving again.

**Resuming into a frame that holds a resource has chapter 19's
consequences.** The frame is still live, so the resource is still
open — which is the point — but a policy that resumes a hundred times
holds it a hundred times longer.

**An interactive restart is a pause wearing a hat.** If the policy is
"ask a human", you have chapter 7's shape: the decision arrives from
outside, later, and the program must survive the wait. Do not build
that on the condition system alone; build it on the pause, and let the
condition system be the vocabulary.

## When not to use it

- **One caller, one policy.** A parameter. This whole chapter is about
  the case where policies multiply.
- **The failure is genuinely fatal.** If nothing can continue, an
  exception says so more simply, and `Fail` is there for the policy
  that agrees.
- **The decision is local.** If the code that meets the problem also
  knows what to do, it should just do it. Signalling exists to move a
  decision to somebody who knows better, and if nobody does, the
  machinery is ceremony.

---

← [14 · A new effect from a prompt](14-a-new-effect.md) ·
[Contents](index.md) ·
[16 · Continuations and monads →](16-continuations-and-monads.md)
