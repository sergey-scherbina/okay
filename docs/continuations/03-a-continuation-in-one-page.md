# 3 · A continuation, in one page

> Chapter 1 ended by noticing that four unrelated problems were all
> missing the same noun. This chapter gives it a name and a shape.
> There is no library in it — not one type from this repository — on
> purpose: the idea should stick to your own code, not to an API.

---

## You already know half of it

Every programmer reasons fluently about one half of a function call:

> **what the function returns.**

Nobody finds that hard. It has a name, a type, a place in the
signature, and a whole vocabulary — return value, result, output.

There is a second half, and it is exactly as real:

> **what the caller will do with it.**

This half has no name in most languages, no type, no place in any
signature. You cannot refer to it, pass it, store it, or run it twice.
And yet it exists at every single call — it is the reason the program
continues after the `return`.

**That second half is the continuation.** Not a new thing in your
program: a name for something that was always there and was never
addressable.

## One picture

Take an ordinary expression and stop in the middle of it:

```
    total = price * quantity + shipping(order)
                               ~~~~~~~~~~~~~~~
                               we are here
```

At the moment `shipping(order)` is running, the program is in two
parts:

- **behind it** — `price * quantity`, already computed, a value now;
- **ahead of it** — a hole where `shipping`'s answer will go, then
  `+`, then the assignment to `total`, then everything the rest of the
  program does afterwards.

That second part — *the hole and everything after it* — is the
continuation of `shipping(order)`. Written as a function, it is:

```
    answer  ⟼  { total = 30 + answer ; ...the rest of the program... }
```

A language with continuations lets you **get hold of that function**.
That is the entire idea. Everything else in this book is consequences.

## Why holding it is worth anything

Because once "the rest" is a value, you can do things to it that you
cannot do to a program:

| you can | and that gives you |
|---|---|
| **not call it** | leaving early: the rest of the walk is discarded, so nothing between here and the boundary has to know |
| **keep it for later** | a program that stops and continues tomorrow, in another process |
| **call it twice** | two futures from one past: run the rest with answer A, then again with answer B |
| **wrap it** | doing something on the way back, once, at a boundary — seeing what passes through |
| **hand it to somebody else, and let them call it** | a decision taken by the caller, deep inside the callee, without the callee being unwound |

Look at that table beside chapter 1's five programs. It is the same
list. Five problems, one mechanism, because all five wanted the same
missing noun.

The last row is worth a second look, because it is the least obvious
and the oldest. If the rest of a computation is a value, it can be
passed *outward* — to a handler that is not part of it — and that
handler can call it back with an instruction. The failing importer
does not end; it waits, holding everything it had, while a stranger
decides. Chapter 15 is that, and the whole difference between an
exception and a condition is whether the rest still exists when the
handler runs.

## How much of "the rest"?

One distinction, and it is the only piece of terminology this chapter
needs.

**Undelimited**: "the rest" means *everything* — to the end of the
program. That is what Scheme's `call/cc` hands you. It is powerful and
almost unusable in a large system, because a function that captures
one has quietly taken hold of its entire caller, including code from
other teams, and no type can express what it might do with it.

**Delimited**: "the rest" means *up to a boundary you placed
yourself*. You mark a point — call it a **prompt** — and a capture
below it reaches up to that mark and no further.

```
    ┌─ boundary you installed ──────────────┐
    │   ...                                 │
    │      here: capture                    │   ← the captured part is
    │      ...                              │     exactly what is inside
    └───────────────────────────────────────┘     the box, below the point
       ...the program outside is untouched...
```

Delimited is what makes this a tool rather than a hazard:

- the captured piece is **bounded**, so its type can describe it;
- the boundary is **yours**, so a library cannot capture your caller;
- boundaries **nest**, so an inner one can be crossed deliberately to
  reach an outer one.

Everything in this book is delimited. When the word "continuation"
appears with no qualifier from here on, it means "up to the nearest
boundary somebody installed on purpose".

## The five problems, one line each

With the noun in hand, chapter 1's programs stop being five problems:

- **Leave early with an answer.** Capture the rest of the walk, and
  don't call it. The layers in between are never told, because they
  are *inside* what was discarded.
- **A producer that pushes.** At each item, capture the rest of the
  producer and hand it over with the item. The consumer now holds "the
  rest of the producing" and runs it when it wants another. That is a
  coroutine, with no thread anywhere.
- **Wait for a person.** Capture the rest of the program at the point
  of waiting. It is a value. The process may now die — as long as
  something can reconstruct that value later, the program continues
  where it stopped. (*How* it is reconstructed is chapter 22's subject
  and is not obvious: the captured function itself cannot be written
  to disk. What gets written down is the answers.)
- **Somebody else's decision.** Capture the rest of the import and
  hand it *out*, with a list of ways it could continue. The handler
  picks one and calls it. Nothing was unwound, so the file is still
  open and the forty thousand parsed rows are still there.
- **On the way back.** Wrap the captured rest instead of replacing
  it — run it, and do your work as its answer comes back through.

## Three things it is not

Worth saying now, because each is a wrong first guess that costs an
hour.

**It is not a thread.** Nothing runs concurrently. A captured
continuation is a value sitting in memory; when you call it, it runs
on your thread, now.

**It is not a `goto`.** A `goto` names a place in the code. A
continuation is a *value* with a type, produced by running the
program to a point. You can return it, store it in a map, hand it to
another function.

**It is not free, and it is not a serialisable snapshot.** It is a
closure over the work that remains. It cannot be written to a file and
read back tomorrow; anything that claims to survive a restart is doing
something cleverer, which chapter 22 explains in detail.

That chapter is worth flagging here rather than later, because
"can I just checkpoint it" is the first question the previous
paragraph provokes. The short answer: **you cannot save the program,
and you do not need to — you save what it was told.** There are four
mechanisms with different costs, and one trap, and they are all in
chapter 22.

## The one-sentence version

> **A continuation is the rest of the program, up to a boundary you
> placed, as a value you can hold: discard it, keep it, call it twice,
> or wrap it.**

If that sentence is now boring, this chapter worked.

## What is still missing

Three things, and each gets its own treatment:

1. **How to write it.** The four shapes, as code you can copy — Part
   II, starting at chapter 5.
2. **When not to.** The honest default is "don't", and chapter 4 is
   next for that reason.
3. **What it does to everything else.** Holding "the rest" as a value
   interacts with state, resources, `finally` and exceptions in ways
   you have to know before you ship one — chapter 19.

---

← [2 · What teams build instead](02-what-teams-build-instead.md) ·
[Contents](index.md) ·
[4 · Deciding: reach for it, or don't →](04-deciding.md)
