# 13 · Multi-shot: a continuation is a value

> Compiled in `src/test/scala/TestBookMultiShot.scala`. The chapter
> that closes Part III, and the one whose consequences are easiest to
> get wrong — so every claim about what happens twice is a test.

---

## The fact

`k` is an ordinary value. Nothing stops you calling it more than once:

```scala
Delim.delimited[List[Int], Pure]:
  direct:
    val x = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
    List(x * 10)
// List(10, 20)
```

The rest of the block — `List(x * 10)` — ran twice, with `x` bound to
`1` and then to `2`. **One past, two futures**, and the two results
were combined by the handler.

This is not an exotic capability bolted on. It is what "the rest of
the program is a value" means when you take it literally, and it is
why chapter 3 put "call it twice" in the table beside "don't call it".

## What it gives you

**Nondeterminism, for free.** Two captures, each resumed twice, and
the cross product appears with no backtracking machinery anywhere:

```scala
val n = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
val s = !Delim.shift[String](k => k("a").flatMap(a => k("b").map(b => a ++ b)))
List((n, s))
// List((1,"a"), (1,"b"), (2,"a"), (2,"b"))
```

Read the body again: it is straight-line code that computes **one**
pair. The four came from resuming, not from anything the body did.
That is how a logic-programming layer is built — a `choose` is a
capture resumed once per alternative, and `fail` is a capture not
resumed at all.

**A debugger that can ask "what if".** Chapter 24 is a production one:
fork an agent run at a tool call, feed two different tool results,
compare the outcomes. The run is a value, so the fork is a second
call.

**Backtracking search**, parser alternatives, property shrinking —
all the same move.

## What goes wrong, measured

Two things, and both are consequences of the same fact rather than
bugs.

### One · Effects in the captured part run once per call

```scala
val x = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
log = log :+ s"ran with $x"        // IN the continuation
List(x)
// log == List("ran with 1", "ran with 2")
```

The log line is *after* the capture point, so it is part of the rest
of the program — and the rest of the program ran twice. Two entries.

This is obvious when the effect is a `log`. It is not obvious when the
effect is a `POST /charge`, and that is the mistake: **anything after
a capture point that talks to the outside world happens once per
resumption.**

Code *before* the capture point is not affected, and the suite pins
that too:

```scala
opened += 1                        // NOT in the continuation
val x = !Delim.shift[Int](k => ...)
// opened == 1
```

So the practical rule is positional: draw a line at the capture. Above
it, once. Below it, once per resumption.

### Two · A `var` below the line is written by every branch

```scala
val x = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
last = x
// last == 2
```

`last` does not hold "the answer" — it holds whatever the last branch
to run put there. A mutable cell shared across resumptions is a cell
shared between futures that were supposed to be alternatives, and the
value in it afterwards is an artefact of execution order.

If you need per-branch state, it must be *in the value*, not in a
cell: that is what the handler's `flatMap` is doing when it combines
`k(1)` and `k(2)`.

### And the one that is not a problem

Calling `k` **zero** times is the early exit of chapter 5, and nothing
below the line runs at all:

```scala
val _ = !Delim.shift[Int](_ => okay.pure(99))   // k dropped
ran = true                                       // never happens
// 99
```

Zero, one, many: the same mechanism, and only "many" has the
consequences above.

## What this costs

Two resumptions do twice the work below the line. That is not
overhead, it is the feature — but it means a capture inside a loop
that resumes twice is exponential in the nesting depth, and the second
example in this chapter is the smallest instance of that: two
captures, two resumptions each, four executions of the tail.

Chapter 20 has the measured numbers for a single capture. For
multi-shot the cost is whatever your own combinatorics say, and the
honest planning advice is that a search built this way is a search: it
has the complexity of the search, not of the machinery.

## When a runtime gives you only one shot

Worth knowing, because it decides what you can port.

Continuations implemented by **copying a stack** — Loom's virtual
threads, OCaml 5's effects, most coroutine implementations — are
naturally **one-shot**: the captured thing is a stack segment, and
resuming it consumes it. They are excellent for pausing and resuming,
which is chapter 7's shape and most of what people want.

They cannot do this chapter. No forking a run, no cross product, no
"what if the tool had answered differently".

Here the continuation is **a program value** — the machine reifies the
captured segment into an ordinary immutable program, closing over no
interpreter state — so calling it twice is calling a function twice.
That is the design decision that makes multi-shot available at all,
and it is also why it survives on all three platforms including
JavaScript, where there is no stack to copy.

## What Part III established

| | |
|---|---|
| 10 | a boundary is a **typed value with identity**, which is what lets you cross a specific one |
| 11 | four captures, of which **three behave distinctly**; use `shift` |
| 12 | one machine owns one prompt stack; the guard catches the shape people write and **not** an abstract row |
| 13 | the rest of the program is a value, so it can run **zero, one or many times** — and effects below the capture line run once per resumption |

Part IV builds with all of it: a new effect from a prompt, resumable
exceptions, why `!` works at all, and where a capture sits in an
effect system.

---

← [12 · One machine, one prompt stack](12-one-machine.md) ·
[Contents](index.md) ·
[14 · A new effect from a prompt →](14-a-new-effect.md)
