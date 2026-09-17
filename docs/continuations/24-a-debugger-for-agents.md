# 24 · A debugger for agents

> **Part VI is production.** Compiled in
> `okay-agent/src/test/scala/okay/agent/TestStepper.scala`, and the
> thing itself is `okay-agent/.../Stepper.scala`. This is the chapter
> where chapter 13's multi-shot stops being a curiosity and pays for
> itself — and where an expectation about it was refuted in writing.

---

## The problem

An agent run is a loop: ask a model, get back tool calls, perform
them, feed the results back, repeat. When it goes wrong, the question
is always the same — *would it have gone differently if that tool had
answered something else?*

Ordinarily you find out by changing the tool, rerunning from the top,
and paying for every model call again. The run is not the same run;
it is a new one that you hope is comparable.

## The shape

The stepper runs an agent program under a debugger's hand: it
**pauses at every tool call**, the operator inspects it — and may
perform it, fake it, or edit its result — and the program resumes none
the wiser.

The pause is a `shift` to a typed prompt. The captured continuation
**is** "the rest of the agent run", reified as an ordinary program
value, which is exactly what `resume` hands back.

Four properties, each pinned:

- fed the real results, a stepped run **equals the unstepped run**;
- the operator can edit a result mid-flight and the program is none
  the wiser;
- stepping with nobody watching equals not stepping — the driver is
  transparent;
- and the one this chapter is for.

## Multi-shot pays for itself

```scala
case Delim.Paused.Ask(_, resume, _) =>
  Delim.run(resume("first world")).flatMap { a =>
    Delim.run(resume("second world")).map { b => (a.finished, b.finished) }
  }
// (Some("FIRST WORLD"), Some("SECOND WORLD"))
```

The **same** continuation, resumed twice, with two different pasts.
One pause, two futures. Everything before the tool call happened once
and is shared; everything after it happened twice, independently.

This is the answer to "what if the tool had said X instead" that does
not involve rerunning anything. It works because this library's
captured continuations are multi-shot — the machine reifies segments
into immutable programs, closing over no interpreter state (chapter
13). A stack-copying one-shot implementation could not do it, and on
JavaScript could not exist.

A debugger that can ask *what if* is the whole argument for multi-shot
in one production use.

## The deletion, which is the other half

This file was written before `Delim` had named patterns, so it carried
its own `Stepping` enum — a `Paused(call, resume)` beside a `Done(a)` —
and its own driver.

That is `Delim.Paused` and `Delim.drive`, exactly. **A stepping run is
a dialogue whose questions are tool calls and whose answers are their
results.** Saying so deleted half the file.

This is chapter 9's composition argument arriving in production code.
The bespoke enum was not wrong; it was the general shape, written
again under a local name, and therefore maintained twice. Recognising
it cost one sentence and left one driver instead of two, `Paused` and
`drive` maintained in one place for every consumer.

> When a module grows its own `Paused`-shaped type, that is the
> signal. The named pattern already exists.

## The refuted expectation, stated in the source

This is the part worth copying, more than the feature.

The backlog entry that asked for this rewrite said the stepper would
gain `Delim.replay` — **a session that survives the process** — for
free, since it was becoming a dialogue anyway. It is a reasonable
inference and it is wrong, and the type system says why:

> `replay` requires `Replayable[Delim + Rest]`, and `Rest` is
> `Context + (Model + Async)`.

Re-running a stepping session would **ask the model again**. A stepping
run is a dialogue *in shape* and a live one *in substance*. To make it
durable, the model's replies must themselves become answers in the
journal — which is a different feature (chapter 23) and not a
consequence of this one.

Two things are worth taking from that paragraph:

1. **The constraint caught a design error before it was built.** Not a
   test, not a review: chapter 21's `Replayable` refused a row, and
   the refusal was the argument. That is what a discipline in the type
   system is *for*.
2. **The wrong expectation is written down where the code is**, in the
   file, permanently, in the form "this was expected, here is why it
   does not hold". A backlog entry that quietly disappears teaches
   nobody; the next person to have the same good idea finds the answer
   in the place they would have written it.

## What it buys and what it does not

**Buys:** one pause, many futures, at no model cost. An operator who
can fake, edit, or replace any tool result. A transparent driver, so
the stepped and unstepped runs are the same run. One driver instead of
two.

**Does not buy:** durability, for the reason above. A session that
outlives its process needs the model's replies in the journal, and
that is chapter 23's machinery, not this one's.

---

← [23 · Durable workflows](23-durable-workflows.md) ·
[Contents](index.md) ·
[25 · Cutting a model mid-sentence →](25-cutting-a-model.md)
