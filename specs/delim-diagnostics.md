# Seeing what a captured continuation is doing

## Overview

The standing complaint against continuations, and the one this
library has been admitting in its docs rather than answering: *stack
traces stop describing your code*. The machine reifies the
continuation, so a JVM stack shows `Delim$.loop`, `Delim$.step` and
`Free.resume` — the interpreter's own frames — and none of the call
chain the author wrote. The same blindness shows up in three other
places, and they are all the same missing fact:

- `NoPrompt` says *"shift to a prompt that is not on the stack"* and
  nothing else: not which prompt, not which prompts ARE on the stack,
  not where the capture was written. It is the error a newcomer meets
  first (delim-nesting, 2026-09-17) and it tells them nothing.
- a `Paused` in a production log is `Ask(question, <function1>)`: you
  can see what it is asking and not where it stands.
- a dialogue that does not move is indistinguishable from one that is
  waiting correctly.

None of this needs a JVM stack trace. The machine HOLDS the
information — the prompt stack is `Segs`, and every delimiter and
every capture is written at a source position the compiler knows.

## Design

**One inline macro, `okay.At`.** `At.here: String` expands to
`"Booking.scala:31"` at the call site, a compile-time constant with
no runtime cost beyond a reference to an interned string. The
`direct` macro already reads `quotes.reflect.Position`; this is that,
exposed.

**A prompt carries a label.** `Prompt[R]` gains `val label: String`,
defaulted from `At.here` by every constructor that a user calls
(`prompt`, `scope`, `delimited`, `collecting`, `pausing`, `collect`,
`resumable`). The label is what a delimiter is CALLED — `"collect @
Walk.scala:12"` — and it is the only field; a prompt's identity stays
its identity.

**`NoPrompt` prints the stack.** The machine already walks `Segs` to
split it; on failure it walks it to render it:

```
NoPrompt: capture at Booking.scala:31 named the prompt
  'resumable @ Service.scala:12' (Dialogue[String, Int, …])
which is not on THIS machine's stack. Installed here, innermost first:
  collect @ Walk.scala:12   (List[Int])
  delimited @ Job.scala:40  (Int)
Hint: a delimiter installed by an inner `Delim.run` is on another
stack — `scope`/`collecting`/`pausing` install on this one
(docs/continuations-in-practice.md, "The second rule: one machine").
```

The hint is worth the lines: this message IS the documentation for
the one hazard the type system does not close.

**A `Paused` knows where it stands.** `Delim.pause` is already inline,
so it can record `At.here`: `Paused.Ask` gains `at: String`, and
`Paused.where` answers it. A stuck dialogue in a log then reads
`waiting at Booking.scala:31 on "Pay 270 for Kyiv?"`, which is the
difference between an incident and a puzzle.

**The delimiter stack as a value, not only in an error.**
`Delim.stack: List[String]` inside a running machine (an operation
that answers the current marks) — so a log line, a metric label or a
trace span can carry "where in the program this is" the way a stack
trace would in ordinary code. This is the piece that makes
`okay-obs`'s `Log` useful inside a captured program.

## What this does NOT do

- It does not reconstruct a JVM stack trace. A continuation that is
  resumed on another thread, in another process, a week later, HAS no
  meaningful JVM stack, and pretending otherwise would be a lie with
  a cost. What it gives instead is the program's own structure, which
  is the thing the author recognises.
- It does not trace inside pure code between captures. A `direct`
  block's ordinary lines are ordinary lines; the debugger is still a
  debugger there.

## Behavior

- [ ] `At.here` is a compile-time constant naming file and line
- [ ] every `Prompt` a user makes through the named doors carries a
      label with its position
- [ ] `NoPrompt` names the capture's position, the prompt it wanted,
      the prompts that ARE installed, and the one-machine hint
- [ ] `Paused.where` answers the position of the `pause` that made it
- [ ] the label costs nothing measurable: `DelimBenchmark`'s
      delimiter and capture lanes move less than noise
- [ ] a dialogue's `Broken` (specs/durable-workflow.md, stage 0)
      carries the same position, so a bad deploy points at a line

## Decisions

- **A label, not a full position type.** `String` interned at compile
  time is one field and no allocation; a structured `Position` would
  buy a formatter nobody asked for.
- **Labels are on PROMPTS, not on captures in the tree.** A capture's
  position travels in the error and in the `Paused`; putting it on
  every `Bind` would be a tracing system, which is `okay-obs`'s job
  and is not free.
- **The hint text lives in the exception.** The rule it states was
  learned by falling into it; a message that teaches is cheaper than
  a doc nobody reads at 3am.

## Out of scope

- A time-travelling debugger over a `Paused` (fork it and run both
  futures — `Stepper` already does exactly that, and it is a use of
  the model, not a feature of it).
- Rewriting JVM stack traces to look like the program's.

## Results

(none yet — written 2026-09-17)
