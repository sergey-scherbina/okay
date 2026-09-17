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

- [x] `At.here` is a compile-time constant naming file and line
- [x] every `Prompt` a user makes through the named doors carries a
      label with its position
- [x] `NoPrompt` names the capture's position, the prompt it wanted,
      the prompts that ARE installed, and the one-machine hint
- [x] `Paused.where` answers the position of the `pause` that made it
- [x] the label costs nothing measurable: `DelimBenchmark`'s
      delimiter and capture lanes move less than noise
- [ ] a dialogue's `Broken` (specs/durable-workflow.md, stage 0)
      carries the same position, so a bad deploy points at a line
      — NOT DONE, and it belongs to that spec's stage rather than
      this one: `Stopped` names an OFFSET, and the line that would
      help is the program's, which needs the position to travel in
      the journal

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

**Landed 2026-09-17** (`TestDelimDiagnostics`, 7 tests).

The message, as it actually prints:

```
the capture at ShowNoPrompt.scala:8 named the prompt 'prompt @ ShowNoPrompt.scala:4',
which is not on the stack of the machine running it.
Installed here, innermost first:
  scope @ ShowNoPrompt.scala:8
  delimited @ ShowNoPrompt.scala:8

ONE `Delim.run` PER PROGRAM. A machine owns one prompt stack,
so a delimiter installed by an INNER run cannot be reached
from the outer one, or the other way about. The combinators
that run a machine are `delimited`, `collect`, `resumable`;
the ones that install a delimiter on the machine already
running are `scope`, `collecting`, `pausing`. See
docs/continuations-in-practice.md, "The second rule: one machine".
```

**Three findings worth keeping.**

- **`At` is a GIVEN, not an inline call, and that is what makes it
  work at all.** The position wanted is the CALLER's; implicit search
  runs at the call site, so a plain `def door(using At)` is labelled
  by whoever called it, with no inline wrapper per door. The cost is
  one reference to an interned literal.
- **The library may never SUMMON one.** A macro cannot be expanded in
  the run that defines it, so `okay`'s own main sources thread the
  `At` their caller supplied and never search for one. `Delim` does
  that everywhere; the constraint is written into `At`'s header,
  because the failure it prevents is a compile error in the CORE, not
  at a use site.
- **The label is built ONCE, and the test caught the version that was
  not.** The first cut smuggled the door's name through the position
  (`scope(body)(using At(s"delimited @ ${at.where}"))`) and produced
  `scope @ delimited @ File:41`. A private `scopeAs(what)` takes the
  door's name beside the position, and `collectAs`/`pausingAs` sit on
  it for the same reason.

**The cost, measured as a pair** (master and the lane, same box, back
to back, `-f 3 -prof gc`; history.tsv 2026-09-17):

| | master | the lane | delta |
|---|---|---|---|
| `delimPushOnly` | 24.205 ± 1.286 µs | 24.470 ± 0.717 µs | inside the error |
| `delimGenerator` | 93.190 ± 2.066 µs | 92.995 ± 3.506 µs | inside the error |
| `plainList` (control) | 4.156 µs | 4.112 µs | 1% apart — which is what makes the pair readable |
| bytes, `delimPushOnly` | 350 040.167 | 358 040.168 | **+8 000.001** |
| bytes, `delimGenerator` | 934 318.507 | 942 326.983 | **+8 008.476** |

The bytes are the verdict, because they are load-proof and they
DECOMPOSE: 1000 prompts × 8 bytes on the push lane (one reference
field), 1000 captures × 8 plus one prompt on the generator lane. No
allocation was added — the `At` value class is scalar-replaced and no
string is built.

**And the first cut was 21% slower, which is why the label is lazy.**
Building it in the constructor (`s"$what @ $where"`) read 23.220 →
28.155 µs/op on `delimPushOnly`, because that lane makes a THOUSAND
prompts per operation and a label nobody asks for is a string
nobody needs. `def label` joins two stored references on demand.

A third round was taken and DISCARDED: a `Virtualization.framework`
VM held 125% CPU and the UNCHANGED control lanes moved 30–40%
(`plainList` 4.13 → 5.46). An instrument that swings further than the
effect cannot price it.

**Deferred, deliberately:** `Delim.stack` as an operation (the
delimiter stack as a value, for a log line or a trace span). Nothing
asks for it yet, and the error path — which is what hurt — does not
need it.
