# foreign-highlevel — Python and R above the call

## Overview

okay-py and okay-r (specs/py.md, specs/r.md) are call-shaped: okay
addresses a named function in a worker process and gets a value or a
condition back, built by hand from `PyValue`/`RValue`. That is the right
FLOOR — crash isolation, real CPython and R, no eval of a string — and
this spec builds the storeys above it, for Python and R both, one lane
each (operator, 2026-09-23: "Да ок делай. В R тоже все что можно
сделай").

The model does not change: a call is an OPERATION of an okay effect
(`PyEval`, `REval`), handled by a subprocess engine, mockable by swapping
the handler. Everything below is either a new operation, a new way to
BUILD operations, or a new instance over them.

## Stages

1. **foreign-journalled** — `Durable` journals Python and R calls.
   `Journalled` moves from okay-agent to okay-codec (`okay.codec`), where
   okay-py and okay-r can see it without depending on okay-agent;
   okay-agent keeps the name as an alias and the `Tool` instance moves to
   `Tool`'s companion. Instances for `PyEval` and `REval` live in their
   enums' companions, so they are found without an import.
2. **foreign-typed-calls** — a foreign function as a typed Scala
   function through `Schema`: `Py.fn[In, Out]`, `R.fn[In, Out]`. Python
   frames gain `rows[A]`/`PyFrame.of[A]`, which R has.
3. **foreign-object-handles** — objects kept on the far side behind a
   `Resource` handle; methods by name; a handle pins its worker.
4. **foreign-inline-modules** — Python/R source beside the Scala,
   shipped as a module; a literal only, no runtime eval.
5. **foreign-module-trait** — a module or package behind a trait.
6. **foreign-streaming** — a generator as a `Stage`, in chunks.
7. **foreign-callbacks** — Python/R performing okay operations mid-call.
8. **foreign-managed-env** — `uv`/`renv` from a declared environment.

Each stage is its own lane and appends its Decisions and Results here.

## Stage 1 — foreign-journalled

### Behavior

- [x] `okay.codec.Journalled` is the trait, unchanged in shape;
      `okay.agent.Journalled` still names it (a type alias), and
      `Durable.tools`, `TestDurableAnyOp` and every existing Durable test
      are green unchanged.
- [x] `Journalled[PyEval]`: the journal's `op` is the function's
      address; the fingerprint is the address plus a SHA-256 of the
      encoded arguments (and frame), so a replay whose inputs drifted is
      refused by the journal's existing drift check; the answer — value
      OR condition — is written as the module's own wire JSON and read
      back equal, None and NaN still distinct.
- [x] `Journalled[REval]` the same, with NA and NULL still distinct.
- [x] A durable program calling Python twice, run, then REPLAYED from its
      journal with a handler that fails on any call: the replay answers
      both calls from the journal and calls nothing (no live Python
      needed — a mock handler counts the calls, the TestRMock precedent).
- [x] A frame round-trips through the journal (Frame operations), and a
      condition does (a failing call replays as the same condition).
- [x] `withKey` returns the operation unchanged — a subprocess call has
      nowhere to carry an idempotency key, so these operations must not be
      declared `OnRepeat.WithKey`; said in the instance's comment.

### Decisions

- **Why move the trait rather than add a bridge module.** The trait needs
  `Handler` (core) and `Json` (okay-codec) and nothing of okay-agent; its
  one agent-specific part is the `Tool` INSTANCE, which belongs in
  `Tool`'s companion anyway (implicit scope finds it there). A bridge
  module `okay-py-durable` would be one more artifact for users to know
  about, for a dependency that was never real. The build.sbt comment on
  okayR ("Durable journals R steps because they are operations, not
  because the modules know each other") is made literally true.

## Results

- Stage 1 (foreign-journalled, 2026-09-23). Eight tests with no live
  interpreter (TestPyJournal 5, TestRJournal 3), green on the first run;
  every existing okay-agent test is unchanged and green. The trait moved
  with its whole doc comment; the only code that named its companion was
  the `Tool` given, now in `Tool`'s companion. Mutant: a Python
  fingerprint without the argument hash fails "drifted inputs are
  refused". The instances' `perform` reconstructs the call inside its
  own match, as the `Tool` instance does, so the answer's type is exact
  and neither half of `(answer, written)` is a cast.
