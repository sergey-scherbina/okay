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

## Stage 2 — foreign-typed-calls

### Found before starting

A Python `dict` answered by an ordinary `Call` never reached okay as a
dict. The shim encodes EVERY str-keyed dict as a frame (dict of
columns); the host's `dec` has no frame case for a value and answers
`PyNone`, and a dict whose values are not lists fails inside the shim
(`[enc(x) for x in 1]`). R has the same shape: a named list that is not a
data.frame is sent as a frame. Nothing tested it because `PyValue` and
`RValue` had no case to hold a record. A typed call needs one, so this
stage fixes the wire first.

### Behavior

- [x] Wire v2 (Python shim 1 → 2, R shim 2 → 3; the handshake refuses
      the old shim by name): `PyValue.Dict(kv)` and `RValue.Named(kv)`,
      tagged `{"t":"dict"}` / `{"t":"named"}` with ordered `kv` pairs,
      nested to any depth through the existing explicit work-list (no
      native recursion on the host). A Python `dict` or dataclass answered
      by a `Call` arrives as a `Dict`; an R named list that is not a
      data.frame arrives as `Named`. Frame operations still answer frames.
- [x] `PyCodec` / `RCodec`: `encode[A: Schema]` and `decode[A: Schema]`,
      TYPED folds over `Schema` (GADT refinement; the product and sum
      kernels `eachField`/`theCase` hold the only casts, as everywhere).
      A product is a `Dict`/`Named`; a sum is one with a `"type"` field
      naming the case; `Option` is None / NULL (a typed NA for an R
      scalar); a sequence is a list / an R vector. Decoding refuses by
      FIELD PATH (`.orders[2].qty: expected a number, got "x"`), never a
      silent default.
- [x] R has no 64-bit integer: a `Long` travels as a double while it is
      exact (|x| <= 2^53) and as its digits beyond, and decodes from any
      of the three; the frame codec's silent `toInt` truncation of a
      `Long` is the same defect and is fixed with it.
- [x] `Py.fn[Out](address)(a, b, ...)` and `R.fn[Out](address)(...)`:
      up to four typed arguments, answering
      `Either[Condition, Out] ! PyEval` (`! REval`). A decode failure is
      a `Condition("Decode", path-and-reason)`, the same channel as a
      Python exception, so a caller matches one `Left`.
- [x] `PyFrame.of[A]` and `frame.rows[A]`, the pair R has.
- [x] Live (python3 on the box; R through the docker image): a case class
      sent to a Python function that reads its fields and answers another
      case class; a dict and a dataclass answered by `Call`; the same
      round trip through an R function over a named list.

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

- Stage 2 (foreign-typed-calls, 2026-09-23).
  - The defect was shown failing first, live: `json:loads('{"a": 1}')`
    answered `Left(TypeError: 'int' object is not iterable)` and
    `{"a": [1, 2]}` answered `Right(PyNone)`. Both now arrive as `Dict`.
  - The first probe asserted only "not None" and PASSED, because the
    scalar case never reached None: it died in the shim. The probe was
    wrong, not the fix.
  - `PyEval`/`REval` became covariant. `effect` requires `F[+_]`, so
    until now nobody could write an okay program over them, only call
    `handler.handle`. The journal instances already rebuilt the call
    inside their match, so nothing else changed.
  - Tests: TestPyCodec (5) and TestRCodec (7) run in the default gate,
    without an interpreter. TestPyTyped (8, python3) and TestRTyped (5,
    R 4.4.1 in docker) are Live. The existing live TestPy and TestR
    suites are green on shims 2 and 3.
  - Two defects were found by the tests and fixed. `RCodec.long` first
    compared `x.toDouble <= 2^53`, which let 2^53+1 through as 2^53: the
    conversion rounds before the comparison. And a 20000-deep record
    overflowed munit's `assertEquals`, not the wire. The wire is
    iterative; the test now walks the answer with a loop.
  - Mutant: restoring the frame codec's `I32(x.toInt)` fails "the frame
    codec keeps a Long past 32 bits".
  - Found beside it: the host sent `I64` as a JSON double, so a Long
    past 2^53 lost digits on the way to Python. It is tagged digits now.
