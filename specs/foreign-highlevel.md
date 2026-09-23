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

## Stage 7 — foreign-callbacks (taken before 3–6: the operator asked
for it, 2026-09-23: "А как у питона и r будет у нас с нашими эффектами и
какими-то обратными вызовами для взаимодействия? Это возможно?")

### The shape

A call that may call back is not one exchange but a short dialogue:

    host  -> {"id": 7, "op": "start", "fn": "m:fit", "args": [...], "callbacks": ["objective"]}
    shim  <- {"ask": {"cb": "objective", "args": [...], "k": 1}}     # Python called okay.call(...)
    host  -> {"op": "resume", "k": 1, "ok": 0.25}                    # okay ran the callback
    shim  <- {"id": 7, "ok": {...}}                                  # the function returned

On the okay side that dialogue is a PROGRAM: `PyEval.Start` and
`PyEval.Resume` answer a `Step` — `Done(answer)` or `Ask(callback, args,
k)` — and the loop between them runs each callback as an okay program in
the caller's row F. So a callback can ask a `Reader`, update `State`,
sleep on `Async`, write a journal, or call Python AGAIN: the shim, while
it waits for a resume, serves any request that arrives (the nesting is
strict, so one wire suffices).

It is the Foreign walker of interop-shared, across a pipe: the
continuation is Python's blocked stack frame, so it is ONE-SHOT — a
handler that resumes twice is refused by name, not answered wrongly.

### Behavior

- [x] Python: `import okay; okay.call("name", *args)` inside a function
      called with callbacks; the shim injects the `okay` module. A
      callback that fails in okay raises `okay.OkayError` in Python, with
      the condition's kind and message; Python may catch it.
- [x] R: `okay_call("name", ...)`, a function the shim defines; a failure
      is an R condition of class `okay_error` (tryCatch-able).
- [x] Scala: `Py.fn[Out](addr).calling(cbs)(args...)` answers
      `Either[Condition, Out] ! (F + PyEval)` where `cbs: Py.Callbacks[F]`
      is built from `Py.callback[In, Out](name)(f: In => Out ! F)`; `R`
      the same. Arguments and answers through `Schema` (stage 2).
- [x] A callback's program runs under the CALLER's handlers: a Python
      optimiser minimising an objective whose value comes from okay's
      `Reader`, and a callback counting its calls in `State`.
- [x] Re-entrancy: a callback that itself calls Python on the same
      worker is answered (nested exchange).
- [x] An unknown callback name, and a callback that was not offered to
      THIS call, are refused by name in the foreign language.
- [x] `Durable` journals the dialogue (`Start`/`Resume` are ordinary
      operations): a replay answers every step from the journal and
      starts no Python; the callbacks' own effects are theirs to journal.
- [x] The wire change is additive (a new op, a new message); the shim
      version still bumps (Python 3, R 4) so a host never talks to a shim
      that cannot answer it.

## Stage 3 — foreign-object-handles

## Stage 7 — foreign-callbacks (taken before 3–6: the operator asked
for it, 2026-09-23: "А как у питона и r будет у нас с нашими эффектами и
какими-то обратными вызовами для взаимодействия? Это возможно?")

### The shape

A call that may call back is not one exchange but a short dialogue:

    host  -> {"id": 7, "op": "start", "fn": "m:fit", "args": [...], "callbacks": ["objective"]}
    shim  <- {"ask": {"cb": "objective", "args": [...], "k": 1}}     # Python called okay.call(...)
    host  -> {"op": "resume", "k": 1, "ok": 0.25}                    # okay ran the callback
    shim  <- {"id": 7, "ok": {...}}                                  # the function returned

On the okay side that dialogue is a PROGRAM: `PyEval.Start` and
`PyEval.Resume` answer a `Step` — `Done(answer)` or `Ask(callback, args,
k)` — and the loop between them runs each callback as an okay program in
the caller's row F. So a callback can ask a `Reader`, update `State`,
sleep on `Async`, write a journal, or call Python AGAIN: the shim, while
it waits for a resume, serves any request that arrives (the nesting is
strict, so one wire suffices).

It is the Foreign walker of interop-shared, across a pipe: the
continuation is Python's blocked stack frame, so it is ONE-SHOT — a
handler that resumes twice is refused by name, not answered wrongly.

### Behavior

- [x] Python: `import okay; okay.call("name", *args)` inside a function
      called with callbacks; the shim injects the `okay` module. A
      callback that fails in okay raises `okay.OkayError` in Python, with
      the condition's kind and message; Python may catch it.
- [x] R: `okay_call("name", ...)`, a function the shim defines; a failure
      is an R condition of class `okay_error` (tryCatch-able).
- [x] Scala: `Py.fn[Out](addr).calling(cbs)(args...)` answers
      `Either[Condition, Out] ! (F + PyEval)` where `cbs: Py.Callbacks[F]`
      is built from `Py.callback[In, Out](name)(f: In => Out ! F)`; `R`
      the same. Arguments and answers through `Schema` (stage 2).
- [x] A callback's program runs under the CALLER's handlers: a Python
      optimiser minimising an objective whose value comes from okay's
      `Reader`, and a callback counting its calls in `State`.
- [x] Re-entrancy: a callback that itself calls Python on the same
      worker is answered (nested exchange).
- [x] An unknown callback name, and a callback that was not offered to
      THIS call, are refused by name in the foreign language.
- [x] `Durable` journals the dialogue (`Start`/`Resume` are ordinary
      operations): a replay answers every step from the journal and
      starts no Python; the callbacks' own effects are theirs to journal.
- [x] The wire change is additive (a new op, a new message); the shim
      version still bumps (Python 3, R 4) so a host never talks to a shim
      that cannot answer it.

## Stage 3 — foreign-object-handles

### Behavior

- [ ] `PyEval.Hold(fn, args)` calls the function and KEEPS its result in
      the worker, answering a `PyRef(id, pyType)`; `REval.Hold` the same
      in R (`RRef(id, rClass)`). A ref is a value on the wire
      (`{"t": "ref", "id": n}`), so it may be passed as an argument to ANY
      call — `stats::predict(model, newdata)`, `m:score(model, X)`.
- [ ] Python: `PyEval.Method(ref, name, args, hold)` calls a method of the
      held object (answering its value, or holding the result when
      `hold`), and `PyEval.Attr(ref, name)` reads an attribute.
- [ ] `Release(ref)` drops the object on the far side; a ref used after
      its release, or on a process that never held it (after a restart),
      is refused BY NAME, as a condition.
- [ ] Typed: `Py.hold(fn)(args)`, `ref.call[Out](method)(args)`,
      `ref.attr[Out](name)`, `ref.release`; arguments through `ToPy`
      (`Schema` values and refs alike), answers through `Schema`. R the
      same with `ToR`, and `R.fn(...)(ref, ...)` for R's
      function-on-object style.
- [ ] `PyWorkers`: a hold PINS its worker (out of the pool until every
      ref it holds is released); calls that name refs go to the owner;
      refs on two different workers in one call are refused by name.
- [ ] `Durable`: a whole program with handles replays from its journal
      (every step answered, no interpreter). RECOVERY past a handle —
      replay up to the crash, then continue live on a fresh process —
      meets a ref the new process never held, and that is refused by name
      rather than answered wrongly. A durable program that must survive a
      crash keeps VALUES, not handles.
- [ ] Live: scikit-learn is not assumed; the Python test holds a stdlib
      object (`collections:Counter`, `random:Random` seeded) and an R test
      holds an `lm` fit and calls `predict` on it.

## Stage 4 — foreign-inline-modules

### Behavior

- [ ] `Py.module("scoring", """def score(xs): ...""")` and
      `R.module("scoring", """score <- function(xs) ...""")`: Python or R
      source written NEXT TO the Scala that calls it. The methods are
      `inline` and REFUSE a source that is not a compile-time constant
      (`requireConst`): an interpolated or computed string does not
      compile. `PyModule`/`RModule` have private constructors, so there is
      no other way to make one.
- [ ] The engine SHIPS the modules at start
      (`PySubprocess.start(..., modules = Seq(m))`, `PyWorkers.start`,
      `RSubprocess.start`): Python gets them as files on its `PYTHONPATH`;
      R `sys.source`s each into its own environment, and `resolve` finds
      `module::fn` there before trying a package. No wire operation evals a
      string: the "untrusted input reaches the interpreter only as data"
      invariant of specs/py.md still holds, and the source is reviewed,
      versioned code in the jar.
- [ ] `m.fn[Out]("score")`, `m.hold("Model")`, `m.fn[Out]("fit").calling(cbs)`
      address the module's functions without spelling its name twice.
- [ ] A module name that is not an identifier is refused where the
      module is made.

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

- Stage 7 (foreign-callbacks, 2026-09-23).
  - Both languages work end to end on the first live run: python3 3.14,
    and R 4.4.1 in docker. TestPyCallbacks has 7 tests: an optimiser
    over a `Reader`, callbacks counted in `State`, a nested call on the
    same worker, `OkayError` caught in Python, an unoffered name refused,
    the pool, and a `Durable` replay. TestRCallbacks has the same 6 in R,
    over R's own `optimize`. TestPyDialogue (2) checks the loop without
    Python, in the default gate.
  - Mutant: resuming each ask with its own arguments instead of the
    callback's answer fails TestPyDialogue.
  - Two wire facts found on the way. jsonlite sends a named `...` as an
    OBJECT, so `okay_call` sends `unname(lapply(list(...), enc))`. The
    shim reads stdin from two places (the main loop and a waiting
    `okay.call`), so both use explicit `readline`, and one reader owns
    the buffer.
  - The R tests define their R functions by `base::source` of a file in
    java.io.tmpdir, which the docker shim mounts. The API still has no
    eval operation. A caller who addresses `base::source` has chosen to
    run a file, which is not the same as the host evaluating a string.
  - Not done, and stated: an exception THROWN out of a callback's program
    (rather than a `Left`) leaves the Python/R frame parked in
    `okay.call`. The worker stays usable, because the shim serves the
    next request from inside the wait, but that frame never returns. A
    `Left` is the supported failure channel.

- Stage 3 (foreign-object-handles, 2026-09-23).
  - Python: 6 live tests. A seeded `random.Random` reproduces
    `0.6394267984578837, 0.025010755222666936`. Also: an object's
    methods, its attribute, the object as an argument, a held method
    result, a release refused by name, a pool of one and a pool of two,
    and the Durable replay versus recovery.
  - R: 2 live tests, `as.formula`, `lm` and `predict` through handles,
    and a released ref refused.
  - Default gate: a journal test of hold, method and release.
  - Mutant: a pool that forgets to rename a ref to its worker's own id
    fails "a pool of two".
  - Design corrected before landing: pinning a worker out of the pool
    starves a pool of one (a plain call would block for ever). Workers
    stay pooled, and each exchange locks the worker. A worker that dies
    while reached through a ref is taken out of the queue before its
    replacement goes in.
