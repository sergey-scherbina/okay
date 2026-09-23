# remote-foreign — a program as data, across a process

## Overview

The callback dialogue of foreign-callbacks (specs/foreign-highlevel.md
stage 7) lets Python and R PERFORM okay operations. Its continuation is
the far side's blocked stack frame, so it can be resumed ONCE: a `Choice`
handler that resumes twice is refused. That is the right default for
ordinary Python, and the wrong one for a language whose continuations
are VALUES. In Haskell, and in any code written in the freer style, the
rest of a program is a pure function, and it can be called twice.

This builds the protocol for that case (backlog polyglot-remote-foreign).
The far side returns its program one node at a time, as DATA: an answer,
or a named operation plus an id for the function that continues it. It
keeps that function in a table under the id. okay performs the operation
with its own handlers and continues the program by id, as often as the
handler asks. This is the Foreign walker of interop-shared, with the
continuation held on the far side of a pipe.

Python is the reference far side, because it is on every machine that
runs okay-py and its lambdas are values. R's closures are values too, so
R gets it as well. Haskell gets it through a module the jar ships
(`/okay/hs/Okay.hs`) and a worker compiled with GHC; it speaks the same
five messages. (The first draft of this paragraph said GHC was not
installed. The operator remembered it had been. ghcup's directories were
there and empty, so GHC 9.14.1 was reinstalled through Homebrew and the
Haskell side was built and tested for real.)

## Protocol (one JSON line each way, on the existing shim)

    host -> {"id": n, "op": "program", "run": r, "fn": "mod:f", "args": [...]}
    host -> {"id": n, "op": "continue", "run": r, "k": k, "answer": v}
    host -> {"id": n, "op": "forget", "run": r}
    shim <- {"id": n, "ok": {"done": v}}
    shim <- {"id": n, "ok": {"perform": "name", "args": [...], "k": k}}

The host chooses `r` (unique per run); the far side chooses `k` and
keeps every continuation of run `r` until `forget r`, so the same `k`
may be continued any number of times.

## Behavior

- [x] Python: `okay.done(v)`, `okay.perform(name, *args)` and `p.then(f)`
      build a program; a function returning one is started with
      `Py.program[Out](addr).calling(cbs)(args)`, answering a `PyRun`:
      `run.program: Either[Condition, Out] ! (F + PyEval)` and
      `run.forget: Unit ! PyEval`.
- [x] MULTI-SHOT across the process: a Python program that performs
      `choose` twice, under okay's `runChoice`, answers all four
      combinations; the same continuation id is continued twice.
- [x] A named operation is a callback (the same `Py.callback` as stage 7):
      a Reader-backed program runs to its answer.
- [x] `Durable` journals `Program`/`Continue`/`Forget`; a replay needs no
      Python.
- [x] A continuation of a forgotten run, and a function that does not
      return a program, are refused by name.
- [x] `PyWorkers`: a run's continuations live in ONE worker; its
      continues go there.
- [x] R: `okay_done`, `okay_perform`, `okay_then`, and `R.program` —
      the same, with R closures as the continuations.

## Decisions

- **Continuations kept by id, released by `forget`.** The far side
  cannot know whether a continuation will be resumed again, because
  only the handler knows that. So a run holds its continuations until the
  caller forgets it, just as a held object holds its value until it is
  released. The one-shot alternative is the callback dialogue, which
  frees each frame as it returns.
- **Not replay-based multi-shot.** Re-running the far program from the
  start with the answers so far would work for any language, one-shot
  generators included, but it costs O(n^2) in the program's length, and
  it needs the far program to be deterministic, which nothing checks.
  Keeping the pure function is exact and linear.

## Results

- 2026-09-23. Python: 4 live tests. Multi-shot `Choice` across the
  process gives 11, 21, 12, 22. A callback runs under `Reader`. A
  forgotten run is refused by name, and so is a function that returns no
  program. A Durable replay needs no Python.
- Haskell (GHC 9.14.1): 4 live tests. The worker is compiled from a
  test's `Main.hs` against the shipped `Okay.hs`. Multi-shot works, a
  callback runs under `Reader`, a Haskell `error` is the condition
  `HaskellError` and the worker lives on, and a Durable replay needs no
  Haskell. `Okay.hs` compiles clean under `-Wall`, after its one partial
  `head` was replaced by a pattern.
- R: 2 live tests (multi-shot with R closures, and a Reader callback).
- Default gate: TestPyProgramShape has a scripted far side that keeps
  continuations by id, and it checks that the first continuation is
  continued exactly twice. Mutant: continuing a fixed id instead of the
  node's `k` sends the walk into a loop that runs out of memory, and the
  test fails.
- The shims move to Python 6 and R 7. `PySubprocess.speaking(command)`
  runs any process that speaks the okay wire.
