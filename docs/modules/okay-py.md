# okay-py

Python as a handler (specs/py.md; the model is specs/r.md's,
verbatim): call-shaped foreign compute. Calls are OPERATIONS —
journalable by `Durable`, mockable by handler swap, supervised by
dead-process-throws. Named functions only: the enum has no
eval-a-string case, structurally, so untrusted input reaches Python
only as data.

| | |
|---|---|
| `PyEval` | `Call("module:qualified.name", args)` / `Frame(fn, frame, args)` — both answer `Either[Condition, _]`: a failing call is DATA and the worker survives it |
| `PyValue` / `PyFrame` | None and NaN are DISTINCT; bytes and integral floats ride tagged past JSON's gaps; frames are columnar (dict-of-lists on the far side) |
| `PySubprocess` | stage 0: one `python3` per session running the stdlib-only shim SHIPPED WITH THE MODULE — the shim/host version handshake refuses drift loudly; the child environment is CLEAN (the parent leaks nothing unless config names it) |
| `verify` | importlib.metadata presence/version per package, mismatches as data — the wrong venv becomes a loud startup refusal instead of a subtly different model fit |
| `PyWorkers` | stage 1: N resident processes behind the SAME handler shape — parallelism is N workers and the GIL is then irrelevant; a dead worker throws to its caller and the pool replaces the corpse COLD |

Why not Python-on-the-JVM is answered once in the spec: Jython is
dead for numerics, JEP/ScalaPy share fate with C-extension
segfaults, GraalPy is watched — the subprocess boundary buys real
CPython, every wheel, crash isolation, N-workers.
