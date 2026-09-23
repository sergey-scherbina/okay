# okay-py

Python as a handler (specs/py.md; the model is specs/r.md's,
verbatim): call-shaped foreign compute. Calls are OPERATIONS —
journalled by `Durable`, mockable by handler swap, supervised by
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

## Typed calls

A Python function can be called as a typed Scala function. Arguments go
out through their `Schema`, and the answer comes back through `Out`'s:

```scala
val total = Py.fn[Total]("okaytyped:total")
assertEquals(total(Order("kyiv-7", 3, 2.5)).runWith, Right(Total("kyiv-7", 7.5, None)))
```

The Python function receives a plain `dict` (`order["qty"]`). The answer
may be a `dict` or a dataclass:

```scala
assertEquals(Py.fn[Order]("okaytyped:an_order")().runWith, Right(Order("kyiv-7", 2, 1.5)))
```

How values cross:

- A case class is a `dict` of its fields.
- An enum or sealed trait case is its dict plus a `"type"` field naming
  the case. This is the discriminated-union shape pydantic and
  dataclass libraries already read.
- `Option` is `None`, and a sequence is a list.
- Integers past 2^53 cross exactly: a JSON number is a double, so the
  wire carries such an integer as its digits.

`Py.fn` answers `Either[Condition, Out] ! PyEval`, an ordinary okay
program. It runs under whichever `PyEval` handler is installed: a
subprocess, a worker pool, a mock, or `Durable` over any of them. A
Python exception and an answer of the wrong shape arrive on the same
`Left`. A wrong shape names the field:

```scala
assertEquals(wrong, Left(Condition("Decode", ".qty: missing")))
```

Frames get the same treatment. `PyFrame.of(rows)` turns case classes
into columns, and `frame.rows[A]` turns them back:

```scala
val frame = PyFrame.of(Vector(Order("a", 1, 1.0), Order("b", 2, 2.0))).toOption.get
assertEquals(back.flatMap(_.rows[Order]), Right(Vector(Order("a", 2, 1.0), Order("b", 4, 2.0))))
```

Wire v2 (shim 2) added the record. Before it, the shim sent every
string-keyed `dict` as a frame. A dict answered by a call either failed
in the shim or reached okay as `None`.

## Journalled by Durable

A Python call is an operation, and `PyEval` carries its own
`Journalled` instance (in its companion, so no import): okay-agent's
`Durable` records each call and answers it from the journal on replay,
without starting Python.

```scala
val live = Durable.over[PyEval](canned(ran), j)()
assertEquals(live.handle(PyEval.Call("statistics:median", xs)), Right(F64(2.0)))
val replay = Durable.replayingOver[PyEval](j)
assertEquals(replay.handle(PyEval.Call("statistics:median", xs)), Right(F64(2.0)))
assertEquals(ran.get, 2, "replay touches no Python")
```

The journal's `op` is the function's address. The fingerprint is the
address plus a SHA-256 of the encoded arguments, so a replay whose inputs
changed is refused with `Durable.Drift` instead of being answered wrongly.
The answer is written in the module's wire JSON, value or condition, so
None, NaN, bytes and an integral float come back as they went in. A
subprocess call has nowhere to carry an idempotency key, so do not
declare it `OnRepeat.WithKey`. okay-py does not depend on okay-agent:
the `Journalled` trait lives in okay-codec. The idea is the one Durable
itself rests on, event sourcing of a computation's answers
\[Fowler 2005, "Event Sourcing"\].
