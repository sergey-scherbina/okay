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

## Callbacks into okay

Python code can call back into okay in the middle of a call.
`import okay; okay.call("name", *args)` runs a callback that the okay
side offered, and the callback is an okay PROGRAM. It runs under the
caller's handlers: Reader, State, Async, Durable, or another call into
Python. Here a Python optimiser minimises an objective whose target comes
from okay's `Reader`:

```python
def minimise(x0):
    # a gradient-free search: every value of the objective is asked of okay
    best, fb, step = x0, okay.call("objective", x0), 4.0
```

```scala
val objective = Py.callback[Double, Double]("objective")(x => Reader.ask[Double].map(t => (x - t) * (x - t)))
val fit = Py.fn[Double]("okaycb:minimise").calling(Py.callbacks(objective))(0.0)
val best = Reader.run(3.25)(fit).runWith
```

A callback that updates `State` updates the caller's state. Two calls
from Python are counted in okay:

```scala
assertEquals(State.handle(0)(prog).runWith, (2, Right(12L)))
```

How it works: a call with callbacks is a short dialogue, not one
exchange. `PyEval.Start` answers either the result or a
`PyStep.Ask(callback, args, k)`. okay runs the callback, and
`PyEval.Resume(k, answer)` hands the answer back to the Python frame
waiting inside `okay.call`. While that frame waits, the shim serves any
request that arrives. So a callback may call Python again on the same
worker, and `PyWorkers` keeps one worker for the whole dialogue.

- **Failures.** A callback that fails in okay raises `okay.OkayError` in
  Python, with the condition's `kind` and `message`, and Python may catch
  it. A name this call did not offer is refused in Python by name.
- **Journaling.** `Start` and `Resume` are ordinary operations, so
  `Durable` journals the whole dialogue. A replay answers every step from
  the journal without starting Python, and the callbacks run again under
  their own handlers.
- **One-shot.** The continuation is Python's blocked stack frame, so it
  can be resumed ONCE. This is a coroutine in the sense of de Moura and
  Ierusalimschy \[Revisiting coroutines, TOPLAS 2009,
  doi:10.1145/1462166.1462167\]. The okay side is an effect handler over
  it \[Plotkin & Pretnar, Handling algebraic effects, LMCS 2013,
  doi:10.2168/LMCS-9(4:23)2013\].

## Held objects

Only values cross a call, so a model fitted in one call is gone by the
next. `Py.hold` keeps the result IN the worker and answers a handle
instead. The object's methods are called by name, its attributes are
read, and the handle can be passed to any function as an argument:

```scala
val acc = Py.hold("okayh:acc")().runWith.toOption.get
assertEquals(acc.call[Long]("add")(5L).runWith, Right(5L))
assertEquals(acc.attr[Long]("total").runWith, Right(8L))
assertEquals(Py.fn[Long]("okayh:total_of")(acc).runWith, Right(8L))
val fork = acc.hold("fork")().runWith.toOption.get
```

A seeded generator keeps its state between calls, as it would inside
Python:

```scala
val rng = Py.hold("random:Random")(42L).runWith.toOption.get
```

- **Release.** `ref.release` drops the object and can be called more
  than once. A ref used after its release, or on a process that never
  held it, is refused by name.
- **Pools.** In a `PyWorkers` pool a ref's calls go to the worker that
  holds it. The worker stays in the pool, so a pool of one that holds an
  object still answers plain calls.
- **Durable.** A handle names state inside ONE process. A whole
  program with handles replays from its journal without Python. But a
  recovery that continues live on a fresh process meets a ref that
  process never held, and is refused by name, not answered wrongly. A
  program that must survive a crash keeps values, not handles.

## Modules beside the Scala

A few lines of Python can live in the Scala file that calls them. They
are then reviewed, versioned and shipped with the jar, and there is no
separate package to install:

```scala
val scoring = Py.module("scoring", """
  def mean(xs):
      return sum(xs) / len(xs)
```

```scala
private lazy val w = PySubprocess.start(TestPy.python.get, modules = Seq(scoring))
assertEquals(scoring.fn[Double]("mean")(Vector(1.0, 2.0, 6.0)).runWith, Right(3.0))
val model = scoring.hold("Model")(10L).runWith.toOption.get
```

- **Constant source only.** The source must be a COMPILE-TIME CONSTANT.
  `Py.module` refuses a computed or interpolated string at compile time,
  and `PyModule` has no public constructor.
- **No eval.** The worker gets the module as a file on its path when it
  starts. No operation on the wire evaluates source, so untrusted input
  still reaches Python only as data.
- **Indentation.** The common indentation is removed, so the literal may
  be indented along with the Scala around it.
- **Pools.** `PyWorkers.start(..., modules = ...)` ships the module to
  every worker.

## A generated facade

A whole module can sit behind a typed Scala object, generated from the
module's own signatures and type hints:

    sbt "okayPy/runMain okay.py.PyFacade statistics Stats my.pkg"

prints a source file to check in. From

```python
def mean(xs: list[float]) -> float:
    """The arithmetic mean."""
```

it writes

```scala
def mean(xs: Vector[Double]): Either[Condition, Double] ! PyEval =
  Py.fn[Double]("facadedemo:mean")(xs)
```

- **Types.** A hint the generator knows becomes a type: `int` is Long,
  `float` Double, `str` String, `bool` Boolean, `bytes` Array[Byte],
  `list[T]` Vector[T], and `Optional[T]` or `T | None` Option[T]. A
  missing or unknown hint becomes a TYPE PARAMETER, so nothing is
  guessed:

  ```scala
  assertEquals(FacadeDemo.echo[String, String]("hi").runWith, Right("hi"))
  ```

- **Defaults.** A parameter with a default is left out, and the method's
  comment says which.
- **Where the description comes from.** The worker describes the module
  through `okay.describe`, a function of the `okay` module its shim
  injects. No new wire operation was needed.
- **Why generated.** A trait written by hand repeats the Python code and
  drifts from it silently. A generated one follows the code, so a change
  in Python shows up as a diff.

## Streams through Python

A Python function over a LIST becomes an okay stage over CHUNKS, and it
composes with `through` like any stage:

```scala
val out = run(okay.through(numbers(10))(Py.stage[Long, Long]("streamy:double", chunk = 4)))
```

- **Chunks.** The stage pulls up to `chunk` elements and calls the
  function once with them. It tells every element of the list that comes
  back. That list may have any length, so the stage can filter or expand
  as well as map. One message crosses per chunk, not per element, and
  the end of the input flushes a partial chunk.
- **Back-pressure.** The next pull waits for the answer, so a slow model
  holds back its source. The default gate checks that the source has
  produced exactly 4, 8, then 10 elements at the three calls.
- **State.** A stateful transformation is a held object. Its `step` is
  called per chunk and its `flush` once at the end:

  ```scala
  val out = run(okay.through(numbers(8))(win.stage[Long, Long]("step", chunk = 2, finish = Some("flush"))))
  ```

- **Failure.** A Python failure ends the stage with `PyStream.Failed`,
  naming the condition, because a stage has no error channel of its own.
- **Why chunks, not a generator.** A generator suspended on its input
  would need a second thread inside the shim. Owning the pull on the okay
  side is what `through` already composes.

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
