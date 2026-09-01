# okay-py: Python as a handler

## Overview

Python is the other half of the world's applied numerics — pandas,
numpy, scikit-learn, torch — and it enters this stack exactly the
way R did, because the shape is the same: CALL-SHAPED FOREIGN
COMPUTE. **The model is specs/r.md's, verbatim, and is not restated
here** (the policy rule: one file states a rule, others link):
calls are OPERATIONS (`PyEval`, the `REval` twin — journalable by
`Durable`, mockable by handler swap, supervised by
dead-process-throws); named functions only, NO string eval,
structurally; neutral values and frames with `Schema` at the edge;
`verify` at startup; a clean environment the parent leaks nothing
into unless config names it. This spec records only what is
PYTHON-SPECIFIC: the engines, the environment story, and the
answer to "why not Python on the JVM".

## The Python-specific parts

- **`PyEval.Call("statsmodels.api:OLS", args)`** — a function
  addressed as `module:qualified.name`, imported by the shim,
  never eval'd from source. `None`/`NaN` map like R's NULL/NA
  (distinct, stated); frames are columnar and pyarrow is
  first-class on the far side, so the Arrow road (r-arrow's twin)
  is nearer here than it was for R.
- **Engines**:
  - *Subprocess* (stage 0): `python3` running a small shim from
    okay-py — stdlib-only (json wire; cbor2 optional when
    present), reads calls, imports, answers, one process per
    session. Robust, zero administration, the batch default.
  - *Persistent worker* (stage 1): the SAME shim, long-lived —
    Python has no canonical Rserve, and does not need one: a
    resident process holding imports (torch's startup is real
    money) IS the served engine. One worker per session; the GIL
    is then irrelevant to us — parallelism is N workers, which is
    the cluster's worker model, not threads.
  - Py4J (Spark's JVM↔Python bridge) noted as prior art, not
    adopted: callback-oriented, JVM-centric, heavier than a shim
    whose whole protocol is "call, answer, condition".
- **verify** checks the interpreter version and importability +
  version of named packages (`importlib.metadata`), mismatches as
  data; the ENVIRONMENT itself (venv/uv/conda) is the analyst's to
  build and ours to name — config carries the interpreter path,
  and verify makes "wrong venv" a loud startup refusal instead of
  a subtly different model fit.

## Why not Python-on-the-JVM (the question answered once)

The value of Python and R is their package ecosystems, and those
ecosystems are C/Fortran-backed exactly where they are valuable
(numpy is BLAS; pandas is C; forecast is Fortran). A JVM
reimplementation therefore lags precisely where the point is:

- **Jython** — Python 2 only, no C extensions: no numpy. Dead for
  this purpose.
- **JEP** (embedded CPython via JNI) and **ScalaPy** (CPython via
  FFI) — the real interpreter, in OUR process: the JRI of Python.
  Full ecosystem, shared fate — a segfault in a C extension takes
  the JVM with it, and the GIL now lives inside our runtime.
  Rejected for the same one-line reason as JRI: isolation is the
  correctness boundary.
- **GraalPy** — the one worth watching: actively developed, Python
  3, runs some C extensions. Still partial exactly on the heavy
  numeric packages, still an embedding (shared fate), still heavy
  to start. If it matures into "pandas just works", it can join as
  ONE MORE ENGINE behind the same handler — an engine that happens
  to be in-process, with the shared-fate trade stated — and
  nothing above the handler changes. That is the seam doing its
  job; betting the design on it today would not be.

The subprocess boundary costs one serialization hop, which Arrow
makes cheap for frames — and buys real CPython, every wheel on
PyPI, crash isolation, and N-workers parallelism. The trade is not
close.

## Module

`okay-py`: `PyEval`/`PyValue`/`PyFrame` plus both engines and the
shim (shipped as a resource, versioned with the module — shim/host
version handshake on startup, drift refuses loudly). JVM first,
Native open via the same stdio road. Depends on okay-codec.
okay-r and okay-py may share internals (a foreign-runtime core) if
implementation finds the duplication real — an implementation
freedom, deliberately not a specced abstraction (two data points
are not a rule of three).

## Interface (stage 0, as built)

```scala
package okay.py

enum PyValue:
  case PyNone                       // Python None — distinct from NaN
  case Bool(v: Boolean); case I64(v: Long); case F64(v: Double)
  case Str(v: String); case Bytes(v: Array[Byte])
  case Arr(v: Vector[PyValue])

final case class PyFrame(cols: Vector[(String, Vector[PyValue])])
final case class Condition(kind: String, message: String)  // type(e).__name__, str(e)

enum PyEval[A]:
  case Call(fn: String, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyValue]]
  case Frame(fn: String, in: PyFrame, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyFrame]]
```

One deviation from the r.md sketch, recorded: the operations answer
`Either[Condition, _]` rather than the bare value — "errors are
data" needs a place where the condition can BE a value, and the
answer type is that place (the blob-get argument, told again).
The wire is one JSON object per line each way; None is null, NaN
and bytes ride TAGGED objects ({"t":"nan"}, {"t":"bytes","b64":..})
because JSON has neither. The shim announces {"shim": N, "python":
"x.y.z"} first; the host refuses a mismatched N loudly.

## Behavior

(the specs/r.md list applies as the contract, PyEval for REval;
Python-specific additions:)

- [ ] `module:name` addressing imports and calls; a missing module
      or attribute is a condition value naming it, the worker
      survives
- [ ] None vs NaN round-trip distinctly; a frame with a nullable
      column maps to Option
- [ ] the shim/host version handshake refuses a mismatched shim
      loudly
- [ ] verify names a missing package and a version mismatch via
      importlib.metadata; the configured interpreter path is the
      one verified (the wrong-venv test)
- [ ] a persistent worker holds imports across calls (second call
      measurably skips import); kill → dead-process-throws →
      supervisor restarts with imports cold, correctness unchanged
- [ ] the same test program passes over subprocess and persistent
      engines unchanged

## Out of scope

- GraalPy engine — the hatch is designed (one more engine behind
  the handler) but not built until "pandas just works" is true in
  our own check
- Jupyter-kernel protocol / ZMQ — a much larger protocol for the
  same call-answer shape; revisit only if notebook interop itself
  becomes the feature
- Python calling back into okay; package installation; sandboxing
  beyond process + clean-env (a hostile-code sandbox is a
  different spec if ever needed — this one assumes the author's
  own code)

## Decisions

- **The r.md model by reference, not by copy** — one statement of
  the foreign-runtime rules; this spec would otherwise drift from
  r.md one edit at a time. Rejected: a self-contained duplicate.
- **Own stdio shim over Py4J/Jupyter-kernel** — the protocol is
  three verbs; adopting a callback gateway or ZMQ machinery for it
  imports complexity without capability. Rejected: Py4J,
  jupyter-client.
- **Persistent worker as the served engine** — resident imports
  are the actual latency win; N workers beat threads-under-GIL
  without touching the GIL question at all. Rejected: an
  in-process engine for latency (shared fate), a thread pool in
  one worker (the GIL makes it a queue with extra steps).
- **Process isolation over JVM-Python** — argued in full above;
  GraalPy explicitly kept as a possible future engine behind the
  unchanged seam. Rejected now: Jython (dead), JEP/ScalaPy
  (shared fate), GraalPy-as-foundation (partial where it
  matters).

## Results

(after implementation — round-trips, the wrong-venv refusal, a
real statsmodels/sklearn call through both engines, import-hold
timings)
