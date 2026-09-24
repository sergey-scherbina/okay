package okay.py

/**
 * The foreign engine under neutral names (foreign-names, 2026-09-24). The
 * engine began as the bridge to Python, so its first names were `Py`,
 * `PyEval` and `PySubprocess`. The same engine and the same wire now carry
 * Python, R's shape, TypeScript, Haskell, Go and Rust, over pipes, TCP, and
 * calls into this process (specs/polyglot-one-wire.md):
 *
 *  - `Foreign`: typed calls (`fn`), held objects, programs as data
 *    (`program`), callbacks — the API `Py` has always had, in the wire's
 *    value shape;
 *  - `ForeignEval`: the effect a program performs to reach foreign code;
 *  - `ForeignWorker`: the engine over a `WireLink` (`speaking`, `connect`,
 *    `over`).
 *
 * The old names stay as aliases, so nothing written against them breaks,
 * and Python's own docs keep calling it `Py`. `Ts` is the same API in the
 * JSON codec's shape, for TypeScript.
 */
object Foreign:
  export Py.*

/** @see [[ForeignEval]] — the name this effect had while it spoke only Python */
type PyEval[+A] = ForeignEval[A]
val PyEval: ForeignEval.type = ForeignEval

/** @see [[ForeignWorker]] — the name this engine had while it ran only Python */
type PySubprocess = ForeignWorker
val PySubprocess: ForeignWorker.type = ForeignWorker
