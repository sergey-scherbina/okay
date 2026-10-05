## cont-safe-mode — a safe mode at compile time and at run time: full trampolining with no re-execution

Re-execution (cont-js-depth stage 4) asks a strict body to be pure or
idempotent up to its `k` call. Bodies with side effects now have a mode
of their own, chosen per scope at compile time and globally at run time
(operator, 2026-10-05).

- `import okay.Cont.safe.given` — every `shift` body that uses `k` is
  CPS-transformed (`k` is data, nothing waits on the host stack, nothing
  runs twice) on the JVM, Scala.js and Native, or it is a compile error
  that says how to write it. A side-effecting body runs a million deep
  with each effect exactly once, in order.
- `import okay.Cont.noReplay.given` — an opaque body compiles as before
  and is never re-executed: its `k` is a barrier.
- `Cont.setMode` / `-Dokay.cont.mode=auto|replay|safe` for the strict
  leaves left. `auto`, the default, is the optimized road: re-execution
  on Scala.js, a fresh stack on the JVM and Native.

`shift` takes the scope's choice as a `using` parameter, so the import
counts as used, and `Shifts.default` sits in the implicit scope. A
default argument there crashed the compiler inside another inline
method.

Spec: specs/cont-js-depth.md, stage 5. Docs: docs/cont-stack.md, "Safe
mode". Tests: TestContSafeMode.
