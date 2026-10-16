## okay-caps-core - okay-core's Cont capture-checked, measured (not merged)

- A probe on okay-core's branch `capture-checking` (fb69ca6): `Cont.scala` under the checker — a capturing self
  type, `Cont^` node parameters, `^{this, f}` results; the bytecode identical in all 430 classes, okay-core's suites
  green, the Scala 2 module reading it. From okay-caps: a kernel program holding the transaction carried out by
  `map` or `defer` is refused; by `flatMap` (a `for`) not — an inline method's capture set is inlined into every
  caller as experimental `@retains`. Verdict in specs/okay-caps.md: partial, not merged; the guard stays; backlog
  okay-caps-next waits for `@retains` to leave experimental.
