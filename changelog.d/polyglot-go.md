## polyglot-go - Go programs on okay's wire

- A Go worker process, as Haskell's. The jar ships a Go package `okay`
  (standard library only): programs as data (`Prog`, `Done`, `Perform`,
  `Then`), the wire's values, and `Serve`.
- Continuations are Go closures kept by id, so okay continues them more
  than once (multi-shot).
- Typed operations: `okay.Op[A]`, `okay.Send` and `okay.Bind`, with
  `Go.ops(pkg, callbacks)` generating one constructor per operation from
  the Scala callbacks' Schemas.
- `GoWorker.build(dir)` runs `go build` offline, and a compile error
  refuses with Go's words. A panic is a `GoError` condition, and the
  worker lives on.

Checked live with Go 1.27.1: multi-shot, a typed program under a Scala
Reader, the panic, and a wrong argument type refused by `go build`. A
mutant is caught.

Docs: docs/go.md; docs/rust.md's Go section updated; the Go row of
"Where each language names okay's effects".
