# polyglot-go — Go on okay's wire

## Overview

Go code is mostly SERVICES, and a Go runtime does not belong inside the
JVM. `-buildmode=c-shared` would load a second garbage collector, a
second scheduler, and signal handlers the JVM also wants
(docs/rust.md, "Go"). So Go reaches okay the way Haskell does: as a
WORKER PROCESS on okay's line protocol (specs/remote-foreign.md).

- **Programs as data.** A Go program is written in `okay.Prog` (`Done`,
  `Perform`, `Then`). Each `Perform` is a callback the Scala caller
  offered, run as an okay program under the caller's handlers.
- **Multi-shot.** A continuation is a Go closure, which the worker keeps
  under an id. okay may continue it more than once, as a `Choice`
  handler does.
- **The same engine.** `PySubprocess.speaking` drives the worker, so
  `Py.program`, the callbacks and `Durable` are unchanged.
- **Typed operations.** Go has generics but no type-level lists, so a
  program's SET of effects cannot live in its type, as it cannot in
  Frege. Each OPERATION can be typed: `okay.Op[A]` names an operation,
  its arguments and a decoder for its answer, and `okay.Send(op)` is a
  `Program[A]`. `Go.ops(pkg, callbacks)` generates the operations from
  the Scala callbacks' Schemas, as `Ts.ops` and `Hs.ops` do.

## Stage 1 — the worker

- [ ] `okay.go` (standard library only) holds:
      - the wire's values, encoded exactly as okay-py's shim and
        `Okay.hs` do;
      - `Prog`, `Done`, `Perform`, `Then`;
      - `Serve(programs)`, where each request's panic is a condition by
        name and the worker lives on.
- [ ] `GoWorker.build(dir)` writes `okay/okay.go` and a `go.mod` if there
      is none, and runs `go build` offline (`GOTOOLCHAIN=local`). A
      compile error refuses with Go's words.
- [ ] Typed operations:
      - `Program[A]`, `Pure`, `Bind`, `Op[A]`, `Send`, and the decoders
        `Int`, `Float`, `String`, `Bool`, `ListOf`;
      - `Go.ops(pkg, callbacks)` writes one constructor function per
        operation: `func PriceOf(a0 string) okay.Op[float64]`.
- [ ] (Live, go) The tests cover:
      - multi-shot: every branch of two choices;
      - an operation as a Scala callback under the caller's Reader;
      - a panic as a condition, with the worker running on;
      - a typed program through the generated operations;
      - `go build` refusing a wrong argument type.

## Stage 2 — Go compiled to WebAssembly

`GOOS=wasip1 GOARCH=wasm go build` makes a WASI module with no TinyGo.
With the Chicory road of specs/polyglot-rust.md stage 3, a Go plugin
runs sandboxed inside the JVM. That is the road for untrusted Go code.

## Decisions

- **A process, not `c-shared`**, for the runtime reasons above. The cost
  is a pipe per call, which is the same trade Python, R, TypeScript and
  Haskell make on this wire.

## Results
