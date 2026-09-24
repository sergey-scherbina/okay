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

- [x] `okay.go` (standard library only) holds:
      - the wire's values, encoded exactly as okay-py's shim and
        `Okay.hs` do;
      - `Prog`, `Done`, `Perform`, `Then`;
      - `Serve(programs)`, where each request's panic is a condition by
        name and the worker lives on.
- [x] `GoWorker.build(dir)` writes `okay/okay.go` and a `go.mod` if there
      is none, and runs `go build` offline (`GOTOOLCHAIN=local`). A
      compile error refuses with Go's words.
- [x] Typed operations:
      - `Program[A]`, `Pure`, `Bind`, `Op[A]`, `Send`, and the decoders
        `Int`, `Float`, `String`, `Bool`, `ListOf`;
      - `Go.ops(pkg, callbacks)` writes one constructor function per
        operation: `func PriceOf(a0 string) okay.Op[float64]`.
- [x] (Live, go) The tests cover:
      - multi-shot: every branch of two choices;
      - an operation as a Scala callback under the caller's Reader;
      - a panic as a condition, with the worker running on;
      - a typed program through the generated operations;
      - `go build` refusing a wrong argument type.

## Stage 2 — Go compiled to WebAssembly

`GOOS=wasip1 GOARCH=wasm go build -buildmode=c-shared` makes a WASI
reactor module with no TinyGo. Its functions are exported with
`//go:wasmexport`. Under `WasmLib` (Chicory, specs/polyglot-rust.md
stage 3), a Go plugin runs sandboxed inside the JVM, with no Go runtime
beside the JVM's: it runs inside the module. This is the road for
untrusted Go code.

- [ ] `okay-rust/kernels/sha256-go`: SHA-256 from Go's standard
      library, over the Rust kernel's ABI shape: `okay_alloc`,
      `okay_free`, and `okay_sha256(in, n, out)`.
- [ ] `enum Digest[+A] derives okay.Effect` has one operation, `Sha256(bytes)`.
      - `Digest.jdk` is the handler over `MessageDigest`.
      - `Digest.wasm(lib)` is the handler over the Go plugin under
        Chicory.
- [ ] (Live, go) THE LAW: for inputs of many sizes (0 bytes to 64 KiB,
      including every length around the 64-byte block), the Go plugin's
      digest is the JDK's.

## Decisions

- **A process, not `c-shared`**, for the runtime reasons above. The cost
  is a pipe per call, which is the same trade Python, R, TypeScript and
  Haskell make on this wire.

## Results

- Stage 1 (polyglot-go, 2026-09-24).
  - Go 1.27.1 from Homebrew. `okay.go` uses the standard library only,
    and `go vet` is clean. The first manual run on the wire answered
    `{"done":22}` after two continues.
  - TestGoProgram (Live, go), four tests, green on the first run:
    - multi-shot across the process (11, 21, 12, 22);
    - the typed `total` through the generated `shop` package under the
      Scala Reader (6.0);
    - a panic as `GoError`, with the worker running on;
    - `go build` refusing `shop.PriceOf(qty)` ("cannot use qty").
  - Mutant: a worker that drops a continuation after its first use fails
    the multi-shot test, and the panic test too, because the second run
    needs the same continuations.
  - Number encoding follows `Okay.hs`: integers below 2^53 as JSON
    numbers, larger ones as `{"t":"int"}`, and integral doubles as
    `{"t":"f"}`, so an int and a double stay apart across the wire.
