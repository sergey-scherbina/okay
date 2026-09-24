## wire-links - the foreign engine over any link; Go over TCP

Stage 1 of specs/polyglot-one-wire.md.

- `WireLink` separates okay's line protocol from its transport.
  `PySubprocess.over(link)` is the engine (typed calls, callbacks,
  programs as data, multi-shot, `Durable`). The links are pipes (what
  `speaking` has always been) and TCP (`PySubprocess.connect(host, port)`,
  another process or machine).
- Go: `Worker.Handle(line)` is the protocol without I/O. `Serve` (stdio)
  and `ServeTCP` (a worker per connection, printing where it listens)
  both use it, and `okay.Main` picks TCP when `OKAY_LISTEN` is set.
- `WireConformance` is one Scala test body that passes over (Go, pipes)
  and (Go, TCP). A mutant is caught on both.
- The spec records the operator's direct style, `okay_call(request) ->
  answer`, for Rust and Go on every transport: the wire's
  ask/resume dialogue, an FFM upcall, and a Wasm host import.

Docs: "From Scala, over a pipe or a socket" in docs/go.md.
