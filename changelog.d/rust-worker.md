## rust-worker - Rust code that performs okay's effects

Stage 2 of specs/polyglot-one-wire.md.

- The jar ships a Rust crate, `okay`, with two styles:
  - direct style: `ctx.call(name, args)` and `ctx.call_op(op)`, the
    operator's `okay_call(request) -> answer`;
  - programs as data: `perform`/`and_then` and a typed
    `send(op).and_then(...)`, whose continuations can be resumed more
    than once.
- `okay::main` serves on stdin/stdout, or on TCP with `OKAY_LISTEN`.
- `RustWorker.build(dir)` compiles a crate against it offline.
- `Rs.ops(callbacks)` writes typed operation constructors from the Scala
  callbacks' Schemas.
- The Scala conformance suite that checks Go passes over Rust pipes and
  Rust TCP as well, and a mutant is caught.

Docs: "Rust code that performs okay's effects" in docs/rust.md.
