## wire-gateway - every language on the network

Stage 7 of specs/polyglot-one-wire.md: before it only Go and Rust served
TCP, so `connect`, `WireAuth` and `WireSecurity` did not reach Python,
TypeScript or Haskell.

- okay-py ships `okay/py/gateway.py` (standard-library Python). It
  listens on TCP, starts the stdio worker per connection, and relays the
  wire. TLS and the HMAC challenge are the gateway's, so the workers
  change not at all, and a CBOR `configure` passes through untouched.
- Scala: `ForeignGateway.start(worker, listen, env)`, `WorkerCommand`,
  `ForeignWorker.pythonCommand`, `TsWorker.command`.
- Tests, live:
  - Python through the gateway runs the whole TLS suite Go and Rust pass,
    with a secret too;
  - TypeScript runs behind a secret (a wrong secret is refused, and each
    connection gets its own worker);
  - Haskell speaks CBOR through it.
  - Mutant: a gateway accepting any mac fails the wrong-secret test.
- The Python shim answers a non-JSON line instead of dying, and ends
  quietly when its host is gone.
- Docs: one-language.md "Every language on the network: the gateway".
