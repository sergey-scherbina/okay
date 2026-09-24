## wire-tls - encryption on the wire: given WireSecurity

Stage 5b, part two, of specs/polyglot-one-wire.md: "шифрованием ... через
имплисииты (и соответствующие внешние механизмы - у каждого имплисита свой)".

- okay-codec `WireSecurity`: `WireSecurity.tls(trust)`, where the trust
  names its own source (`Trust.pem(path)`, `Trust.pemFromEnv(name)`,
  `Trust.system`). The server's name is verified, not only its chain. The
  default is plain. `ForeignWorker.connect` takes it as `using`.
- Go `ServeTCP` and Rust `serve_tcp` serve TLS with `OKAY_TLS_CERT` and
  `OKAY_TLS_KEY`. Rust's is the okay crate's `tls` feature (rustls on ring,
  offline), built with `RustWorker.build(dir, features = Seq("tls"))`.
  Without the feature, a worker asked for TLS refuses to start and names
  the switch.
- Mismatches are refused by name, including a plain host meeting a TLS
  server, which hears no hello and is told why.
- Tests: `TlsConformance` on Go and Rust (the conformance suite over TLS,
  name check, stranger trust, wrong name, both mismatches, TLS with
  `WireAuth`) and `TestRustTlsFeature`. Mutant: no name check lets a
  wrong-name certificate through, and that test failed.
- Docs: one-language.md "Encryption: WireSecurity" (RFC 8446, RFC 6125),
  go.md, Limits.
- Board: `foreign-in-durable-workflow` queued from the operator's request
  to bring all of this into Durable, workflows, static Proc and
  do-notation.
