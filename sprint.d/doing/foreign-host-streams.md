- [ ] foreign-host-streams — the stream the HOST feeds under the far side's
      credit, and a full-duplex transform (a stream in and a stream out of one
      call), on the multiplexed wire (specs/foreign-one.md, "Streams are
      symmetric"; left by foreign-mux-duplex, Decision 25). The far-driven
      half landed (`Py.stream`, `okay.Emit`/`okay_emit`). Trigger: a far
      function that must PULL a host stream at its own pace (a model reading
      a feed), or a duplex transform that `Stateful` (a far state stepped
      per chunk) cannot express. Gate: a host stream of 100 000 rows read by
      a Go and a Rust function under a credit, counted on the host; a dedup
      both ways at once.
