- [ ] cont-stack-okay2-macro — the open half of cont-stack-okay2 (landed
      2026-09-25 with Layers 2 and 3). LAYER 1 A DONE 2026-10-04
      (changelog.d cont-stack-okay2-macro): `Cont.shift` is a blackbox
      macro; a tail body is `tailShift`/`tailPure`, a million on 128 KB
      with zero switches; the core's own callers write `shiftLeaf`. LAYER
      1 B DONE the same day: answer-using bodies CPS-transformed onto an
      explicit pending stack on okay2's own runner (the Scala 3 core's
      cont-stack-layer1-b road, not a frame-machine port), a million on
      128 KB, contAnswer 1.03-1.04x. LEFT: only the JDK 22+ FFM reader
      below. Was: Layer 1 A in Scala 2 — `shift`
      as a blackbox def macro that reads the lambda literal and rewrites
      a body whose every use of `k` is a tail call `k(v)`, `v` free of
      `k` (through blocks, `if`, `match`), to `Cont.tailShift(() => v)`
      / `tailPure(v)`, as the Scala 3 `ContMacro` does; every other body
      is the leaf as today. Proof: 1M tail shifts on a 128 KB thread
      with `StackSwitch.switches` unchanged. Also open: the JDK 22+
      FFM reader as okay2's own Multi-Release variant (`versioned` in
      okay2/build.sbt, `-release 22` for scalac 2.13) — until it lands
      okay2 counts on every JDK, first room ~218 levels on a 2 MB thread
      (its cold level is ~4 KB: 11 frames), one pooled switch (~4 µs)
      past them.
