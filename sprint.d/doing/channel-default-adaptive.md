- [ ] **channel-default-adaptive** — does the bounded default channel
      still need to ADOPT a ring? `growing` (the default since
      2026-09-08) is `adaptive` plus the adoption of its first ring as
      part 0, and that adoption is the one-shot swap across which a
      producer's own order may break once. It was chosen when
      `adaptive` was measured with its capacity divided among parts;
      at per-part capacity — which `growing` itself holds since
      growing-part-sizing — the recorded table (docs/queues.md) has
      `adaptive` at 169/166/115 against growing's 183/165/126 and the
      conclusion was never redrawn. spec: specs/channel-default-adaptive.md.
      HOW: A/B through `Channel.apply`'s own switch
      (`-Dokay.channel.buffer=adaptive -Dokay.channel.parts=8`), two
      alternating rounds, own JVMs, control lanes that name their
      mechanism; lanes `ManyProducersBenchmark.default_elem/chunk` (new,
      1/2/4/16 producers) and the actor tell/ask rows. DONE WHEN the
      verdict is in the spec; if matched, the default flips and the
      exact law returns to TestChannelLaws for it, with `Growing`'s
      deletion filed as the follow-up.
