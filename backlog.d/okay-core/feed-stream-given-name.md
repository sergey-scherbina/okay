- [ ] feed-stream-given-name — the pure writer stream instance
      `given [A]: Stream[[W] =>> A ! Writer % W, Pure]` (src/main/scala/
      Writer.scala ~422) is anonymous, while its G-effectful twin is
      `writerStreamIn`; every summon spells the whole type lambda —
      six sites in Chunks.scala and the compare JMH after the `Chunks`
      retype. Name it (`feedStream`, beside `writerStreamIn`) and
      shorten the summons. Cosmetic, one file plus the call sites;
      no behaviour change, gate as usual.
