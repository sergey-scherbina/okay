- [ ] stream-twin-findings — what the machine's stream twin found
      (stream-twin, 2026-10-08; specs/freer-min.md stage 52):
      (1) A PULL, NOT A PUSH. On the machine a handler's clause has no
      capability for the rest of the row, so the classic's shape — a
      transformation as a handler that tells again downstream — does not
      carry over; the machine's stream is a pull (`StreamCont.Src`, an
      Async program answering the next element and the rest), its
      transformations plain functions. The two backends sit behind one
      front, `Streaming[S]` (`okay.streams.machine` / `.classic`), and one
      suite runs on both.
      (2) DONE (stream-chunks, 2026-10-08): the price was per element
      (range collected 2.1x the classic, mapped 1.19x — a Step, a Src, a
      delay and a bind each). A pull now answers a CHUNK of 256, a lazy
      view, `map`/`filter` still per element inside it: range collected
      593 us against the classic per-element Source's 1323 (0.45x),
      mapped 659 against 3842 (0.17x). AGAINST THE FAIR RIVAL
      (stream-fair, 2026-10-08), the classic's own chunked form (`Chunks`,
      256 per chunk, inline `map`/`foldLeft`), the machine is 2.0x: 296
      against 593 collected, 325 against 659 mapped. A STRICT chunk
      (stream-strict, 2026-10-08: `ArraySeq`, as `Chunks`; `map` computes
      its chunk at once, `take` still never pulls the producer past the
      chunk it needs, `fromIterator` still one element a pull): 469
      collected (1.58x `Chunks`), 635 mapped (1.95x). What is left is the
      `map` itself — `Chunks.map` is `inline` and specialised, the
      machine's an `ArraySeq.map` over boxed elements — and the per-chunk
      program nodes. INLINE (stream-inline, 2026-10-08: `map`/`filter`/
      `foldLeft` build their chunk loop once at the call site, the
      classic's `ChunkBuf.mapper`/`filterer`, and recurse on it as a
      value): 450 collected (1.52x `Chunks`), 474 mapped (1.46x) — `map`
      now costs what the classic's does; what is left, ~0.4 us a chunk,
      is the pull itself (a Step, a Src, a delay, a bind, run through the
      machine), the next thing to measure alone.
      (3) `merge` on the pull leaves the pending pulls running when the
      consumer stops early; the classic closes them by a cancel scope —
      the machine's drive has scopes now (async-cancel), not yet used here.
      (4) The machine's front type is an alias of `StreamCont.Src`, so its
      instance is found by the import only; the classic's is opaque and
      found by its companion. Opaque for both would make them symmetric.
