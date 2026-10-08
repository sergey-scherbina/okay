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
      mapped 659 against 3842 (0.17x). The fair rival is the classic's
      own chunked form (`Chunks`, `.chunked`), not yet measured beside it.
      (3) `merge` on the pull leaves the pending pulls running when the
      consumer stops early; the classic closes them by a cancel scope —
      the machine's drive has scopes now (async-cancel), not yet used here.
      (4) The machine's front type is an alias of `StreamCont.Src`, so its
      instance is found by the import only; the classic's is opaque and
      found by its companion. Opaque for both would make them symmetric.
