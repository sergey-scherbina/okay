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
      (2) THE PRICE PER ELEMENT: range collected 2.1x the classic (2786
      against 1323 us per 100k), mapped 1.19x (4578 against 3842). Each
      element is a `Step.Next`, a `Src`, a `Free.delay` and a bind. The
      lead: a CHUNKED pull (`Step.Chunk(ws, rest)`), as the classic's
      `Chunks` amortise their nodes.
      (3) `merge` on the pull leaves the pending pulls running when the
      consumer stops early; the classic closes them by a cancel scope —
      the machine's drive has scopes now (async-cancel), not yet used here.
      (4) The machine's front type is an alias of `StreamCont.Src`, so its
      instance is found by the import only; the classic's is opaque and
      found by its companion. Opaque for both would make them symmetric.
