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
      now costs what the classic's does. MEASURED, NOT GUESSED
      (stream-range, 2026-10-08, -prof gc): the "per-chunk" rest was 25 B
      an ELEMENT — `ArraySeq.range`, generic over `Integral`, boxed every
      element it built (6.25 MB/op against `Chunks`' 3.74). A long[] filled
      by a loop, as `Chunks.range` does: 227 us collected (0.77x
      `Chunks`), 256 mapped (0.79x), 3.79 MB/op. The pull's own nodes per
      chunk were never the cost. (2) is closed: the machine's chunked
      pull is FASTER than the classic's chunked form on these lanes.
      (3) DONE (stream-merge-scope, 2026-10-09): `merge` opens a cancel
      scope over its pending pulls and exits it when both sides end; a
      consumer that stops first ends with it open and the drive cancels
      them (tested: `take(2)` over a side that never answers).
      (4) DONE (stream-merge-scope): the machine's `Flow` is opaque too;
      both instances are found where their type is, no import needed.
      (5) CHANNELS (stream-channels, 2026-10-09): the classic `Channel` is
      backend-neutral at its callbacks, so the machine needed only words:
      `StreamCont.send`/`receive` (an Await over `sendAsync`/
      `receiveAsync`, cancelled by `cancelSend`/`cancelReceive`),
      `fromChannel` (a chunk per `receiveManyAsync`), `Src.toChannel`, and
      `buffer` (a pump fiber under a cancel scope, as `merge`), in the
      front for both backends. Not yet: `fromChannel`'s pending receive
      has no canceller (`receiveManyAsync` offers none), and the
      buffered pipeline is unmeasured beside the classic's.
      (6) ZIP, JOINS, WINDOWS, WRITTEN ONCE (stream-joins, 2026-10-09):
      the front gained primitives — `unconsChunk`, an effectful `unfoldP`,
      `pureP`/`flatMapP` — and `zip`, `zipWith`, `joinSorted`/`left`/
      `full` and `windowed` are written once over them (`StreamingOps`),
      driving the backend-neutral engines `SortMerge` and `Windows` with a
      chunk cursor; one suite runs them on both backends, JVM/JS/Native.
      The classic's own `Source.zip`/`joinSorted` need `CanBlock` (they
      buffer through channels); these pull, and run on JS too. Not yet:
      measured beside the classic's; a backend may override a word for
      speed (the machine's `zip` could pair chunks).

