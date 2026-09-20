- growing-channel-order-under-load — ANSWERED 2026-09-18: the three
  sightings (2026-09-10/11/17, producer 1, one element hoisted exactly
  `Channel(4)`'s capacity ahead) were one mechanism, named by
  `ProbeGrowingOrder` — a producer's elements split across the one-shot
  swap, and the consumer has passed part 0 — and the operator closed it
  as a documented trade (8af62bc7): the default keeps a producer's order
  except once, across its swap; `adaptive`/`fifo` keep it exactly. The
  sightings ledger, the retracted stale-route mechanism and the counts
  live in BUGS.md `growing-stale-route`; the optional buy-back is
  `growing-order-drain-guarantee` (okay core).
