- [ ] adaptive-elementwise-small-ring — the ELEMENTWISE merge on the
      `adaptive` default at a small ring: `MergeCapBenchmark` cap 64 reads
      90.9 against Loom's 80.7 (1.13x, 2 alternating rounds -f 3,
      2026-09-29); parity at cap 256/1024. Not the monitor tick
      adaptive-chunked-merge-cost fixed: spreading the two feeds at once
      (`forkLong`) made it WORSE, 1.25x — a 64-slot ring blocks both feeds
      over and over, and each resume is a callback drive's re-entry where
      Loom unparks a virtual thread. FIRST: a CPU and wall profile of both
      arms at cap 64, and count the Await registrations per op (how often
      a feed parks on a full ring) before guessing. PRIORITY: LOW — cap 64
      is `merge`'s default capacity, but 10 us an op. Rows:
      src/jmh/history.d/2026-09-29T141148Z-adaptive-chunked-merge-cost.tsv.
      (2026-09-29)
