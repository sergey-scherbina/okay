- [x] handle-decompose — DONE (2026-09-15). `relay` and
      `Effects.handle` were being compared by two numbers that each
      included 24.6 µs of tree construction. `handlePrebuilt` is the
      missing twin of `relayPrebuilt`: same pre-built 10 000-node tree,
      nothing different but the handler. Like for like the gap is
      **1.51x** (223.3 µs against 148.1), and it is ALLOCATION —
      2 869 306 B/op against 1 753 945, identical to the digit across
      two rounds in opposite lane order, which over 9 900 forwarded
      operations is **+112.7 B per forwarded operation**. At ~12 GB/s
      that megabyte is ~93 µs against a 75 µs time delta, so there is
      nothing else to explain. Rows `hd-*`; the stale 1.45x is
      corrected in docs/benchmarks.md §2 and in `relay`'s own comment.
