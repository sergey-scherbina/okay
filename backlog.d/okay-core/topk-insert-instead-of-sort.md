- [ ] topk-insert-instead-of-sort — what §9h left on the table. An
      element that DOES make the cut still sorts k+1 through
      `List.sorted` (array copy out, sort, list back), and about
      `k · ln(n/k)` elements make the cut: 57 of 10 000 at k = 8,
      which is the whole 30 KB residual. Inserting into the
      already-sorted list would allocate the prefix and nothing else.
      Small, and only worth doing with the probe in front of you —
      `compare/runMain okay.TopKProbe 10000 8` prints the exact bytes.
