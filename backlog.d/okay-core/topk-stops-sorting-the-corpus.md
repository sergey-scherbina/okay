- [x] topk-stops-sorting-the-corpus — DONE (2026-09-10):
      `Aggregator.topK` consed and re-sorted for EVERY element, and
      `MemoryStore.search` folds a whole corpus through it. Guarded on
      the k-th kept element: 5 358 168 B to select 8 of 10 000 records
      became 30 328 (docs/benchmarks.md §9h), and the store's search
      lane 351 713 B/op and 2542 us became 49 943 and 1036.
