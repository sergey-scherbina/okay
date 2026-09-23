- [ ] handlers-bench-suite — PRIORITY: MEDIUM. no standard effect-handler benchmark is
      in `compare/`: Kammar, Lindley & Oury, "Handlers in action"
      (ICFP 2013) and its descendant, the effect-handlers-bench suite
      the Koka, Effekt, OCaml 5 and Lexa papers report on — countdown,
      fibonacci_recursive, product_early, iterator, nqueens, generator,
      tree_explore, triples, parsing_dollars, resume_nontail,
      handler_sieve. We price against kyo/cats/ZIO on our own lanes;
      these give comparability with PUBLISHED numbers and cover the
      shapes ours do not (non-tail resumption, sieve of handlers,
      deep resume). THE LANE: the suite under docs/benchmarks.md's lane
      rules (one shape per lane, the hand-written loop beside each,
      per-lane gated JMH), on the shipping runners and under
      `Direct.staged` where a block applies; a table against the
      published Koka/OCaml 5 figures with the machine named. The
      OCaml 5 harness's source is the reference for each shape, read
      before porting (the fs2 lesson: the competitor's SOURCE decides
      what a lane measures). (2026-09-23)
