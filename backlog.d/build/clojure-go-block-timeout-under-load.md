- [ ] clojure-go-block-timeout-under-load — okay-clojure's
      `TestCoreAsync` "a Clojure go block produces, okay consumes, in
      order, to the end" hit munit's default 30 s timeout (48.8 s) in an
      `affected master` gate on 2026-09-25 (cst-walk-stack-safe) at load
      average 83-129 on 14 cores. That lane's diff does not touch
      okay-clojure; okay-clojure was in the affected set only
      transitively. The suite rerun alone at load 125 was green (7
      tests). What would settle it: whether this test's wall time grows
      with load (Clojure's go-block thread pool against a saturated
      box) or whether it can park forever. A `jcmd` dump taken while it
      is past 30 s tells the two apart. It is the first sighting.
      (2026-09-25)
      SECOND sighting the same day: http-mcp-agent-edge's `affected
      master` gate (34.5 s), okay-clojure again only transitive (a
      build.sbt change); the suite alone right after, at load 131-182,
      green (7 tests). Two red gates from one test on unrelated lanes
      is the shape of a budget, not a bug — a longer timeout on this
      one test is the cheap answer if the dump says it only grows.
      THIRD sighting the same day: the kernel lane's gate (39.1 s),
      okay-clojure again only transitive (build.sbt).
