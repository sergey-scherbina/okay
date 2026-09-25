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
