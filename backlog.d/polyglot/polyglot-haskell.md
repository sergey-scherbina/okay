- [ ] polyglot-haskell — feasible, and half of it is ALREADY DONE:
      Frege is Haskell 2010 on the JVM, and okay-frege runs its programs
      in-process, multi-shot, 0.27 µs a step. What Frege cannot run is
      GHC's ecosystem (GHC extensions, Hackage packages with C bits).
      For that: (1) a GHC SUBPROCESS handler, the okay-py model — a
      small Haskell shim package speaking polyglot-remote-foreign; a
      Haskell program written in a freer monad (the `Prog` shape, or
      freer-simple/polysemy/effectful) runs its effects in okay, and
      because a Haskell continuation is pure, a `Choice` handler can
      resume it twice across the wire. This is the one foreign language
      where the full okay semantics survive a process boundary.
      (2) GHC in-process (`foreign export` + a C shim + the JDK's FFM
      API) is REFUSED unless a measured need appears: two garbage
      collectors and two schedulers in one process, `hs_init` once per
      process, and GHC's RTS signal handlers beside the JVM's — every
      crash in either takes both down, the objection specs/py.md raises
      against JEP.
