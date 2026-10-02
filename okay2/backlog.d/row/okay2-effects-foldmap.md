- [ ] okay2-effects-foldmap — the last member of okay's `trait Effects`
      okay2 does not have (okay2-level1-api brought the rest, 2026-10-02):
      `foldMap[G](nt)(implicit Monad[G], TailRecM[G])`, the program folded
      into any monad through the carrier's own loop
      (specs/eager-carrier-depth.md). okay2 has no `TailRecM` yet; it
      comes first, with an instance per carrier, and the 1 000-operation
      overflow test the core's first cut failed. (2026-10-02)
