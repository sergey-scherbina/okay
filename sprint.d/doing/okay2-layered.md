- [ ] okay2-layered — the Scala 3 core's `Layered` (specs/
      layered-reflection.md: monadic reflection over a tower of layers,
      `reify` as η $ e) has no okay2 twin. It needs `Delim.dollar`,
      which okay2 has since okay2-dollar (2026-09-25), and the stacked
      `Has.Below` for its typed doors, also there now. Port the core
      layer operations and a TestLayered twin; say in
      specs/scala2-twin.md what does not carry over. Filed by
      okay2-dollar, not built. (2026-09-25)
