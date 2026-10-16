- [ ] okay-caps-next — what okay-caps measured as the deciding gap: a kernel
      program holding a capability leaves its bracket unseen, okay-core
      being compiled without capture checking. Next, each a lane: (1) a
      probe in okay-core — `Cont` and its closures under the checker,
      what it costs the kernel's types and speed; (2) a handler that
      provides a capability in direct style, the capability unable to
      leave the handler; (3) an agent sandbox in okay-agent, as
      lampepfl/atc does — a model's snippet typed by its row of effects
      and capture-checked, no door to the world but its capabilities.
      (1) PROBED (okay-caps-core, specs/okay-caps.md "The kernel"):
      Cont capture-checked at no run-time cost, map/defer leaks refused,
      but flatMap (inline) cannot carry a capture set without making
      every caller -experimental — wait for @retains to leave
      experimental, then annotate flatMap; the probe is okay-core's
      branch capture-checking (fb69ca6).
      Also open: LazyList does not track its function's captures (a
      standard-library gap, not ours: report it upstream). Until (1)
      lands, the guard (okay-caps-guard) makes those leaks fail loud.
