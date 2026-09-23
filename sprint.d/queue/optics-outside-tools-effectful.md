- [ ] optics-outside-tools-effectful — TRIGGER LIFTED by the operator (2026-09-23: "сделай без триггера. Это полезная штука, пусть будет … Все это нужно"); the original trigger stays below as the record of what was waited for. — `Toolbox` handlers are
      `A => String`, because that is the seam `Mcp.Server`,
      `Handlers.tools` and `Stepper` already take. A tool that must do
      I/O has to close over its own runner today. Widening to
      `A => String ! Rest` is a separate decision with those three
      callers to carry; see the spec's Out of scope.
      RE-VERIFIED 2026-09-11: `Persist.append(partition, key, value,
      ack): Long` returns a value directly — okay-persist is
      synchronous by design, so nothing in the tree gives a tool a
      reason to suspend. TRIGGER: the first tool that must do I/O its
      caller cannot do for it.
