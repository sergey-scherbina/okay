- [ ] gate-affected-short-form-in-chain — `scripts/gate.sh "affected master
      staged; okayDeploy/testOnly X"` does not expand the short form: the
      expansion (gate.sh, "ONLY for the `affected <ref>` form") matches the
      WHOLE command string, so inside a `;` chain `affected master staged`
      reaches sbt raw and is refused ("Not a valid key: staged"). Loud, so
      it costs a run, not a verdict (foreign-one-r, 2026-09-26). Fix: split
      the chain first, then expand each `affected` element. Two related
      notes from the same lane: a `build.sbt` edit of ONE `dependsOn` line
      widens `affected` to all 187 projects (the lane scoped itself with
      `--files=` minus build.sbt and said so), and `--plan` over
      okay-foreign-cluster's own file did not list okayForeignCluster as
      changed — ANSWERED by foreign-facade-close (2026-09-26): the mapping
      was right, the project was missing from the root `.aggregate` since
      it was created (25199ebaf), so `affected` filtered it out as not
      gated and `family all` never ran it; it is aggregated now.
