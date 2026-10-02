- [ ] okay2-shift-merge — specs/shift-merge.md's twin in the Scala 2.13 twin
      (operator's pick, 2026-10-02): okay2's `Delim` folded into its `Shift`
      — `Delim` is `Shift[Any]` (the operator's "Shift % Any"), every
      `Delim` door a member of `object Shift`, one machine guard over any
      key, `Shift.dynamic` for static to dynamic; no `Delim` alias. The
      core's stage 3 (`Stacked` as `Shift % p.type`) and its one-evidence
      guard are the sibling lane shift-merge-guard's, ported after they
      land. (2026-10-02)
