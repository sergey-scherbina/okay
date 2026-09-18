- [ ] optics-field-fuse — `Lens.field[S]("name")` is the one
      constructor the planner cannot read (verdict, 2026-09-10), so it
      pays the interpreter while `Lens[S](_.f)` is free. Measure it
      first (it has no benchmark row), then either teach `Fuse` the
      `FieldOf.apply` shape or say the price on the guide page.
