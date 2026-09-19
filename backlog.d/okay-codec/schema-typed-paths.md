- [ ] schema-typed-paths — `Schema.path[A].field("address").field("city")`
      → a checked `Lens[A, String]`: the field NAME verified at compile
      time against the Mirror, the focus typed. The operator's third
      ask in the schema-fold conversation (2026-09-11); `JsonOptic.path`
      is most of it over `Json`, over `A` it is a macro over the
      Mirror's labels. Filed by specs/schema-fold.md (Out of scope),
      to be taken after stage 3; a different risk (macro) from the fold.
      STALE BY TWO DAYS when written (found 2026-09-18): one level of
      this exists — `Lens.field[S]("name")`, by name, the name checked
      against the Mirror at compile time, the focus typed, no macro —
      since optics-core on 2026-09-09 (c2ff5cfe). What is still asked
      for is the CHAIN with the intermediate type inferred, and its
      cost question is `optics-field-fuse` (the planner cannot read
      the by-name constructor, so it pays the interpreter today).
