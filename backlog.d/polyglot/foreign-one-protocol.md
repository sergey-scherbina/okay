- [ ] foreign-one-protocol — stage 2 of specs/foreign-one.md: the wire's
      eleven operations (`call`/`frame`/`start`/`resume`/`hold`/`method`/
      `attr`/`program`/`continue`/`forget`/`release`) become FIVE (`call`
      with `held` and a polymorphic address — fn, a held object's method
      or attribute; `program`/`continue`/`forget`; `release`), plus the
      stream channel. A message is a HEAD and PARTS: a `Table` argument or
      answer is an Arrow IPC part named from the tree (`{"t":"table",
      "part":i}`), so a call carries any number of tables (py-arrow's
      one-stream-with-metadata carried one); columnar JSON in the tree
      where Arrow is not spoken. `perform` is the ONE callback message —
      direct style (a parked thread) and programs as data send the same
      line, and `programs: one-shot|multi-shot` in the hello says whether
      a second `continue` is refused. Host side: `Foreign[L, +A]` (5 ops),
      one `Value` tree, one `Refused`, one `Node`. Shim 7 in every shim
      (py, r after foreign-one-r, ts, hs, go, rust); the protocol written
      as `specs/foreign-wire.txt`, a golden transcript replayed against a
      fake far side (default gate) and every shim (live). Gate: every
      existing live suite green with only constructors renamed; a call
      with a table AND a ref AND a value crossing as one head + one part.
      R JOINS HERE (foreign-one-r, 2026-09-26, spec Decision 12): `RValue`
      becomes the one `Value` (a typed `na` escape; `I32` = Int at
      `Shape[R]`; `Named` = `Dict`), `RCodec`/R's `Wire`/`REval` fold into
      the one codec and effect, and R runs `WireConformance` and
      `CrashConformance` as a row.
