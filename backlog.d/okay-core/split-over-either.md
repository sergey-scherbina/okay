- [x] split-over-either — LANDED 2026-09-15 as mostly a refutation: the Either
      was scalar-replaced on every walker but the Source/Pipe road (bytes
      −2..−6% there, identical to the byte everywhere else, time neutral);
      all fifteen converted for one idiom, Resource keeps `<|>` (its arms
      `return`). specs/core-cleanup.md. The entry as written: sixteen walkers outside the three core files
      still split their row with `<|>`, an `Either` per operation:
      Pipe (10 sites), Writer (5), Source, Resource, Logic, Refs,
      Generate, Condition, Channel, Delim. `split` replaced it in
      State and Writer.tell (split-without-either) and in `translate`
      (core-cleanup). Each of these is a `@tailrec` loop, so every
      conversion is an inlining-budget question (`relay`'s 325-byte
      cliff): one walker per lane, `-XX:+PrintInlining` before and
      after, the lane's own benchmark or none. NOTE (defer-eff-removal,
      2026-09-15): `Free.resume` is 323 bytes now and inlines into every
      loop under 325 — a walker converted to `split` may also cross
      that line; `relay` gained 6% from the paste, `handle` lost 15%
      until its cold arms were extracted. Read that entry first.
