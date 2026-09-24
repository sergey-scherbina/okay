- [ ] scala2-alias-param-inference — the `+`/`!` aliases of stages 16-17
      break scalac 2's inference in a PARAMETER type with an abstract
      row: `def f[R, A](p: A ! (State[Int] + R))` cannot be called in
      any position (measured 2026-09-24, 0 of 3), while
      `p: Eff[State[Int] with R, A]` infers R in all three; result types
      and concrete rows are unaffected. Lane: pin both in
      TestRowAliasFromScala2, say it in docs/scala2.md section 3 and the
      spec, changelog. (2026-09-24)
