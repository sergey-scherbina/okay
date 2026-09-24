- [ ] bang-row-no-parens — in Scala 3 `!` sits at the `= !` level, `+`
      and `%` above it (Effects.scala:87), so `A ! (State % S + Writer % W)`
      needs no parentheses and direct-style.md already writes it bare;
      every other page and test parenthesises. Lane: strip the parens
      wherever the row is a plain `%`/`+` chain (sources, tests, docs;
      NOT the Scala 2 side, where every infix type has one precedence
      and the parens are required), pin the precedence with a `=:=`
      test, make the Scala 3 / Scala 2 comparison tables show both
      spellings, gate the family. (2026-09-24)
