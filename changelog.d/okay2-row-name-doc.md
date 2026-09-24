## okay2-row-name-doc - docs/okay2.md says why the type is called `Row`, with the literature

- Operator: `Row` stays ("короткий и удобный"), and the guide should say
  where the word comes from. Section 2 gains "Why the type is called
  `Row`": row variables (Wand 1987, Rémy 1989), Koka's effect row and
  its tail variable (Leijen 2014, 2017), Links and Frank (Hillerström &
  Lindley 2016; Lindley, McBride & McLaughlin 2017); a handler's `R` is
  that tail, `Pure` the empty row. And the one difference, stated: Scala
  2 has no rows, so okay2 spells one as an intersection of requirements
  (Brachthäuser, Schuster & Ostermann 2020, effects as capabilities),
  while the Scala 3 core spells it as a union and has no `Row` type at
  all — `Row` is okay2's own name. Section 14 lists the five papers.
