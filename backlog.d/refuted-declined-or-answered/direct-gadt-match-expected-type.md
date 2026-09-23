- direct-gadt-match-expected-type — DONE 2026-09-23, REFUTED AS FILED:
  the macro cannot ascribe a tail `match` — an inline method's
  arguments are typed BEFORE the macro runs, and the arms have already
  failed. `apply`'s signature cannot carry it either: a leading
  `using Pin[A]` (contravariant) and a prefix object typed after `A`
  both fail identically. The rule is dotty 3.9's and needs no macro:
  a GADT `match` whose expected type is a type variable bounded only
  above (what a covariant result leaves) is fixed by its first arm,
  down to the singleton (`Required: (1L : Long)`); an invariant result
  types every arm at `X`. `(e match …): X` stays the spelling.
  TestDirectGadtTail pins both halves, macro-free, so a Scala release
  that changes the rule turns them red. Trigger to reopen: that red.
