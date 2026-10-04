- [ ] union-bind-flatmap — specs/foreign-effects-in-tree.md stage 3: a
      `for` over different effects with no widen, by a bind whose result
      row is written `F + G` (it exists as a method, `Row.bind`, since
      bind-in-row-union, 2026-09-23). The work of this lane continues as
      freer-two-part-row (specs/freer-two-part-row.md on its branch). As `flatMap` two roads are refuted (extension: lexical
      givens' `flatMap` win, self-recursion; a `Join` given: dotty's
      row-membership-crash). Next: road (a), a member overload for a
      `Free` receiver by evidence; else (b), the foreign programs'
      own wrapper type.
