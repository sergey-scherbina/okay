## core-maybe-supply - Maybe, Either→Throws, Supply in the core; the rest of the core review filed

- `Maybe` (src/main/scala/Maybe.scala) is an effect of its own, lifted from
  an `Option`. `Some(x).maybe` answers `x`, `None.maybe` stops the program,
  and `Maybe.run` answers the `Option`. There are also `Maybe.none`,
  `orElse` and `getOrElse`. It has its own class, not `Throws % Unit`, so
  `Maybe + Throws % E` is a good row. `Abort + Throws % E` is refused by
  `Distinct`, and TestMaybe now pins that. A refuted pattern in a `Maybe`
  row stops through `Maybe` (`CanFail`: Choose > Maybe > Abort).
- `either.orRaise`: the `Right` is the answer and the `Left` is raised
  into `Throws % E`.
- `Supply[S]` (src/main/scala/Supply.scala) has one operation,
  `Supply.next`. `Supply.run(first)(step)` threads the seed like `State`,
  so the program can be re-run and each `Choose` branch keeps its own seed.
  `Fresh` is `Supply % Long` with `Fresh.next`/`Fresh.run`. It is not a
  top-level `fresh`, because DI owns that name.
- TestMaybe, TestOrRaise, TestSupply: 12 tests, including stack safety at
  100 000 binds.
- Seven findings of the review are filed in backlog.d/okay-core:
  handler-single-pass, resume-inline-budget-guard, tag-rename-pass-cost,
  bracket-forwards-no-effects, writer-listen-censor,
  error-accumulation-effect, random-clock-signatures.
- Spec specs/core-gaps.md; docs/guide.md, "Absence — Maybe" and
  "Fresh values — Supply".
