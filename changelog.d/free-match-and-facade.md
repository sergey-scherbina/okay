## free-match-and-facade — a match on a `Free` operation refines again; Scala 2's screen in the facade

After one-bridge (operator: "а раньше как это происходило? Как исправить? Для
scala 2 сделай в фасаде экранирование матч типа").

- `Free.Inject.unapply` answers the operation at its own type, `F[A]` (a
  name-based extractor over a value class, `Free.Op`; nothing allocated). The
  node's field is typed by the bridge, a match type, which a nested pattern
  does not reduce — with `Lift` it was `F[X]` itself. `case Inject(Writer.Say(c))`
  refines again; TestSqlPure is back to its own words.
- okay-ui's `Nav` is as it was (`Run(prog, s)`). The screen for Scala 2.13 is
  the facade's: `NavCase.of(nav)` is `Nav` to match on, its `Run` holding the
  program in `NavProgram` (specs/scala2-facade.md, docs/scala2.md 8i).
