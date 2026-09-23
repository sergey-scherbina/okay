## comonad-id-map-capture - a bare value no longer has a `.map`

`given Comonad[Id]` was declared at package level in Monad.scala, and
through `Functor`'s `extension map` it put a `.map` on EVERY type in
package `okay` and in every file with `import okay.given`. A lexical
extension beats one found in a receiver's companion, so it won against
facades (`Static` was made a class for it, `Par(p).map(f)` typed as the
identity comonad's and returned an `Id`), against `String`'s own `map`
(TestStructured went through `.toList`) and against kyo's in benchmarks.
Four sightings, one comment calling it "a known footgun".

- Monad.scala: the instance moves into `object Comonad` as
  `Comonad.id`. A companion is in the implicit scope of its own type,
  so `summon[Comonad[Id]]` and the `Handler[Id]` derived from it
  resolve unchanged; a bare value's lexical scope no longer holds it.
  The whole family's `Test/compile` was green with no other change —
  nothing depended on the universal `.map`.
- `TestIdInstance` (4): `5.map(_ + 1)` refused in package `okay` and
  under `import okay.given`, while `String.map` is String's; the
  instance and its handler are still summoned. Watched FAIL first.
- TestPar's pin, written "so the day the footgun is fixed this line
  says so", flipped: `Par(p).map(f)` is now asserted to be Par's own
  and to run. TestOpticCarriers writes `Static.op(x).map(f)` instead
  of `fmap` through the instance; TestStructured drops its comment.
  Comments in Par, Static, Optic and Throws say what is true now.
- WHAT DID NOT GO: `import okay.Prog.{flatMap, map}`. The backlog item
  expected it to become unnecessary. Dropped, TestProg fails with
  "value + is not a member of A" and TestTx with `Required:
  Prog.Rep[Async, B, R, T]` — `Prog`'s own `flatMap` resolves both
  times, but reached through the companion it does not infer the
  continuation's type. The capture had been masking a second cause.
  Both imports stay with the real reason beside them; docs/guide.md
  and specs/freer-base.md (a REVISED note) now say so.
- Docs: guide, tutorial (two cautions removed), theory ch. 12 (the
  story told to its end), typepedia (three entries), okay-kyo's gotcha
  removed.
