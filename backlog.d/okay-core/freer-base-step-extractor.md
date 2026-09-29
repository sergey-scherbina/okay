- [ ] freer-base-step-extractor — ONE indexed base for `Free` and
      `Cont`, the dual of cont-on-free: PROBED AND IT COMPILES
      (src/test/scala/ProbeFreerStep.scala, 2026-09-29, bare dotc
      3.9.0 and the repository's gate). specs/freer-base.md stage 1
      was refuted because matching an indexed `Bind` makes the
      intermediate index existential and the pinning extractor it
      tried put the type variable only in `unapply`'s RESULT, which
      dotty infers as `Nothing`. The facade that landed instead keeps
      the tree unindexed and pays two casts in Cont's runner
      (`Shift.at`, `pinned`) plus the `Shift[+X] = (X => Nothing) =>
      Any` spelling. The probe shows the other placement: an indexed
      `Freer[G, A, S, R]` (`Return | Op | Bind | Delay`), ONE
      index-polymorphic `resume`, `Cont = Freer[Shift, A, S, R]` with
      the precise leaf `(X => S) => R` and a runner the GADT types
      end to end (zero casts), `Free = Freer[Lift[F], A, Unit, Unit]`
      with `Lift[F] = [X, S, R] =>> F[X]`, and the erased side's ONE
      cast in `Step` — an extractor whose pattern-bound type
      variables sit in its PARAMETER type (`unapply[F, X, A, T](b:
      Bind[Lift[F], X, A, Unit, T, Unit])`), so the compiler inserts
      the type test that binds them, and whose result is the node
      itself (a Product match: `aload_1; areturn`, no Option, no
      tuple). Casts 2 -> 1, and the one left is a constant claim
      ("a Lift tree is built at Unit") where the facade's two are
      trusted at two nodes. `Prog` becomes the same enum at a third
      signature (`Typed[F] = [X, S, R] =>> (F[X], S => R)`) instead
      of a second facade with its own `transition`.
      WHAT THE COMPILER REFUSES (the probe's comment has each):
      answer types that do not meet in a bind, a continuation of the
      wrong answer type, `Step` on a concrete Cont (E030, proved
      unreachable), `Step` on an abstract-G tree (E092, red under
      "no warnings"). WHAT IT LETS THROUGH: `Step` on the diagonal at
      an abstract R (`Cont[A, R, R]` — the GADT may bind R to Unit),
      so `Step` lives in `object !` and is applied to `Free[F, A]`
      scrutinees only — the standing of today's `@unchecked`, no
      worse.
      THE LANE, if taken: (1) `Freer` replaces `Free`'s enum in
      Free.scala, `Free[F, A]` the alias, `Op` for `Inject` (or keep
      the name `Inject` — the spec's Names decision), `Step` in
      `object !`; (2) the 106 `(x.resume: @unchecked) match` sites
      change `Bind(Inject(e), k)` -> `Step(Inject(e), k)`
      mechanically; (3) Cont.scala loses `Shift.at`, `pinned`, the
      `Shift` alias and the paragraph that justifies it; `Leaf`,
      `Reentry`, `Cps`, the room and the pending stack are unchanged
      — they never depended on the tree's indexes; (4) Prog.scala on
      the third signature, `transition` reviewed against what the
      index now checks; (5) every core lane within the bars — the
      nodes are the same objects, so any movement is a defect. Full
      `affected master staged`; `check-citations` before the merge
      (specs/freer-base.md cites stage 1's shas). NOT a performance
      lane: the prize is a runner the compiler checks and a base that
      stops growing facades.
