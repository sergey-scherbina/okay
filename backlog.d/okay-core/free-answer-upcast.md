- [ ] free-answer-upcast — `Free[F, A]` is invariant in its ANSWER `A`
      as well as in its row, and the answer side has no coercion the
      way the row has `RowLift.coerce`. Met 2026-09-23 in
      `SharedOnce.answer[X](o: Once[X]): X ! Async` (once-across-fibres):
      matching `Force(h)` on a COVARIANT `enum Once[+A]` refines only
      `Option[A'] <: X`, never `=:=`, so `Option[A'] ! Async` is not an
      `X ! Async`; `Once.step` never met it because it answers a
      TUPLE, which is covariant. Today's answer is `.map(x => x)` — a
      `Bind` per `Once` operation on the shared road, an allocation
      bought for the type checker. Every `translate` handler over a
      covariant GADT enum that answers a program will meet the same.
      THE LANE, two roads in order: (1) SPIKE `enum Free[F[+_], +A]`
      — `A` occurs only covariantly in the tree (`Return(a: A)`,
      `Inject(F[A])` under `F[+_]`, `Bind`'s `B`), so the variance
      check may simply pass and the hole close everywhere; the cost is
      an inference change across the core and a full matrix, and the
      row-variance spike (specs/writer-covariance.md, free-row-variance)
      is the precedent for "passes the check, loses on a number" — so
      measure the core lanes before believing it. (2) If (1) is refused
      or slower: `!.up[A, B >: A](p: A ! F): B ! F`, one commented cast
      beside `RowLift.coerce`, sound by the same argument, zero cost;
      then replace `SharedOnce.answer`'s `map(x => x)` with it.
      Operator, 2026-09-23: "в чём опять проблема? вроде уже решали её?"
      — the row half was solved (RowLift); this is the answer half.
