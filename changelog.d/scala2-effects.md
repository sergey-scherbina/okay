## scala2-effects - a Scala 2.13 user declares and handles their own effect

The operator asked how a Scala 2 user could declare their own effects.
okay's `derives Effect` is a Scala 3 derivation, but a row split needs
only `TypeableK.test(x: Any): Boolean`, and the facade builds that
test from a `ClassTag`, which scalac 2 supplies.

- okay.scala2 `Op[+A]` (every Scala 2 effect's operations extend it),
  `Effect[F]` (`object Console extends Effect[Console]` is the whole
  declaration; `send`, `handle`, and `run` for the last effect),
  `Handler[F, R, B]` (the operation AND its continuation). A second
  documented cast, `narrow`, sits beside the class test that proves it.
- Probe: `TestOwnEffectFromScala2`. It covers resumptive, multi-shot
  (all four answers of two flips) and aborting handlers, and two user
  effects in one row. Two `compileErrors` checks pin that an
  unhandled effect and `run` over a wider row are both type errors.
  22 tests pass under `-Xlint -Werror`.
- `handle` at the last position inferred `R = Any`, which `-Xlint`
  reports. The prototype blamed the singleton capability; that was
  refuted (the same errors with `Effect[Console]`). `run`, which has no
  `R`, is the answer.
- Docs: a "Your own effect" section in docs/modules/okay-scala2.md
  (copied from the probe; Plotkin & Pretnar cited), and a guide
  sentence. Spec stage 3 boxes are checked and the results recorded.
  scala2-streams and scala2-fibers-channels are queued in the sprint.
