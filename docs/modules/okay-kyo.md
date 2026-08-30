# okay-kyo

Value bridges plus the STRUCTURAL row mapping — kyo's ArrowEffect
shapes match okay's operation-with-typed-answer arrow for arrow
(`Choice` is `ArrowEffect[Seq, Id]`, literally our `Choose`).

- `fromKyo` (pure eval in), `fromKyoAsync` (their async runs inside
  one okay operation), `toKyo` (okay as a kyo IO suspension).
- Outbound, operation for operation: `toKyoEnv` (Ask → `Env.get`),
  `toKyoEmit` (tell → `Emit.valueWith`), `toKyoAbort` (raise →
  `Abort.fail`, the dead continuation dropped), `toKyoChoice`.
- Inbound through their `ArrowEffect.handleFirst`, whose continuation
  we repack as our operation — multi-shot included: a kyo Choice
  computation explored by our `runChoice` visits every branch.
  `fromKyoEnv` asks once and runs theirs with the constant
  environment (Env is a ContextEffect, not an arrow).

Caveats: kyo needs `Tag`/`Flat`/`SafeClassTag` evidence in places;
watch for `.map` hijacking (ascribe lambda parameters, prefer
flatMap).
