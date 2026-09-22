## typeablek-instanceof - a derived signature's test is a constant-class instanceof

`derives Effect`/`derives TypeableK` built `Effect.ByClass(ct.runtimeClass)`:
a test that reads its class from a FIELD and calls `Class.isInstance`,
where the hand-written `case e: E1[?]` compiles to a constant-class
`instanceof`. That test is under every `split` in the library. Now
`TypeableK.derivedImpl` reads the signature's erasure off the type
(every argument a wildcard) and emits, per `derives` site — one per
signature — a class of its own whose `test` is `x.isInstanceOf[F[?, …]]`;
the `ClassTag` parameter is gone from `derived`/`derivedEffect`
(nothing needed it once the macro reads the type). `ByClass` stays for
`typeableK(cls)`, whose class is a run-time value. The union refusal is
unchanged. Law (`TestDerivedInstanceof`): a derived instance is not a
`ByClass`, `typeableK(cls)` still is, and the two agree on own, foreign
and `%`-shaped operations.
Measured (history `tki-*`, per-arm minima over five alternated rounds
on a box that never went fully quiet, loads 3–44; bytes identical on
every lane, the `fusedSWr` floor 122 624 unmoved): `nestedSWr`
15.98 → 13.25 µs (0.83), `relayForward` 175.1 → 161.4 (0.92),
`fusedSWr` 12.99 → 11.99 (0.92), `inline4` 103.1 → 103.6 (1.00). The
lane the residual was named on did not move — the flat macro's chain
of tests was one devirtualised call already; what moved is every
walker whose test runs under `split` per operation. TKI_CLEAN_ROUND
