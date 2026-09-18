- [x] tag-test-the-signature-too — DONE (2026-09-11), and the feared
      cost was ZERO. `Tag`'s `Effect` given now asks `TypeableK[F]`
      beside the key, so the test is key AND signature — what
      `Instances` does — and `Of["k", Beep] + Of["k", Buzz]` is an
      ordinary row instead of a collision. The source-compatibility
      worry was measured before the change was kept: the whole build
      and every test compile with zero errors, because an `Effect` IS
      a `TypeableK` and every effect that goes under a key already
      has one. The half no runtime test can reach — same signature,
      same key — still misroutes and is still pinned.
