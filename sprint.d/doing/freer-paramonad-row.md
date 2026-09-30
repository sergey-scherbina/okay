- [ ] freer-paramonad-row — the operator's follow-up to freer-paramonad
      (2026-09-30): WHERE in the effect system the indexes are used, not
      only in one three-ary signature. Answered by compiling in
      TestFreerPara: a row `[S, R, X] =>> PSt[S, R, X] | At[State[Int,
      *], S, R, X]` — an indexed effect beside a unary one lifted ON THE
      DIAGONAL (`At.Op[F, R, X](e) extends At[F, R, R, X]`, which `Lift`'s
      phantom index cannot say and a handler's loop needs) — and State's
      handler written over it: its own operations answered from the
      threaded Int, PSt forwarded with the index it came with, the result
      run through the indexed natural transformation. Findings to
      specs/freer-base.md. Gate: additive — `okayJVM/testOnly
      okay.TestFreerPara` + `affected master Test/compile`.
