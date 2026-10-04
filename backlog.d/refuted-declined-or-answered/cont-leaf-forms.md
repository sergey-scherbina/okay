- cont-leaf-forms — answered 2026-10-04: the macro does NOT shrink, and the
  premise was backwards. The strict leaf is the cheaper one: contAnswer's
  body `k(x + 1) + 1` (M = 1000) as `Cont.shiftLeaf` runs 25.1 µs and
  230 304 B against the macro's lazy leaf at 29.3–29.8 µs and 390 152 B
  (0.85x time, 0.59x bytes, two rounds, history.d cont-leaf-forms;
  lane `HandlerBenchmark.contAnswerStrict`). The four forms are not
  alternatives by cost: `Strict` is the only meaning of an opaque body,
  `Lazily` is what keeps an answer-using body off the host stack where
  there is no StackSwitch to fall back on (JS, specs/cont-stack.md
  Platforms), `Resume` is a lazy `k`'s call and `Program` a body that
  answers a program. What the number does open is filed as
  okay-core/cont-leaf-by-platform.
