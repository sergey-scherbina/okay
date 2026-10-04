## cont-leaf-forms: answered — the strict leaf is the cheaper one, ContMacro stays

- New lane `HandlerBenchmark.contAnswerStrict`, contAnswer's body as `Cont.shiftLeaf`. Against the
  macro's lazy leaf: 25.1 vs 29.3–29.8 µs, 230 304 vs 390 152 B (0.85x, 0.59x; history.d cont-leaf-forms).
- The macro does not shrink. The lazy leaf is there for stack safety where there is no StackSwitch
  (JS), not for speed (specs/cont-stack.md, Results). Picking the strict leaf on JVM and Native is
  filed as okay-core/cont-leaf-by-platform, to be measured at depth first.
