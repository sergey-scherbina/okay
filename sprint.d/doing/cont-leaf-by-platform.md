- [ ] cont-leaf-by-platform — PRIORITY: LOW (2026-10-04, from
      cont-leaf-forms). On a shallow answer-using body the strict leaf is
      0.85x the time and 0.59x the bytes of the macro's lazy leaf
      (contAnswer vs contAnswerStrict, M = 1000). On JVM and Native a deep
      strict body is still safe (rooms, then StackSwitch), so ContMacro
      could pick the strict leaf there and keep the CPS transform for JS
      alone. Measure first at the depths that matter: M = 100 000 and
      1 000 000, where the strict leaf pays the room gauge and the
      switches and the lazy one pays nothing extra. If strict still wins
      there, the choice goes per platform. If it does not, close this as
      answered.
