- cont-program-leaf-always — ANSWERED 2026-10-04, nothing to change: its
  premise was wrong. A strict `k` under a body that answers a PROGRAM
  holds no host stack. `k(x)` runs the rest only to the next capture,
  whose body answers its program unrun, so the call returns. `runChoice`
  and `Prob.runExact` over 100 000 choice points make 0 stack switches
  under the tests' room of 64 (a nested run per level would make over
  1 500). TestProgramAnswerStackFree pins both, with a control: an
  answer-using body over a VALUE does switch. The survey that filed this
  counted shifts a user controls, not frames on the stack. What still
  needs the stack is road 3 (specs/cont-stack.md): an opaque body that
  needs `k`'s finished VALUE.
