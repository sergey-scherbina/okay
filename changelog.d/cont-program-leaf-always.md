## cont-program-leaf-always: answered — a body that answers a program holds no host stack

- `TestProgramAnswerStackFree` (new): `runChoice` and `Prob.runExact` over 100 000 choice points switch the
  stack 0 times under a room of 64. A control proves the counter: an answer-using body over a value
  switches. The strict `k` returns at the next capture, so library handlers answering programs were never
  on the host stack. Nothing to change. specs/cont-stack.md, road 2.
