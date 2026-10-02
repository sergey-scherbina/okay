## cont-macro-while - a while loop with k in it is read by the Cont macro, each iteration trampolined

cont-stack-layer1-c item (5) in part (the operator: "Продолжай
исправлять", "keep fixing").

- **What is read.** `while c do body` with `k` in the condition or the
  body becomes a local loop function. Each iteration is `Cont.later`, a
  step the machine forces in its own loop.
- **Iterations that never call `k`** hold no host frame either.
- **When the condition fails**, the rest after the loop runs, once.
- **Depth.** A million bodies with such a loop, and a million
  iterations calling `k` only in the first, both on a 128 KB thread with
  zero switches. Both failed with StackOverflowError first.
- **Meaning is kept:** the order of side effects, a condition that calls
  `k`, and multi-shot.
- **No library or benchmark body changes form.**
- **`try` around `k` stays opaque on purpose.** With a lazy `k`, the
  rest of the program would run outside the `try`.

Tests: TestContMacro. Docs: docs/cont-stack.md.
