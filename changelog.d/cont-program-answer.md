## cont-program-answer - a Cont body that calls k and answers a program gets a lazy k: no nested run, on any platform

The operator's ask on way 1 of the strict-k bridge ("можешь решить эту
проблему?"): make the rest of such a body a frame of the machine without
handle-on-machine's 1.74x.

- **The new leaf.** `ContMacro` emits `Cont.programLeaf` for an opaque
  body whose answer `S` is a program and which calls `k` itself, outside
  any lambda. Its `k(a)` returns at once: `Delay(M.ownedFlat(k(a)))`.
- **Who runs the rest:**
  - any interpreter forces it, in one bounded run whose answer is the
    program that goes on;
  - a running machine steps into it, and continues into that answer in
    its own loop (`Own`'s new `flat`).
- **Why it isn't the 1.74x.** Nothing is translated, since `Free` is
  the machine's own tree at a narrower row, and no operation leaves the
  machine and re-enters it.
- **Depth.** A million nested such bodies, forced or stepped into,
  pass on a 128 KB JVM thread, on Scala.js and on Native. The strict
  leaf on the same program fails: StackOverflowError on 128 KB, and
  "Maximum call stack size exceeded" on Scala.js. Both were watched
  first.
- **What changes form.** One site in the whole tree:
  TestHandleForward's multi-shot handler, with its answers and the
  order of forwarded operations unchanged. Every library body passes
  `k` on, or calls it inside a lambda, and keeps its leaf.
- **Price.** `ShiftBenchmark.shift0_twoShot` (the road `Own` is on)
  shows identical bytes and time within noise.
- **Contract.** Host side effects after `k(a)` in such a body now run
  before `k`'s rest (docs/cont-stack.md).

Tests: TestContProgramAnswer (cross), TestContProgramAnswerSmallStack.
Specs: cont-js-depth.md stage 3b. History: history.d
`cont-program-answer`.
