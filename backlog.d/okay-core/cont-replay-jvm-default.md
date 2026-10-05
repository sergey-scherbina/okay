- [ ] cont-replay-jvm-default — PRIORITY: LOW (operator's decision).
      cont-js-depth stage 4 (2026-10-05) made re-execution the strict
      `k`'s answer on Scala.js. The JVM and Native keep the fresh stack
      (StackSwitch) by default, although the operator's bar was
      "полный трамплининг", not a stack switch. The numbers for that call:
      re-execution on the JVM is 4.94x the fresh stack on 1 000 nested
      opaque bodies (~150 ns a level; ShiftBenchmark.cont_strict_seq,
      history.d cont-replay). On Native a throw unwinds at ~55 us a
      frame. And it brings the contract: a strict body's prefix before
      its `k` call may run twice. `-Dokay.cont.replay=true` switches the
      JVM today. TRIGGER: the operator says which.
