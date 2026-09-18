- [x] handle-loop-inlining — REFUTED (2026-09-15), and it is the THIRD
      face of a rule this repository has now paid for three times.
      The diagnosis was right and the fix bought nothing.
      DIAGNOSIS, by -XX:+PrintInlining before touching anything, which
      the entry demanded: `Effects.handle`'s loop compiles to 388 bytes
      against `FreqInlineSize` 325 and is refused SIX times ("hot
      method too big"), while `relay`'s loop is 262 bytes and inlined
      hot four times. So the suspicion was exact.
      THE FIX WORKED AND DID NOTHING. Extracting the terminal case and
      the capturing fallback — the same move `relay.last` exists for —
      brought the loop to 318 bytes and flipped the verdicts to
      "inline (hot)" x3. The lane did not move: three rounds after
      (157.3, 153.0, 153.2 µs) against two before (154.2, 156.2), with
      `relay` drifting the same way in the same rounds, and the RATIO
      measured in-run at 1.039 / 1.030 / 1.011 against 1.029 / 1.039.
      The ranges overlap completely. Reverted; rows `hli-*`.
      THE THREE FACES, so nobody re-derives them one at a time:
      over the line COSTS when the body is straight-line (`relay`, 10%,
      25102517); under the line COSTS when the method is a loop
      inlined into other loops (`Free.resume`, up to 1.44x, 409c06e2);
      and here, under the line is simply NEUTRAL — a loop whose caller
      is a 10-byte wrapper gains nothing by being pasted into it. The
      question to ask is not "is it over the line" but "who is the
      caller, and is the body a loop".
      WHAT THE 3% IS, then, by elimination and by reading the two
      loops: allocation is identical to the digit, inlining is ruled
      out, and `handle` does strictly one more test per handled
      operation than `relay` (`Cont.onAnswer`, on top of the `split`
      they share). That test is what buys `handle` the two things
      `relay` cannot do — abort, and perform G. It is a price, not an
      overhead, and this entry closes.
