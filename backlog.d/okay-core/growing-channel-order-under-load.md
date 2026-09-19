- [ ] growing-channel-order-under-load — SEEN ONCE, NOT REPRODUCED,
      and recorded because the alternative is forgetting it. A full
      matrix on 2026-09-10 failed `okay.TestGrowing`'s "each
      producer's own order survives the swap"
      (src/test/scala-jvm/TestGrowing.scala:186) at round 34 of 200:
      producer 1's subsequence came back 49, **57**, 51, 55, 59 — its
      own FIFO order broken across a part swap, on the SHIPPED
      `Channel(4)` the test deliberately uses. The box was at load 45
      (an operator VM holding ~11 of 14 cores) and two full matrices
      on the same tree an hour earlier were green, as were 13
      subsequent runs of the suite alone — 2 600 rounds at load 15–29,
      no failure. So it is one of: a real race in the adoption/swap
      path that needs contention to show, or a promise the growing
      channel does not actually make under it. NOT tagged and NOT
      retried: both would hide a real defect, and the suite is the
      only place this guarantee is stated. Reproduce with load, not
      with repetition — that is what distinguished the two runs. The
      gate log is okay-gate.ySVIJbPIg1 (the diff is in it).
      **SEEN A SECOND TIME, 2026-09-11 13:12** (route-headers gate, a
      lane that touches only okay-http and okay-openapi), and the
      SIGNATURE MATCHES, which is what makes this more than a flake:

          round 27: producer 1 came back out of its own order
             5, +13, 7, 11, -13, 15

      Both occurrences are PRODUCER 1, and both are ONE element
      hoisted forward past its own predecessors across a part swap —
      2026-09-10 was 49, **57**, 51, 55, 59 at round 34. Two
      independent trees, two days, the same shape and the same
      producer index. That is the first of the two hypotheses above
      (a real race in the adoption/swap path that needs contention to
      show), not the second.
      Load at the failure: `{ 11.75 20.87 24.93 }` — a box coming down
      off a long busy stretch, which again is contention rather than
      repetition. The run was afterwards SIGTERM-killed at 3089 test
      results; `gate.sh` reported RED rather than KILLED, correctly,
      because a suite that failed and was then killed is red (the
      ordering that branch was given on 2026-09-11 exists for exactly
      this case).
      Still NOT tagged and NOT retried into green by the gate. The
      lane that met it re-ran its matrix after filing this, which is a
      person deciding on an unrelated module — not the script hiding a
      defect. The gate log is okay-gate.dHgXx2UyXr.
      **SEEN AGAIN 2026-09-17 — AND THIS ONE IS AFTER THE FIX, which
      the first framing of this note got wrong.** The two occurrences
      above are 2026-09-10 and 2026-09-11; `growing-stale-route`
      landed 2026-09-14 (3ba1d825) and closed the mechanism they
      share. Calling this one "the third of the same" reads as three
      sightings of one open bug. It is not: it is the FIRST
      RECURRENCE AFTER A FIX, which is a different and worse fact.
      See BUGS.md, where the entry is reopened with the analysis.
      (delim-diagnostics-position, a lane touching only okay-persist
      and prose), round 36:

          round 36: producer 1 came back out of its own order
             29, +37, 31, 35, -37, 39

      PRODUCER 1 again, one element hoisted forward again — and the
      third occurrence is what makes the three MEASURABLE rather than
      merely similar. Each producer emits an arithmetic sequence, so
      the hoist can be counted:

          2026-09-10   49 -> 57   +8   = 4 of its own elements
          2026-09-11    5 -> 13   +8   = 4 of its own elements
          2026-09-17   29 -> 37   +8   = 4 of its own elements

      THE HOISTED ELEMENT IS EXACTLY FOUR OF ITS OWN AHEAD, three
      times out of three — and the test deliberately uses the shipped
      `Channel(4)`. Four is the CAPACITY. That turns "a race in the
      adoption/swap path" into something falsifiable: a producer's
      element one full part ahead becomes readable before the part
      holding its three predecessors is drained. It also gives the
      experiment that was missing — run the same suite at capacity 8
      and see whether the hoist becomes +16. If it does, the bug is
      indexed by capacity and the swap is reading the new part early;
      if it stays +8, the number is a coincidence of three and the
      lead is dead.
      Load at the failure: `{ 8.6 6.6 6.3 }` — NOT a busy box this
      time, which weakens "needs contention to show" as a necessary
      condition, though all three runs were full matrices.
      Still NOT tagged and NOT retried into green by the gate. The
      lane that met it re-gated afterwards, having filed this first —
      a person deciding about an unrelated module, not the script
      hiding a defect. The gate log is okay-gate.UR07J2fMsW.
