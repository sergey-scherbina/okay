- [ ] scalajs-arraydeque-null — REPRODUCED AND EXPLAINED, report NOT
      SENT (operator, 2026-09-25: reproduce, do not send yet).
      `java.util.ArrayDeque` on Scala.js 1.22 (same on `main`) loses the
      front of the queue when it grows while its ring buffer wraps:
      `ensureCapacityForAdd` allocates a NEW array for that branch and
      copies only `[0, endIndex)`. A stack's 33rd push pops back as 33,
      then 16 nulls, then 16..1. `okay-sql`'s ProbeScalaJsArrayDeque pins
      it on all three platforms. The fix (`Arrays.copyOf` instead of
      `new Array`) was verified on a transcription: 192 wrong pops as
      shipped, which is exactly our original sighting, and 0 fixed. No
      existing issue covers it. THE REMAINING STEP: send
      `upstream/scalajs-arraydeque-null.md` when the operator says so,
      then cite the issue in the probe's comment. (2026-09-25)
