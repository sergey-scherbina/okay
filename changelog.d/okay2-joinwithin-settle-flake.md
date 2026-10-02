## okay2-joinwithin-settle-flake - an endless side's stop is waited for, not slept on

okay2's TestSourceJoinWithin turned two full okay2 gates red in a row on
a loaded box. Run alone it was green 10 times out of 10 on the branch
and on master. The test asserted "the endless side stops" by reading a
production counter twice, 50 ms apart. When the join returns, the
feeder may still be filling its buffer, and under load that catch-up
outlasted the sleep.

**The fix.** The suite now asserts the property itself: production
stands still (unchanged for 50 ms, waited for up to 10 s) and stays
bounded.
- The bound is what the join read, plus the buffer, plus the element in
  hand: 15 to 17 measured, against a bound of 32.
- A mutant with a buffer of 100 000 turns the test red, at 131 138
  elements.

**Other suites with the pattern:**
- okay2's TestSourceJoin had the same race, and now uses the same
  helper, `Settle.await` (okay2-stream tests, JVM only).
- The core's TestSourceZip keeps its 50 ms check: it waits for the
  feeder thread to end first, so no producer is left to race.
