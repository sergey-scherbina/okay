## freer-base-remeasure — the invariant indexes and the diagonal leaf re-read on a quiet box: nothing moved, bytes identical

freer-consumed-index and freer-diag-leaf landed unmeasured while the
CI runner gated beside them. A/B afterwards, 78f8dfec2 against
7ae9a6aa9 (the base before both; the arms differ in four core files),
three alternating rounds, MIN per lane, `jmh-lane.sh -f2 -wi3 -i5
-prof gc`, every lane quiet: fib100 1.004, statePara 1.010,
relayForward 1.001, stepBulk 0.996, allocation identical to the byte
on all four. Rows in src/jmh/history.d, the table in
specs/freer-base.md "Re-measured". On the way: the runner's family
gate was found hung 65+ min on a spinning okay-stream test fork
(recorded as okay-stream/BUGS.md `windowjoin-trim-spins`, the fork
killed on the operator's word), and a JMH result table prints the
class without its package, which a parser keyed on the full name
misses.
