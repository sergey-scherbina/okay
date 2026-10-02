## scheduler-flip-remeasure — the default's table re-measured on today's code

The adaptive default's speed claim rested on a table taken before the
drive's slice hooks and late-resume `Bind` landed. Re-run on master of
2026-09-30, one lane per `jmh-lane.sh`, two rounds each with the
arms alternating by `-Dokay.scheduler`: fork/join 10k outside 0.68
(was 0.65), inside 0.97 (0.98), cancel 1k 0.70 (0.68), spawn 100
outside 0.55 (0.97), inside 0.54 (0.51), parallel8 0.69 (0.67). Every
row holds and the one that moved moved for the default. The lanes the
table never had were closed by their own lanes the same two days
(adaptive-chunked-merge-cost, spawnjoin-rise-bisect,
channel-route-per-producer). specs/schedulers.md, "The flip".
