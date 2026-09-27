## jmh-lane-term-exits — one SIGTERM ends a lane, queued or running

`scripts/jmh-lane.sh` trapped INT and TERM with handlers that only
cleaned up, and under `sh` a trap that does not exit lets the script
carry on: a lane queued behind the lock survived SIGTERM and took a
`kill -KILL` (found by ring-head-tail-padding). Now EXIT does the
cleanup (the bench-window request, the lock once held), INT/TERM exit
130/143 after killing the tree of the attempt in flight, and the
attempt is waited on in the background so the signal is handled at once
rather than when sbt finishes. `jmh-lane-selftest.sh` cases 12 and 13
(queued lane; lane holding the lock with a run in flight: exits, releases
the lock, its run is gone) were watched FAIL on master first; the whole
selftest PASSES under sh and bash.
