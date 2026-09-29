## check-citations-shallow - a shallow clone says what it cannot judge instead of failing

A cloud session's clone (depth-limited, as CI's and the cloud's are) read
`12120c2a` in changelog.d/producer-writer-carrier-stage0.md as dangling. It
is not: the commit is from 2026-08-31, inside v0.1.1's history, on master.
The clone's history simply stops at a graft, and 283 cited commits lie
beyond it. In a shallow clone `check-citations.sh` now counts those, says
the verdict belongs to a full clone, and fails only on what it can see.
A full clone behaves exactly as before.
