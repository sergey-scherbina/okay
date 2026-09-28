## check-citations-nine-hex — the citation check could not see a 9-hex sha

Found landing foreign-value-rename (2026-09-28): `land.sh` rebased the
lane, the changelog's `Commits:` line kept the three PRE-rebase shas, and
`scripts/check-citations.sh` said "all reachable from HEAD". Its regex was
`\b[0-9a-f]{8}\b` — exactly eight — while `git log --format=%h` in this
repository now abbreviates to NINE, so every 9-hex citation written since
the abbreviation grew was invisible to the check that exists to catch
exactly this (AGENTS.md: "a check that requires remembering what to check
is not a check" — this one required the sha to be eight long).

Now `{8,40}`. Run over the whole tree the widened check found one more
non-ancestor, and it was legitimate: `backlog.d/okay-cluster-dataflow/
cluster-pool-numbers.md` cites the tip of the PARKED branch
`feature/cluster-pool-numbers` ("preserved UNGATED on ..."), which is not
a landing and is not meant to be on master. So a sha that is the tip of
an existing local branch is skipped: a parked lane's reference stays
valid until the branch is deleted, at which point the citation is dead
and the check says so. The three shas in changelog.d/foreign-value-rename.md
are corrected to the landed ones.

Commits: (this lane's one commit; the ff-merge adds nothing).
