## ci-runner-okay2-gate - the okay2 half of a whole build reached no verdict because gate-retry looked for gate.sh under okay2/

`gate-retry.sh` ran `bash scripts/gate.sh` relative to the worktree it
was handed, and the runner hands `okay2/` as that worktree for the okay2
build (`cd okay2 && ../scripts/gate-retry.sh "$PWD" …`), where
`scripts/gate.sh` does not exist: rc=127 six times in a minute, read as
"the box took it", and every whole build whose range touched okay2/ —
every one since the okay2 lanes of 2026-09-25 — ended in "no verdict"
and no push (origin 222 commits behind at midnight). gate-retry now runs
the gate.sh BESIDE itself, from the worktree's directory, which is what
`cd okay2 && ../scripts/gate.sh` has always been. Proven from okay2/:
one attempt, GREEN, exit 0.
