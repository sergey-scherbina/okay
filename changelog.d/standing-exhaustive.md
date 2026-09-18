## standing-exhaustive - master was red for everyone, by one missing case

`static-workflow-par` (52eb9c69) gave `Wf.Proc.Standing` a third case —
`Waiting`, a `Par` whose branches are both outstanding — and
`TestProcDirect`'s walk-versus-replay match still covered two. E029,
exhaustivity, which this repository counts as RED under "no warnings,
ever". Every lane gating after it inherited the red, which is how it
was found: a lane touching only okay-ui came back red in the core.

The booking in that test has no `Par` and can never stand on two
questions, so the new case FAILS with what it saw rather than
pretending to handle it — a match arm that cannot happen should say so
loudly if it ever does.

Why it slipped: the lane that added the case gated green, and its own
gate did not recompile that file. A WARM gate says nothing about
warnings — `gate.sh` prints exactly that when nothing was compiled, and
the lesson is the same one the script already states: a lane gates in a
fresh worktree, and a green warm run is not evidence about warnings.
