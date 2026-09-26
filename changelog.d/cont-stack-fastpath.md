## cont-stack-fastpath: fib100 back to the base

Round 3 of plan stage C (specs/cont-stack.md). `PrintInlining` on
fib100, base against master, showed the `Mapped` leaf's lambda
reaching its continuation through `callK` — a type test, then
`Reentry.enter` — which some compiles refused ("callee uses too much
stack"), so the lambda the base scalar-replaced escaped. `mappedK` now
decides that type test ONCE, when the leaf is applied: a plain `k`
gets the base's own `a => k(g(a))`, a `Reentry` a lambda that enters
it. fib100 reads 2281 against master's 2501 ns (0.91x) and 1.04x the
pre-cont-stack base, at the base's exact 21 552 B/op on every round —
the +1 600 B/op that stage C chased is gone, deterministically.

Refuted in the lane and recorded: splitting `step` into a small loop
plus a walking loop for Layer 1 B bodies (fib100 1.02x master,
statePara 0.98x — within noise). statePara is unchanged: 1.11x the
base with no switch possible and +21 KB/op of REAL allocation (an
exact count says so), filed back as cont-stack-fastpath with the next
instrument named. Commits: the spec note 532f7a16d, the change
2941e3ac3, and the landing commit that carries this entry.
