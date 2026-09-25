## stack-safety-workflow - the workflow walks carry their bound: a Proc term is the program's own composition

Stack-safety stage 5, both cores. Every walk of a `Proc` term —
`foldMap`, the drawer `go`, `nodes`, and `Wf`'s journal walker
`go`/`loop`, okay2's `nodes` — descends once per node of the term, and
the term is what the program composed: its own text, or a fold over
steps it chose. That bound is written on the rows. The `Iter` round
recurses through `G`'s own flatMap, a value at every program row.

Decision 7 (specs/stack-safety.md) says why this is a bound and not a
fix, unlike the Query predicate: the arrow's GADT hides an existential
type at every `Then`, so an explicit stack over it is a redesign of the
typed fold, and no consumer composes thousands of steps dynamically
today. The trigger and the first measurement are in backlog
`okay-core/proc-deep-composition`. ProcMacro's rows are stage 7.
