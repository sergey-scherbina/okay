## logic-cut-releases — a resource before a `choose` is shared by its branches, released once

The entry as filed was wrong, and a probe showed it: `cut` over a scope
leaked nothing. The real defect ran the other way. A resource acquired
before a `choose` was acquired once and released once PER BRANCH
(`runChoice` over three alternatives released it three times).

Now the branches share it, as every effect of a search's prefix is shared,
and it is released once, when the last branch that can use it is done.
Each acquisition counts its holders (`Held`). A `Choose` passing out of a
scope that holds something carries its branch point (`Forked`, still a
`Seq`, so still a `Choose` to every handler). Every branch that starts
takes the hold again. The branch point lets go when every alternative has
started, or when the rest is abandoned:
- `Logic.cut` and `observe` abandon what they drop (`Logic.abandon`; a
  split's rest carries its branch points);
- a throw that leaves `runChoice` or `msplit` abandons them too
  (`HandleFrames.unwinding`, a finalizing frame on a machine);
- a handler of one's own says `Choose.abandon(c)`.

Cost: 0.99x for `runChoice` and 1.02x for `observe` with no scope; 1.11x
for 100 branches over one shared resource (LogicBenchmark,
ChoiceBenchmark, history.d).

Spec: specs/backtracking.md, "A resource shared by branches". Docs:
docs/effects/resource.md, "Shared by branches". Tests: TestSharedResource.
