## snippet-debt-paid - the docs ratchet entry foreign-facade-3 pinned and did not delete

`FacadeConformance` (foreign-facade-3) imports `okay.cluster.{Flow, Flows}`,
which pinned that line of docs/modules/okay-cluster.md; the debt entry
had to go with it and did not, so the whole build on master was RED on
`TestDocSnippets`'s ratchet while 167 commits waited to be pushed.
Deleted by `OKAY_SNIPPET_DEBT=write`, as the rule says.
