## foreign-facade-spec - the facade over every foreign language, specified: one typeclass per capability, three tiers of data

Stage 0 of the operator's foreign-facade (2026-09-25): specs/foreign-facade.md.
The shape generalises foreign-engine-typeclass — `Engine[-M]`/`Reduces[-M]`
become one family, `Calls`/`Frames`/`Streams`/`Programs`/`Holds`, a
typeclass per CAPABILITY by the module's type, each instance optional,
so a capability a language lacks is a compile error and a new language
is a module type plus the instances it can honestly give. `Speaks[M]`
reads the hello for what a worker DOES, and the conformance suite checks
the claim both ways. The data model is three tiers chosen by the data —
a value as one JSON line, a Table as Arrow IPC (columnar JSON where Arrow
is not spoken), a Source as frames back-pressured through the wire's
`continue` — with MUST/SHOULD/MAY per language and degradation as a rule
`Speaks` reports. `Schema` and `Table` are the vocabulary; `PyValue`/
`RValue` are codecs. The zero-cost `JvmModule` tier (a Table by
reference) is the test that the model does not limit. The measurement
table (language × tier × transport) carries the numbers already known
and the empty cells that are the work. Six stages follow. Also: the
stack-safety spec's stage-4 header is ticked (4a and 4b had closed it).
