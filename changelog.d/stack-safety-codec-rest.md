## stack-safety-codec-rest - a JSON key is bounded, not walked; stage 2 of the stack-safety audit closes

The remaining 38 okay-codec rows, one hole. `JsonOptic.path` and
`Policy.hide` descend the schema once per segment of a key a caller
hands over, and through a sum at every level that descent is a search
over the cases rather than a loop — so on a recursive enum a
100 000-segment key was a StackOverflowError (`TestJsonOpticDepth`, red
first on a 256 KB stack). A key past `JsonOptic.MaxSegments` (64) names
nothing in `path` and is refused by name at `hide`'s construction.

The other 37 rows carry their bounds: `Staged`'s fifteen walks run at
compile time over the user's type with a `seen` cut; `Stubs` declares a
product once and budgets its key table; `TsTypes` reads a declaration
file a person wrote; `Compat` and `Digest` carry their own `seen` guards;
`Schema.fold`'s edge is a lazy val answered by identity. With 2a–2d
landed, stage 2 (codecs) of specs/stack-safety.md is closed.
