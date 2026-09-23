## foreign-journalled - Durable journals Python and R calls

Stage 1 of specs/foreign-highlevel.md. `Journalled` moves from
okay-agent to okay-codec (`okay.codec.Journalled`). okay-agent keeps the
name as a type alias, and the `Tool` instance moves into `Tool`'s
companion. okay-py and okay-r, which do not depend on okay-agent, now
carry their own instances in `PyEval`'s and `REval`'s companions, so
`Durable.over[PyEval]` journals a Python call and
`Durable.replayingOver[PyEval]` answers it without starting Python. The
same holds for R.

The fingerprint is the function plus a SHA-256 of what it was asked, so
drifted inputs are refused. The answer is kept in the module's own wire
JSON, and None, NaN, typed NA and NULL stay distinct. This makes true
the promise that specs/py.md and specs/r.md made and then withdrew on
2026-09-07.

Tests: TestPyJournal and TestRJournal, eight tests that need no live
interpreter. A mutant that drops the argument hash is caught. Docs: new
"Journalled by Durable" sections in docs/modules/okay-py.md and
docs/modules/okay-r.md.
