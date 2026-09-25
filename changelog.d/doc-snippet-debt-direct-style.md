## doc-snippet-debt-direct-style

`docs/direct-style.md`'s share of the doc-snippet-debt ratchet: 118
unpinned example lines down to 0. Every `scala` example on the page
now matches a tested or library source line verbatim.

Along the way, several genuine drifts were found and fixed rather than
just re-typed to match: the `Monadic` object example still showed a
retired `.?` mark instead of the real `!?`; `Once`'s enum/def
signatures were paraphrased with parens and comments the real source
doesn't have; `Gen`'s class definition predated its move to a
`chain`-based `AnyVal`; and the whole `Fetch`/`page`/`Test` walkthrough
still said `Fetch.now`/`object Test`/`Db` after the shipped fixture
(`TestDirectOnce`) renamed to `Fetch.time`/`Runner`+`test`/
`Reader % (Users, Feeds)`. Two `Condition.run` calls did not actually
compile unwrapped and needed `!.run(...)`.

New `TestDocExamples*`-style coverage landed across `TestMonadic`,
`TestDirect`, `TestDirectAuto`, `TestDirectDeep`, `TestDirectDoors`,
`TestDirectLoops2`, `TestDirectOnce`, `TestDirectSource`,
`TestDocExamplesGen`, `TestStaged`, `TestStagers`, `TestSharedOnce`.

Landed as f9c424ad3.
