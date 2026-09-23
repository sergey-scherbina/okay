## loop-audit - the `!.loop` doors counted: three more rewritten, the rest recorded with reasons

Stage 2 of specs/fold-until.md rewrote four of the twelve hand-written
`def loop(state)` programs and rejected two on reading; the other six
were never looked at. Every production `def loop(` outside the
interpreter walks is now read and decided (spec Decisions). REWRITTEN
over `!.loop`: `Form.asking`, the policy road of `Form.askWith`, and
`Form.askSchema` — each a `def loop` with three or four recursive
calls and a trailing seed, each now a `map` whose cases answer a value
(`Right(Some(a))`, `Left((draft, errors))`); the `pure` import went
with them; six okay-ui suites unchanged, 33 green. KEPT with the reason
written down: `Ui.run` and chatweb's loop pull a source (the
`Dialog.run` reason — `uncons`'s `Either` under the loop's reads
backwards; they are `Take.foldUntil`-shaped consumers), `Conversation`
is mutually recursive with `fill`, `Nio.listen` has no state and no
exit (a `Right` would be unreachable). Not `!`-programs: `PWizard`'s
`Machine` loop and `Wire.serveClosing`'s `Stage` loop — the latter is
a `Stage.transduceUntil` door, filed as `wire-serve-transduce-until`
(backlog okay-ui). Landed as a4ce2859.
