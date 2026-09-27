## relay-forward-same-inject — a forwarded operation is forwarded, not rebuilt

A handler loop that meets an operation of another effect forwards it to
the rest of the row, and every such arm built a fresh `Inject(e)` to do
it although the node it had just matched IS that `Inject`. Now one named
door, `forwarded[F, G](i)` beside `split` (Handler.scala), carries the
one type claim — the excluded middle `split` already makes — and every
arm that forwards to the REST of the row uses it: `relay`, `handle`,
`translate`, Writer's `loopWith`/`foldUntil`/`uncons`/`widen`, State,
Supply, Chronicle, Refs, Once, Logic, Generate. The compiler refused it
at the six arms that forward into a DIFFERENT row (Lexical, `State.zoomWith`,
Writer's `map`/`expand`/`listen`) — a second claim, not written. Measured:
`ProbeRowCost`'s row 168.4 -> 152.4 B per level (exactly the 16 B);
`handlePrebuilt` 0.96x time, -9% bytes; `relayForward` 0.96-0.98x, -7%;
the four-effect `okayRow` 0.92-0.93x, -10%. `TestInlineBudget` green.
specs/effect-row-cost.md D2; history `…-relay-forward-same-inject.tsv`.
