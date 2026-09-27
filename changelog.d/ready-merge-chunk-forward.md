## ready-merge-chunk-forward — stage 1 (a notifying ring-side receive) REFUTED; the chunked roads stay on the shared channel

The operator unpaused the chunk-on-ring work as one lane: first
`ring-standing-receiver`, then — only if it removed the slow regime —
the chunked merge roads onto `ReadyMerge`. Stage 1 was built as a
notifying receive in `SentinelChannel` (`receiveManyOrWatch`: an armed
waiter only says "look again", the elements stay in the ring, the
reader takes everything on its own thread; seven laws for order, end,
failure and cancel, green with the existing channel and merge suites)
and `drained` read through it. Measured on the rebuilt chunked ring
road (f0f355bd4's), `okayChunked`, five arms of 10 forks alternating
through `jmh-lane.sh`: slow forks 1/10 and 5/10 against the one-shot
receive's 2/10 and 5/10, the shared channel 0/10, and the notifying
arm's slow forks made MORE side wake-ups (137-149 per op against
91-106). A caught-up consumer finds one chunk per look whichever thread
pops it, so no receive-side design can batch it; that is the second
receive-side fix refuted. Stage 2 was not run, the code was dropped,
nothing in the library changed. `ring-standing-receiver` is filed as
refuted, `ready-merge-chunk-forward` is back in the backlog with no
candidate fix. Rows in
`src/jmh/history.d/…-ring-standing-receiver-notify.tsv`;
specs/source-merge-via-ready.md (the stage and its Result).
