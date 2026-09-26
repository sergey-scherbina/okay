## foreign-mux-part1 — several requests at once on one Go worker (2026-09-26)

Part 1 of foreign-mux-duplex (specs/foreign-one.md Decision 24). A far side
that claims `"mux": true` over pipes or a socket gets requests without the
host waiting for each answer; answers are matched by `id`. On the host,
whoever waits reads the link and completes the others' answers on the way
(leader/follower) — 1.01x a sequential exchange on 2 000 calls, where a
dedicated reader thread cost up to 1.42x; a session with a deadline keeps
one reader thread. Go claims it: a goroutine per request, one worker lock
released while a call waits. WireConformance's MUX case (a call answerable
only after a second one runs on the same worker) passes on every Go row;
every other far side is served as before. docs/one-language.md, "Several
requests at once on one worker". Left: Rust, credit streams, the journal.
